#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-01-21 10:47:07
#' 
#' Content:
#'
#'  TODO: 
#'  - only one child reports no emissions but uncertainty data (e.g. SVN CH4), 
#'    atm: disaggregate_emissions takes emission value from parent node (sum(childs) == 2*parent!!!)  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(tidyverse)
library(units)
library(ggforce)
# library(my.utils)
library(data.tree)
library(collapsibleTree)
library(countrycode)
library(truncnorm)
library(logr)

############################################################################## # 
##### general settings #########################################################
############################################################################## # 

options("datatable.print.class" = TRUE)

# source utils
source(file.path('src', 'functions.R'))
# read config and setup log script
config <- setup_config_and_log()

path2output <- config$path2output

############################################################################## # 
##### settings #################################################################
############################################################################## # 

path2ut <- config$path2UNFCCC_uncertainty

path2tier1 <- file.path(path2ut, 'Tier1_cleaned_1b.RData')
path2tier2 <- file.path(path2ut, 'Tier2_cleaned_1b.RData')

gases <- config$gases

CT_classes <- readRDS(file.path(path2output, 'create_CT_CRF_classification_table.RData'))

############################################################################## # 
##### functions #############################################################
############################################################################## # 
source(file.path('src', 'functions_dirichlet.R'))
source(file.path('src', 'functions_trees.R'))

############################################################################## # 
##### load data #############################################################
############################################################################## # 
tier1 <- readRDS(path2tier1)



############################################################################## # 
##### prepare data #############################################################
############################################################################## # 

# select gases
tier1 <- tier1[B %in% gases]

# when parties have 2 versions: take one without lulucf
parties_duplicated <- tier1[, .(party, LULUCF)] %>% 
  unique %>% 
  .[duplicated(party),] %>% 
  .$party
tier1 <- tier1[!(party %in% parties_duplicated
                 & LULUCF == 'incl')]

tier1[, .(party, LULUCF)] %>% 
  unique

# kick out sectors 4
tier1 <- tier1[!grepl('^4.', A)]

# kick out sectors with zero/NA emissions
tier1 <- tier1[!(is.na(D) & is.na(G))]


# retrieve necessary columns
data <- copy(tier1[, .(year, party, A, classification, B, D, G)])
setnames(data, c('D', 'G'), c('emissions', 'cv'))

# convert CV from % to decimal
data[, cv := cv / 100]

data[, sd := emissions * cv]

# convert units from GWP100 to original unit
data[B == 'CH4', `:=`(sd = sd / 25, 
                      emissions = emissions / 25)]
data[B == 'N2O', `:=`(sd = sd / 198, 
                      emissions = emissions / 198)]

data[, year := as.character(year)]
data[, emissions := set_units(emissions, Gg)]
data[, sd := set_units(sd, Gg)]
data[, cv := sd / emissions]

# kick out zero emissions
data <- data[emissions != set_units(0, g)]

# create node id
data[, id := paste0('I.', A)]

# create country code column
data[, party := countryname(party, 'iso3c', warn = TRUE)]

# # unify fuels
# data[, fuel := tolower(fuel) %>% gsub(' ', '_', .)]

# change column names to avoid problems with data.tree::as.Node (see : https://github.com/gluc/data.tree/issues/160)
setnames(data, 
         c('A', 'B','sd', 'emissions', 'cv'), 
         c('category_code', 'gas', 'sd_NIR', 'emissions_NIR', 'cv_NIR'))

# set keys
setkey(data, year, party, category_code, classification, gas)


# Totals for category 
# TODO: revise this! doesnt work eg for AUS CH4 1.A (which is only biomass)
# data$classification %>% unique
# new_rows <- data[classification != 'total_for_category', 
#      list(sd_NIR = sqrt(sum(sd_NIR^2)), 
#           emissions_NIR = sum(emissions_NIR),
#           classification = 'total_for_category'), 
#      by = .(year, party, category_code, gas, id)]
# new_rows[, cv_NIR := sd_NIR / emissions_NIR]
# data <- rbindlist(list(data, new_rows), use.names = TRUE)



# quick and dirty fixes ========================================================
# TODO

# Germany
data[party == 'DEU' & grepl('^5', category_code) & gas == 'CO2']
names(data)
new_row <- data.table(
  year = '2015', 
  party = 'DEU', 
  category_code = '5.C', 
  classification = 'total_for_category', 
  gas = 'CO2', 
  emissions_NIR = set_units(511.9565, Gg), 
  cv_NIR = data[grepl('^5.C', category_code) & gas == 'CO2']$cv_NIR %>% 
    mean(na.rm = TRUE),
  id = 'I.5.C'
)
new_row[, sd_NIR := emissions_NIR * cv_NIR]
data <- rbindlist(list(data, new_row), use.names = TRUE)

# ITAly
data[party == 'ITA' & gas == 'CH4' & category_code == '1.A.3.c']
names(data)
new_row <- data.table(
  year = '2015', 
  party = 'ITA', 
  category_code = '1.A.3.c', 
  classification = 'total_for_category', 
  gas = 'CH4', 
  emissions_NIR = set_units(0.00396, Gg), 
  cv_NIR = data[grepl('^1.A.3.c', category_code) & gas == 'CH4']$cv_NIR %>% 
    mean(na.rm = TRUE),
  id = 'I.1.A.3.c'
)
new_row[, sd_NIR := emissions_NIR * cv_NIR]
data <- rbindlist(list(data, new_row), use.names = TRUE)


# Estonia
ids_to_replace <- copy(data[party == 'EST' & grepl('cattle', classification), which = TRUE])
(new_rows <- data[ids_to_replace, list(emissions_NIR = sum(emissions_NIR), 
                             sd_NIR = error_propagation_analytical(sd_NIR), 
                             classification = 'cattle'), 
                      by = .(year, party, category_code, id, gas)])
new_rows[, cv_NIR := sd_NIR / emissions_NIR]
setcolorder(new_rows, names(data))

data <- replace_DT_rows(data, rows = ids_to_replace, new_row = new_rows)


# Australia: 2.C.1 iron and steel --> 2.C.7 other (thats where the CRF reports them)
data[party == 'AUS' & category_code == '2.C.1' & gas %in% c('CH4', 'N2O'), 
     category_code := '2.C.7']


# kick out 3.B.2 (Indirect N2O Emissions from Manure Management)
# TODO: where do these emissions belong? maybe 3.D.2 indirect n2o from managed soils?
# edit 2022-10-20: Emissions are part of 3.B but reported separately in CRF (mail Guetschow)
# TODO: include these emissions!!
#data[(gas == 'N2O' & category_code == '3.B.2')]

data <- data[!(gas == 'N2O' & category_code == '3.B.2')]


# create node id
data[, id := paste0('I.', category_code)]

# merge with UNFCCC emissions data =============================================
emissions <- readRDS(file.path(path2output, 'prepare_UNFCCC_CRF.RData'))
emissions[, classification := tolower(classification) %>% gsub(' ', '_', .)]
setnames(emissions, c('party'), c('party'))
setkey(emissions, year, party, category_code, classification, gas)

datafull <- merge(emissions, data, all = TRUE,
                  allow.cartesian = TRUE,
                  suffixes = c('_CRF', '_UT'))

nomatch_x <- datafull[is.na(value)]
nomatch_y <- datafull[is.na(cv_NIR)]


# exclude totals (non-match might be due to higher category detail) and zusammengestzte classifications
temp <- nomatch_x[classification != 'total_for_category'
                  & !grepl('\\&', classification)
                  & !grepl('\\&', category_code)
                  , .(year, party, category_code, classification, gas)]
data[
  party %in% temp$party
  & category_code %in% temp$category_code
  & gas %in% temp$gas]

# merge(temp, emissions, by = c('year', 'party', 'category_code', 'gas'),
#       all.x = TRUE, suffixes = c('UT', 'CRF')) %>%
#   view_excel()


# "other" classes in 3.A and 3.B ==============================================================
temp <- nomatch_x[classification == 'other']
for (i in 1:nrow(temp)) {
  irow <- temp[i,]
  CRFclasses <- emissions[year == irow$year 
                          & party == irow$party
                          & category_code == irow$category_code
                          & gas == irow$gas
                          & classification != 'total_for_category'
  ]$classification
  
  NIRclasses <- data[
    year == irow$year 
    & party == irow$party
    & category_code == irow$category_code
    & gas == irow$gas
    & !(classification %in% c('other', 'total_for_category'))
  ]$classification
  
  other_classes <- CRFclasses[!(CRFclasses %in% NIRclasses)]
  # ...cattle --> cattle
  if (sum(grepl('cattle', other_classes)) > 0 
      & sum(grepl('cattle', CRFclasses)) > 1) {
    other_classes <- other_classes[!grepl('cattle', other_classes)]
  }
  
  # fix classification (combine)
  data[
    year == irow$year 
    & party == irow$party
    & category_code == irow$category_code
    & gas == irow$gas
    & classification == 'other'
    , classification := paste(other_classes, collapse = '\\&')]
  cat(i, '')
}


# calculate total for 'liquid_fuels' (mostly/only cat 1.A.3)=============
# commented out 16.01.23 (caused troubles in script 2b)
#data <- readRDS('./temp_results/NIR_unertainty_table_cleaned.RData')
# data_add <- data[classification %in% CT_classes[main_class == 'liquid_fuels']$CRF_class
#                  & classification != 'liquid_fuels'
#                  , list(classification = 'liquid_fuels',
#                         emissions_NIR = sum(emissions_NIR), 
#                         sd_NIR = sqrt(sum(sd_NIR^2)), 
#                         cv_NIR = NA)
#                  , by = .(year, party, category_code, gas, id)] %>% 
#   .[, cv_NIR := sd_NIR / emissions_NIR] 
# data <- rbindlist(list(data, data_add), use.names = TRUE)  

setorder(data)

# select leaves only =====================================================
data[, isleaf := is_leaf_pathString(id), by = .(year, party, classification, gas)]
data[isleaf == FALSE]

data <- data[isleaf == TRUE]

#data[party == 'CAN' & grepl('^1.B.2.c', category_code) & gas == 'CO2']

# Remove units ================================================================
# TODO: data.tree does not handle units very well (make use of sapply a lot --> units get lost)
data[, emissions_NIR := drop_units(emissions_NIR)]
data[, sd_NIR := drop_units(sd_NIR)]
data[, cv_NIR := drop_units(cv_NIR)]


# save results ==============================
data$classification %>% unique

save_results(data)
#saveRDS(data, file.path(path2output, paste0(filename, '.RData')))
log_close()
# End 

