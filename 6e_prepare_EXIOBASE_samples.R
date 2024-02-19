#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2023-07-11 15:04:37.369072
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(tidyverse)
library(units)
library(ggforce)
library(logr)
# library(mRio)
library(countrycode)
library(arrow)
############################################################################## # 
##### settings #################################################################
############################################################################## # 
source(file.path('src', 'functions.R'))
source(file.path('src', 'functions_mRio.R'))

# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())



############################################################################## # 
##### functions #################################################################
############################################################################## # 


############################################################################## # 
##### load data #############################################################
############################################################################## # 
# files <- list.files(path = path2output, 'sample_EXIOBASE', full.names = TRUE)
# 
# dt <- lapply(files, read_feather)
# dt <- rbindlist(dt)
# 
# object.size(dt) %>% format(units= 'GiB')

# begin old stuff

dt <- read_feather(file.path(path2output, 'sample_EXIOBASE_detailed.feather'))
dt[, sample := as.list(sample)]
dt[, country_code := as.character(country_code)]
# map to EXIOBASE regions -----------------------------------------------------
exio_regions <- fread(file.path(config$path2exiobaseIOT, 'unit.txt')) %>% 
  .$region %>% 
  unique



dt[, country_code2 := countrycode(country_code, 'iso3c', 'iso2c')]
dt[is.na(country_code2)]$country_code %>% unique

dt[country_code == 'ANT', country_code2 := 'NL'] # Netherlands Antilles : https://laendercode.net/en/3-letter-code/ant
dt[country_code == 'SCG', country_code := 'SRB'] # Serbia and Montenegro to Serbia : https://laendercode.net/en/country/rs


dt[!(country_code2 %in% exio_regions), 
    region := countrycode(country_code, 'iso3c', 'region')]
dt[country_code == 'ESH', region := "Middle East & North Africa"]

# kick out oversea areas (should be already included in national inventories):
dt <- dt[!(country_code %in% c('MYT', 'REU', 'SHN'))]

dt[is.na(country_code2)]

dt[(country_code2 %in% exio_regions), 
    EB_region := country_code2]

dt$region %>% unique
dt[region %in% c("East Asia & Pacific", "South Asia"), EB_region := 'WA']
dt[region %in% c("Latin America & Caribbean", "North America"), EB_region := 'WL']
dt[region %in% c("Europe & Central Asia"), EB_region := 'WE']
dt[region %in% c("Sub-Saharan Africa"), EB_region := 'WF']
dt[region %in% c("Middle East & North Africa"), EB_region := 'WM']

# should be empty
dt[is.na(EB_region)]

# aggregate by EXIOBASE region =================================================
dt$classification %>% unique
dt2 <- dt[, list(sample = (sum_samples(sample))), 
    by = .(year, EB_region, gas, category_code, classification, industry_code, 
           database)]

rm(dt)
gc()

# save .tex table with database information ====================================
table <- dt2[database %in% c('edgar', 'unfccc'), .(EB_region, database)] %>% unique
table <- table[!(EB_region == 'NL' & database == 'edgar')]
table[, EB_region := factor(EB_region, levels = exio_regions)]
setorder(table, EB_region)

country_names <- rio::import(file.path('data', 'CountryMappingDESIRE.xlsx'), 
                             which = 'DESIREregions') %>% 
  as.data.table
table <- merge(table, country_names[, .(`DESIRE code`, Name)], by.x = 'EB_region', 
      by.y = 'DESIRE code', sort = FALSE)
table[, database := toupper(database)]
setnames(table, c('EB_region', 'database'), c("Country code", 'Database'))
table <- table[, .(`Country code`, Name, Database)]


path2plot <- file.path('figures', config$version)
if(!dir.exists(path2plot)) dir.create(path2plot)


save_results_xlsx(table, filename = 'EB_regions.xlsx')


kableExtra::kbl(table, escape = FALSE, format = 'latex', 
                booktabs = FALSE, 
                toprule = '\\tophline', midrule = '\\middlehline', 
                bottomrule = '\\bottomhline', linesep = '', vline = '',  
                caption = "EXIOBASE v3 countries/regions and raw data source 
                (EDGAR or UNFCCC inventories). The columns 'Country code' 
                depict ISO 3166-1 alpha-2 codes, except the five Rest of the World (RoW) regions.", 
                label = 'countries') %>% 
  write(file = file.path(path2plot, 'table_EB_regions.tex'))




# add broader category code ====================================================
dt2[grepl('^1.A.1', category_code), category_code2 := '1.A.1']
dt2[grepl('^1.A.2', category_code), category_code2 := '1.A.2']
dt2[grepl('^1.A.3', category_code), category_code2 := '1.A.3']
dt2[grepl('^1.A.4', category_code), category_code2 := '1.A.4']
dt2[grepl('^1.A.5', category_code), category_code2 := '1.A.5']
dt2[grepl('^1.B', category_code), category_code2 := '1.B']
dt2[grepl('^2', category_code), category_code2 := '2']
dt2[grepl('^3', category_code), category_code2 := '3']
dt2[grepl('^5', category_code), category_code2 := '5']
dt2[grepl("^0.A", category_code), category_code2 := '0.A']
dt2[grepl("^0.B", category_code), category_code2 := '0.B']

dt2[grepl("^4.", category_code) & database == 'edgar', 
    category_code2 := '5']



# 2. samples ===================================
#dt3 <- readRDS('./temp_results/5c_EXIOBASE_samples.RData')

# 
# dt4 <- dt3[, list(sample = (sum_samples(sample))), 
#            by = .(year, EB_region, gas, industry_code, category_code2)]




# checks =======================================================================

dt2[is.na(category_code2)] # TODO still some unclassified categories
dt2[is.na(category_code2)]$category_code %>% unique

dt2[is.na(industry_code)] # TODO still some NAs in industry codes

dt2[, n_by_cat := length(id), by = .(year, EB_region, 
                                     gas, classification, category_code)]

# calculate summary statistics =================================================

#dt2 <- calculate_summary_statistics(dt2)


# extract household emissions =================================================
dt_hh <- dt2[industry_code == 'y01']
dt2 <- dt2[industry_code != 'y01']


dt_hh <- calculate_summary_statistics(dt_hh)
save_results(dt_hh, suffix = '_households', type = '.feather')
rm(dt_hh)
# sum by industry =============================================================
dt_by_industry <- dt2[
  , list(sample = list(Reduce('+', sample)))
  , by = .(year, EB_region, gas, industry_code)
]

dt_by_industry <- calculate_summary_statistics(dt_by_industry)

dt_by_industry[is.nan(mean)]

save_results(dt_by_industry, suffix = '_by_industry', type = '.feather')
rm(dt_by_industry)

# sum by industry & IPCC category =============================================
dt_by_cat <- dt2[
  , list(sample = list(Reduce('+', sample)))
  , by = .(year, EB_region, gas, industry_code, category_code2)
]

dt_by_cat <- calculate_summary_statistics(dt_by_cat)

dt_by_cat[is.nan(mean)]
save_results(dt_by_cat, suffix = '_by_industry_and_CRF', type = '.feather')
rm(dt_by_cat)

# sum by region/country ==============================================
dt_by_region <- dt2[
  , list(sample = list(Reduce('+', sample)))
  , by = .(year, EB_region, gas)
]


dt_by_region <- calculate_summary_statistics(dt_by_region)
dt_by_region[is.nan(mean)]
save_results(dt_by_region, suffix = '_by_region', type = '.feather')
rm(dt_by_region)
gc()

# sum by region & category ==============================================
dt_by_reg_crf <- dt2[
  , list(sample = list(Reduce('+', sample)))
  , by = .(year, EB_region, gas, category_code2)
]
dt_by_reg_crf <- calculate_summary_statistics(dt_by_reg_crf)
save_results(dt_by_reg_crf, suffix = '_by_region_and_CRF', type = '.feather')
rm(dt_by_reg_crf)

############################################################################## # 
##### save results #############################################################
############################################################################## # 

save_results(dt2, suffix = '_detailed', type = '.feather')



# THE END ---------------------------------------------------------------------
