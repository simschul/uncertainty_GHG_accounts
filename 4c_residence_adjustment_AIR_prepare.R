#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-09-06 14:28:55
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
library(testthat)
library(janitor)
library(countrycode)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source(file.path('src', 'functions.R'))
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output

N <- config$sample_size
GASES <- config$gases
YEAR <- config$year
path2eurostat <- config$path2eurostat_bridging_items
path2worldbank <- config$path2worldbank_air_transport


############################################################################## # 
##### 1. Eurostat bridging items################################################
############################################################################## # 



data_bridge <- readRDS(file.path(path2eurostat, 'env_ac_aibrid_r2_GHG.RData'))
#data_bridge <- data_bridge[grepl(iparty, geo_label)]
#data_bridge <- data_bridge[time == YEAR 
#                           & airpol == igas]

data_bridge <- data_bridge[indic_nv == 'NRA_AIR']
data_bridge[, country := countrycode(geo, origin = 'eurostat', 
                                     destination = 'iso3c')]
data_bridge <- data_bridge[!is.na(country)]

data_bridge[, share_regional := values / sum(values), by = .(airpol, indic_nv, time)]

data_bridge <- data_bridge[time == YEAR, .(country, time, airpol, share_regional, values)]
setnames(data_bridge, c('time', 'airpol', 'values'), c('year', 'gas', 'eurostat_bridge'))

data_bridge[, share_regional := drop_units(share_regional)]
data_bridge[, eurostat_bridge := set_units(eurostat_bridge, Gg)]

# Test =========================================================================
data_bridge[, sum(share_regional)]
data_bridge[, sum(eurostat_bridge), by = gas]

test_that('all good',{
 expect_equal(data_bridge[, sum(share_regional)], length(GASES)) 
})

############################################################################## # 
##### 2. Worldbank carried passenger data ######################################
############################################################################## # 
datawb <- fread(file.path(path2worldbank, '/API_IS.AIR.PSGR_DS2_en_csv_v2_4756008.csv'), 
                skip = 4, header = TRUE)
setnames(datawb, names(datawb[,1:4]), make_clean_names(names(datawb[,1:4])))

datawb$V67 <- NULL
setkey(datawb, country_name, country_code, indicator_name, indicator_code)
datawb <- datawb[, c(key(datawb), YEAR), with = FALSE]
datawb[, `2015` := as.numeric(`2015`)]

datawb <- melt(datawb, id.vars = key(datawb), 
     variable.factor = FALSE, variable.name = 'year')

datawb <- na.omit(datawb)
datawb <- datawb[year == YEAR]

# Exclude regional agregates (EU, etc)
metawb <- fread(file.path(path2worldbank, 'Metadata_Country_API_IS.AIR.PSGR_DS2_en_csv_v2_4756008.csv'), 
                header = TRUE)
metawb <- clean_names(metawb)
metawb[region != '']

datawb <- datawb[country_code %in% metawb[region != '']$country_code]

# Exclude all countries already covered in Eurostat bridging items
datawb <- datawb[!(country_code %in% data_bridge$country)]

# calculate shares
datawb[, share_regional := value / sum(value)]
datawb <- datawb[, .(country_code, country_name, year, share_regional)]
setnames(datawb, 'country_code', 'country')

# copy for each gas (assumption: same shares per gas)
datawb <- lapply(GASES, function(x) datawb) %>% 
  setNames(GASES) %>% 
  rbindlist(idcol = 'gas')



# Test =========================================================================

test_that('all good',{
  expect_equal(datawb[, sum(share_regional)], length(GASES)) 
})

############################################################################## # 
##### 3. Combine both ######################################
############################################################################## # 

country_shares <- rbindlist(list(
  'EU' = data_bridge[, .(country, year, gas, share_regional)], 
  'nonEU' = datawb[, .(country, year, gas, share_regional)]
), idcol = 'region', use.names = TRUE, fill = TRUE)

# Test ===========
test_that('test 1', {
  expect_true(zero_range(country_shares[, sum(share_regional), by = .(region, gas)]$V1))  
})
# End test =





# save results =================================================================
save_results(data_bridge, suffix = '_eurostat', type = '.feather')
save_results(datawb, suffix = '_worldbank', type = '.feather')
save_results(country_shares, type = '.feather')




# THE END ---------------------------------------------------------------------
