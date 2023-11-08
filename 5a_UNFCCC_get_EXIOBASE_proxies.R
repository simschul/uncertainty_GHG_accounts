#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-07-14 15:51:48
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
#library(my.utils)
library(countrycode)
library(mRio)
library(testthat)
library(arrow)
############################################################################## # 
##### settings #################################################################
############################################################################## # 

source('./src/functions.R')

# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())



path2CountryMappingDESIRE <- config$path2CountryMappingDESIRE
path2CT <- file.path(path2output, 'prepare_CT_UNFCCC.RData')
path2CT <- config$path2CT_CRF_EXIOBASE_parsed

############################################################################## # 
##### load data #############################################################
############################################################################## # 


unfccc_samples <- readRDS(file.path(path2output, 'sample_UNFCCC.RData'))
ct <- readRDS(path2CT)
ct[sapply(EXIOBASE_code, length) == 0]
############################################################################## # 
##### 1. Merge UNFCCC samples with EXIOBASE Correspondence table ###############
############################################################################## # 

# _a) data preparation =========================================================

# unfccc_samples[, category_code := pathString %>% 
#                  gsub('I\\/', '', .) %>% 
#                  gsub('\\/', '\\.', .)]

ct[, CRF_class := normalize_UNFCCC_classfications(CRF_class)]
ct <- ct[, .(id, CRF_code, CRF_class, EXIOBASE_code, proxy_data_source, EXIOBASE_products)]

setkey(unfccc_samples, year, party, gas, category_code, classification)
setkey(ct, CRF_code, CRF_class)

ct$CRF_class %>% unique
unfccc_samples$classification %>% unique



# _b) merge ===================================================================

dt <- merge(unfccc_samples, ct, 
            by.x = c('category_code', 'classification'), 
            by.y = c('CRF_code', 'CRF_class'), 
            all.x = TRUE)
dt[, n_corres := list(sapply(EXIOBASE_code, length))]
dt[EXIOBASE_code == 'all', n_corres := 163]
#dt <- dt[proxy_data_source %in% c('USE', 'SUPPLY') | n_corres == 1]


# _c) checks ==================================================================

# __i. proxy data sources

# flatten list
dt$proxy_data_source %>% unique

dt[sapply(proxy_data_source, is.null), 
   proxy_data_source := NA]

unlist(dt$proxy_data_source) %>% length
length(dt$proxy_data_source)


if (all(sapply(dt$proxy_data_source, function(x) length(x) <= 1))) {
  dt$proxy_data_source <- unlist(dt$proxy_data_source, recursive = FALSE)
} else {
  warning('column proxy_data_source could not be converted to character (i.e. unlisted) because at least one element has length > 1')
}
dt$proxy_data_source %>% unique



# correction 1: eurostat --> SUPPLY (only very minor emissions sources)
dt[proxy_data_source == 'eurostat', 
   `:=`(proxy_data_source = 'SUPPLY', 
        EXIOBASE_products = 'all')]

# correction 2: IEA: TODO insert IEA data on power-to-heat ratios
dt[proxy_data_source == 'IEA']$category_code %>% unique

# check if all information is available
dt[is.na(proxy_data_source) & n_corres > 1] # should be empty

# check for 'all' in EXIOBASE correspondences
dt[EXIOBASE_code == 'all']

# check coverage by each proxy data source:
dt$proxy_data_source %>% unique
dt[n_corres > 1 | proxy_data_source == 'PEFA', 
   sum(emissions_CRF), 
   by = .(gas, proxy_data_source)] %>% 
  ggplot(aes(x = gas, y = V1, fill = proxy_data_source)) + 
  geom_col(position = 'stack') + 
  facet_wrap(~gas, scales = 'free')

# check coverage ====
unfccc_samples3 <- merge(unfccc_samples[, list(total_emissions = sum(emissions_CRF)), 
                                        by = gas], 
                         dt[, list(covered_emissions=  sum(emissions_CRF)), by = gas], 
                         by = 'gas')
unfccc_samples3[, percent_covered := covered_emissions / total_emissions]
unfccc_samples3

# _d) more preparations ======================================================

dt[, key := paste(year, party, 
                  paste(EXIOBASE_code, sep = ''), 
                  paste(EXIOBASE_products, sep = ''),
                  paste(proxy_data_source, sep = ''),
                  sep = '')]

# list all proxy data sources
dt$proxy_data_source %>% unique
dt$proxy_data_source %>% sapply(length) %>% unique
dt[sapply(proxy_data_source, length) == 0] 
#dt[, proxy_data_source := sapply(dt$proxy_data_source, '[', 1)]

dt[is.na(proxy_data_source) & n_corres == 1
   , proxy_data_source := '1to1']
dt[proxy_data_source == 'output']
dt[proxy_data_source == 'eurostat']
dt[EXIOBASE_code == 'all']

dt[proxy_data_source == ('USE')]

dt[is.na(proxy_data_source) & n_corres != 1]

# there are still 1to1 correspondences which are declared as 'SUPPLY' --> ajust proxy data source
dt[n_corres == 1 & !(proxy_data_source %in% c('1to1', 'PEFA')) 
   , proxy_data_source := '1to1']

# _e) attach EXIOBASE region to each country ==================================
country_mapping <- rio::import(path2CountryMappingDESIRE)
country_mapping <- as.data.table(country_mapping)

dt <- merge(dt, country_mapping[, .(ISO3, `DESIRE code`)], 
            by.x = 'party', by.y = 'ISO3', all.x = TRUE)
setnames(dt, 'DESIRE code', 'EXIOBASE_region_code')
dt[is.na(EXIOBASE_region_code)]$party 
dt[party == 'ROU', EXIOBASE_region_code := 'RO']


dt[, .(party, EXIOBASE_region_code)] %>% unique

############################################################################## # 
##### 2. Get proxies ##########################################################
############################################################################## # 

############################################################################## # 
# _a) SUT ------------------------------------------------------------------
############################################################################## # 

# __i. load data ---------------------------------------------------------------

use <- read_feather(file.path(path2output, 'prepare_SUT_proxies_use.feather'))
#use[, country_industry := countrycode(country_industry,'iso2c', 'iso3c')]
use <- na.omit(use)


supply <- read_feather(file.path(path2output, 'prepare_SUT_proxies_supply.feather'))
#supply <- readRDS('./temp_results/4a_supply.RData')
#supply[, country_industry := countrycode(country_industry,'iso2c', 'iso3c')]
supply <- na.omit(supply)

# load meta data
#meta <- parse_EB3_metadata('/home/simon/Documents/PhD_PROSET/data/EXIOBASE3/V3.8.2/IOT_2015_pxp')


# __ii. data checks -----------------------------------------------------------
# are all proxy details there? (should return empty data.table)

test_that('all proxy details (corresponding EB indudstries and products) are there', {
  expect_equal(0, nrow(dt[proxy_data_source == 'USE'& is.na(EXIOBASE_products)]))
  expect_equal(0, nrow(dt[proxy_data_source == 'USE'& is.na(EXIOBASE_code)]))
  expect_equal(0, nrow(dt[proxy_data_source == 'SUPPLY'& is.na(EXIOBASE_products)]))
  expect_equal(0, nrow(dt[proxy_data_source == 'SUPPLY'& is.na(EXIOBASE_code)]))
})




# all information on EB sectors correct?
#dt$EXIOBASE_code %>% unlist %>% unique %>% non_common_elements(meta$industries$CodeNr %>% unique)
#dt$EXIOBASE_products %>% unlist %>% unique %>% non_common_elements(meta$products$CodeNr %>% unique)


# __iii. retrieve information from USE table ----------------------------------
dt[proxy_data_source == 'USE'
   , proxies := list(mapply(FUN = get_SUT_shares, 
                            products = EXIOBASE_products, 
                            industries = EXIOBASE_code, 
                            industry_countries = EXIOBASE_region_code, 
                            MoreArgs = list(sut = use), 
                            SIMPLIFY = FALSE)), 
   by = key]

# __iv. retrieve information from SUPPLY table ---------------------------------
dt[proxy_data_source == 'SUPPLY'
   , proxies := list(mapply(FUN = get_SUT_shares, 
                            products = EXIOBASE_products, 
                            industries = EXIOBASE_code, 
                            industry_countries = EXIOBASE_region_code, 
                            MoreArgs = list(sut = supply), 
                            SIMPLIFY = FALSE)), 
   by = key]

# __v. check results ----------------------------------------------------------
dt
dt[sapply(proxies, function(x) if (is.data.table(x)) nrow(x) == 1 else FALSE)]

############################################################################## # 
# _b) ROAD TRANSPORT ------------------------------------------------------------------
############################################################################## # 
road <- read_feather(file.path(path2output, 'prepare_ROAD_TRANSPORT_proxies.feather'))
road[, proxies := lapply(proxies, as.data.table)]
road[, proxy_data_source := 'PEFA']
#road$proxies_ROAD[[1]]
setnames(road, c('region', 'proxies'), c('EXIOBASE_region_code', 'proxies_ROAD'))

dt <- merge(dt, road, 
            by = c('EXIOBASE_region_code', 'proxy_data_source', 
                   'gas'), 
            sort = FALSE, 
            all.x = TRUE)

dt[sapply(proxies, is.data.table) & sapply(proxies_ROAD, is.data.table)]
dt[sapply(proxies_ROAD, is.data.table), proxies := proxies_ROAD]
dt[, proxies_ROAD := NULL]

dt[grepl('1.A.3.b', category_code) & !sapply(proxies, is.data.table)]



############################################################################## # 
# _c) IEA (power-to-heat ratio) ------------------------------------------------------------------
############################################################################## # 
# atm: use 0.5 (average of default power-to-heat values of table 2 of below report)
# see: https://ec.europa.eu/eurostat/documents/38154/42195/Final_CHP_reporting_instructions_reference_year_2016_onwards_30052017.pdf/f114b673-aef3-499b-bf38-f58998b40fe6
dt[proxy_data_source == 'USE']$proxies[[1]]

dt[proxy_data_source == 'IEA', 
   proxies := list(list(data.table(
     industry_code = unlist(EXIOBASE_code, recursive = FALSE), 
     share = 1 / length(unlist(EXIOBASE_code))
   ))), 
   by = .(key, gas)]


############################################################################## # 
##### Tests #################################################################
############################################################################## # 

shares_not_sum_to_1 <-   dt[
  !is.na(proxy_data_source)
  & !(proxy_data_source %in% c('1to1', 'PEFA'))
  & !sapply(proxies, function(x) isTRUE(all.equal(sum(x$share), 1)))
]

test_that('all proxy shares sum to 1 (SUPPLY, USE, IEA)', {
  expect_equal(nrow(shares_not_sum_to_1), 0)
})

missing_proxies <- dt[!(proxy_data_source %in% c('1to1'))
                      & !sapply(proxies, is.data.table)
                      , .(category_code, classification)] %>% 
  unique


test_that('no proxies are missing', {
  expect_equal(nrow(missing_proxies), 0)
})

# 



############################################################################## # 
##### Save results #################################################################
############################################################################## # 

save_results(dt)
#saveRDS(dt, './temp_results/5a_UNFCCC_samples_with_EXIOBASE_proxies.RData')

#TODO: check codes again in Correspondece table (maybe not fitiing codes are just wrong)!
###



# THE END ---------------------------------------------------------------------
# ############################################################################## # 
# # _b) PEFA ------------------------------------------------------------------
# ############################################################################## # 
# 
# # __i. load data ==================================================
# 
# pefa <- readRDS(file.path(path2output, 'prepare_PEFA_proxies.RData')) #'./temp_results/4b_PEFA_proxies.RData')
# setnames(pefa, 'proxies', 'proxies_NACErev2')
# pefa[, proxy_data_source := 'PEFA']
# 
# dt[proxy_data_source == 'PEFA']
# 
# 
# # __ii. Assign to NACE rev2 ==================================================
# dt <- merge(dt, pefa, 
#             by.x = c('year', 'party', 'gas', 'proxy_data_source'), 
#             by.y = c('time', 'party', 'gas', 'proxy_data_source'), 
#             all.x = TRUE, 
#             sort = FALSE)
# 
# dt[sapply(proxies_NACErev2, is.null) & proxy_data_source == 'PEFA']$party %>% unique
# # TODO: AUS, CAN, CHE, TUR not covered atm
# 
# dt[proxy_data_source == 'PEFA']
# 
# ############################################################################## # 
# # _c) Employment ------------------------------------------------------------------
# ############################################################################## # 
# 
# # __i. load data ==================================================
# empl1 <- readRDS(file.path(path2output, 'prepare_EMPLOYMENT_proxies_primary.RData'))
# empl2 <- readRDS(file.path(path2output, 'prepare_EMPLOYMENT_proxies_secondary.RData'))
# 
# empl1[, proxy_data_source := 'PEFA']
# empl1[, PEFA_avail := FALSE]
# 
# empl2[, proxy_data_source := 'PEFA']
# empl2[, PEFA_avail := TRUE]
# 
# setnames(empl1, 'region', 'EXIOBASE_region_code')
# setnames(empl2, 'region', 'EXIOBASE_region_code')
# 
# # __ii. attach to dt 
# # a) (countries with PEFA data -> as secondary proxy (only used to further split industries))
# # b) countries without PEFA -> as primary proxy (to also split btw hous/ind)
# 
# dt[proxy_data_source == 'PEFA' & !sapply(proxies_NACErev2, is.null), 
#    PEFA_avail := TRUE]
# dt[proxy_data_source == 'PEFA' & sapply(proxies_NACErev2, is.null), 
#    PEFA_avail := FALSE]
# 
# 
# dt <- merge(dt, empl1, 
#             by = c('EXIOBASE_region_code', 'proxy_data_source', 'PEFA_avail', 
#                    'gas'), 
#             sort = FALSE, 
#             all.x = TRUE)
# dt <- merge(dt, empl2, 
#             by = c('EXIOBASE_region_code', 'proxy_data_source', 'PEFA_avail'), 
#             sort = FALSE, 
#             all.x = TRUE)
# 
# # combine both columns to proxies_empl
# dt[!sapply(proxies_empl.x, is.null), proxies_empl := proxies_empl.x]
# dt[!sapply(proxies_empl.y, is.null), proxies_empl := proxies_empl.y]
# 
# 
# dt[grepl('^1.A.3.b', category_code), 
#    proxies := proxies_empl]
# 
# test_that("All proxies for Road Transport 1.A.3.b is there", {
#   expect_equal(0, 
#                nrow(dt[grepl('^1.A.3.b', category_code) & sapply(proxies_empl, is.null)]))
# })
# 
# 
# dt[, proxies_empl := NULL]
# dt[, proxies_empl.x := NULL]
# dt[, proxies_empl.y := NULL]
