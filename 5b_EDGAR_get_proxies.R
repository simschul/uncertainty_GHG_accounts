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

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source('./src/functions.R')

# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())



#' Get use or supply shares from SUTs (result of script 0a_prepare_exiobase_SUTs.R)
#' 
#' Extract use/supply shares for 
#' - (a) specific product(s) 
#' - from (a) specific countr(ies)
#' - in specific industries
#' - in specific countries
#' 
#'
#' @param x a supply or use table (result of script 0a_prepare_exiobase_SUTs.R)
#' @param products 
#' @param industries 
#' @param product_countries 
#' @param industry_countries 
#'
#' @return
#' @export
#'
#' @examples 
# sut = use
# products = temp$EXIOBASE_products %>% unlist %>% unique
# industries = temp$EXIOBASE_code %>% unlist %>% unique %>% as.factor()
# industry_countries = c('FIN', 'DEU')
# product_countries <- 'all'

get_SUT_shares <- function(sut, 
                           products = 'all', 
                           industries = 'all', 
                           product_countries = 'all', 
                           industry_countries = 'all') {
  #sut <- copy(sut)
  #print(sut$product_code)
  #print(is.data.table(sut))
  if (length(products) == 1 && products == 'all') products <- sut$product_code
  if (length(industries) == 1 && industries == 'all') industries <- sut$industry_code
  if (length(industry_countries) == 1 && industry_countries == 'all') industry_countries <- sut$country_industry
  SUT_shares <- sut[
    product_code %in% products
    & industry_code %in% industries
    & country_industry %in% industry_countries
    , list(value = sum(value))
    , by = .(industry_code)
  ]
  SUT_shares[, country_total := sum(value)]
  SUT_shares[, share := value / country_total]
  
  
  if (nrow(SUT_shares) == 0) {
    # product not used in any of the industries --> equally share them among all
    # TODO: more elaborate
    SUT_shares <- data.table(industry_code = industries, 
                             share = 1 / length(industries))
  }
  
  return(SUT_shares[, .(industry_code, share)])
  
}


############################################################################## # 
##### load data #############################################################
############################################################################## # 

edgar_samples <- readRDS(file.path(path2output, 'sample_EDGAR.RData')) #'./temp_results/3b_EDGAR_samples.RData')
ct <- readRDS(config$path2CT_CRF_EDGAR_parsed)
#ct <- readRDS('/home/simon/Documents/Projects/Uncertainty_Extensions/code/UNFCCC_CRF_correspondence_tables/intermediate_results/CT_EDGAR_parsed.RData')



############################################################################## # 
##### 1. Merge UNFCCC samples with EXIOBASE Correspondence table ###############
############################################################################## # 

# _a) data preparation =========================================================

# edgar_samples[, category_code := pathString %>% 
#                  gsub('I\\/', '', .) %>% 
#                  gsub('\\/', '\\.', .)]

ct <- ct[, .(id,  EDGAR_code, EXIOBASE_code, proxy_data_source, EXIOBASE_products)]

setkey(edgar_samples, year, country_code, gas, category_code)
setkey(ct, EDGAR_code)

# _b) merge ===================================================================

dt <- merge(edgar_samples, ct, 
            by.x = c('category_code'), 
            by.y = c('EDGAR_code'), 
            all.x = TRUE)
dt[, n_corres := list(sapply(EXIOBASE_code, length))]
#dt <- dt[proxy_data_source %in% c('USE', 'SUPPLY') | n_corres == 1]


# _c) checks ==================================================================
dt$proxy_data_source %>% unique
dt[is.na(EXIOBASE_code) & proxy_data_source %in% c('SUPPLY, USE'), ]

# check coverage ====
edgar_samples3 <- merge(edgar_samples[, list(total_emissions = sum(emissions, na.rm = TRUE)), by = gas], 
                         dt[, list(covered_emissions=  sum(emissions, na.rm = TRUE)), by = gas], 
                         by = 'gas')
edgar_samples3[, percent_covered := covered_emissions / total_emissions]
edgar_samples3

# _d) more preparations ======================================================

dt[, key := paste(year, country_code, 
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
dt[EXIOBASE_code == 'all']
dt[proxy_data_source == ('USE')]
dt[is.na(proxy_data_source) & n_corres != 1]

# _e) attach EXIOBASE region to each country ==================================
library(countrycode)
country_mapping <- rio::import(config$path2CountryMappingDESIRE)
country_mapping <- as.data.table(country_mapping)

dt <- merge(dt, country_mapping[, .(ISO3, `DESIRE code`)], 
      by.x = 'country_code', by.y = 'ISO3', all.x = TRUE)
setnames(dt, 'DESIRE code', 'EXIOBASE_region_code')
dt[is.na(EXIOBASE_region_code)]$country_code 
dt[country_code == 'ROU', EXIOBASE_region_code := 'RO']
dt[country_code == 'SCG', EXIOBASE_region_code := 'WE']

dt <- dt[!(country_code %in% c('SEA', 'AIR'))]

############################################################################## # 
##### 2. Get proxies ##########################################################
############################################################################## # 

############################################################################## # 
# _a) SUT ------------------------------------------------------------------
############################################################################## # 

# __i. load data ---------------------------------------------------------------

use <- readRDS(file.path(path2output, 'prepare_SUT_proxies_use.RData'))
use <- na.omit(use)


supply <- readRDS(file.path(path2output, 'prepare_SUT_proxies_supply.RData'))
supply <- na.omit(supply)


# load meta data
#meta <- parse_EB3_metadata('/home/simon/Documents/PhD_PROSET/data/EXIOBASE3/V3.8.2/IOT_2015_pxp')
meta <- parse_EB3_metadata(config$path2exiobaseIOT)


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
dt[sapply(proxies, function(x) if (is.data.table(x)) nrow(x) == 1 else FALSE)]$proxies[[1]]


############################################################################## # 
# _b) ROAD TRANSPORT ------------------------------------------------------------------
############################################################################## # 
road <- readRDS(file.path(path2output, 'prepare_ROAD_TRANSPORT_proxies.RData'))
road[, proxy_data_source := 'PEFA']
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
                      , .(category_code)] %>% 
  unique

test_that('no proxies are missing', {
  expect_equal(nrow(missing_proxies), 0)
})


############################################################################## # 
##### Save results #################################################################
############################################################################## # 
save_results(dt)
#saveRDS(dt, './temp_results/6a_EDGAR_samples_with_EXIOBASE_proxies.RData')

#TODO: check codes again in Correspondece table (maybe not fitiing codes are just wrong)!
###



# THE END ---------------------------------------------------------------------


# ############################################################################## # 
# # _b) PEFA ------------------------------------------------------------------
# ############################################################################## # 
# 
# # __i. load data ==================================================
# pefa <- readRDS(file.path(path2output, 'prepare_PEFA_proxies.RData')) #'./temp_results/4b_PEFA_proxies.RData')
# setnames(pefa, 'proxies', 'proxies_NACErev2')
# pefa[, proxy_data_source := 'PEFA']
# 
# temp <- copy(dt)
# # __ii. Assign to NACE rev2 ==================================================
# dt <- merge(dt, pefa, 
#             by.x = c('year', 'country_code', 'gas', 'proxy_data_source'), 
#             by.y = c('time', 'party', 'gas', 'proxy_data_source'), 
#             all.x = TRUE, 
#             sort = FALSE)
# 
# dt[sapply(proxies_NACErev2, is.null) & proxy_data_source == 'PEFA']$country_code %>% unique
# 
# #
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
# 
# dt[proxy_data_source == 'PEFA']
