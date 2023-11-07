#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2023-04-25 13:34:03
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
library(mRio)
library(logr)
library(countrycode)
library(testthat)
############################################################################## # 
##### settings #################################################################
############################################################################## # 
source('./src/functions.R')

setDTthreads(threads = 5)
# read config and setup log script
config <- setup_config_and_log()

path2output <- config$path2output
path2eb <- config$path2exiobaseIOT


theme_set(theme_bw())



############################################################################## # 
##### functions #################################################################
############################################################################## # 
source('./src/functions_eurostat.R')

############################################################################## # 
##### load data #############################################################
############################################################################## # 



############################################################################## # 
##### 1. PEFA + Employment #############################################################
############################################################################## # 
# all EU countries where first allocation based on PEFA, then on EMPL


empl <- readRDS(file.path(path2output, 'prepare_EMPLOYMENT_proxies_secondary.RData'))
pefa <- readRDS(file.path(path2output, 'prepare_PEFA_proxies.RData'))

empl <- as.data.table(unnest(empl, proxies_empl))
pefa <- as.data.table(unnest(pefa, proxies))

# kick out all countries which have PEFA data, but no employment (bc not part of EXIOBASE)
# "MKD" "SRB" "ISL"
regions_eu <- pefa$party %>% 
  unique

regions_eb <- empl$region %>% 
  unique %>% 
  countrycode(., 'iso2c', 'iso3c')
  
pefa <- pefa[!(party %in% setdiff(regions_eu, regions_eb))]


# TODO: combine PEFA and employment to ONE proxy table
# load correspondence table: NACE --> EXIOBASE =================================

ct_nace_eb <- readRDS(config$path2CT_NACE_EXIOBASE_parsed)
ct_nace_eb[, value := NULL]

#ct_nace_eb <- ct_nace_eb[, list(industry_code =list(sapply(target, c))), by = source]

# add rows for all combined nace codes (e.g. C10-C12)
CT_nace_combinations <- list(
  'C10-C12' = paste0('C', 10:12), 
  'C13-C15' = paste0('C', 13:15), 
  'C31_C32' = paste0('C', 31:32),
  'E37-E39' = paste0('E', 37:39), 
  'J59_J60'= paste0('J', 59:60 ),
  'J62_J63'= paste0('J', 62:63 ),
  'M69_M70'= paste0('M', 69:70 ),
  'M74_M75'= paste0('M', 74:75 ),
  'N80-N82'= paste0('N', 80:82 ),
  'Q87_Q88'= paste0('Q', 87:88 ),
  'R90-R92'= paste0('R', 90:92 )
) %>% 
  lapply(as.data.table) %>% 
  rbindlist(idcol = 'nace_comb') %>% 
  setnames('V1', 'nace_code') %>% 
  .[]
CT_nace_combinations_2EB <- merge(CT_nace_combinations, ct_nace_eb, 
                                  by.x = 'nace_code', by.y = 'source') %>% 
  .[, .(nace_comb, target)] %>% 
  unique %>% 
  setnames('nace_comb', 'source')

ct_nace_eb2 <- rbindlist(list(ct_nace_eb, CT_nace_combinations_2EB))

# add row to map household energy/emissions
ct_nace_eb3 <- rbindlist(list(ct_nace_eb2, data.table('source' = "HH_TRA", 'target' = 'y01')))

# which exiobase industries map to which nace industries? ======================
dt <- merge(pefa, ct_nace_eb3, by.x = 'nace_code', by.y = 'source', 
            allow.cartesian = TRUE, all.x = TRUE)

test_that("all sectors mapped", {
  expect_equal(0,nrow(dt[is.na(target)]))
})

dt[, region := countrycode(party, origin = 'iso3c', destination = 'iso2c')]

dt2 <- merge(dt, empl, 
             by.x = c('region', 'target'), 
             by.y = c('region', 'industry_code'), 
             suffixes = c('_pefa', '_empl'), 
             all.x = TRUE)

dt2[, sum(share_empl, na.rm = TRUE), by = .(region, gas, nace_code)]

# calculate shares per NACE sectors
dt2[, share_empl := share_empl / sum(share_empl, na.rm = TRUE), 
    by = .(region, party, nace_code, time, gas)]

dt2[nace_code == 'HH_TRA', share_empl := 1]
dt2 <- dt2[!is.na(share_empl)]

test_that('shares sum to one', {
  expect_equal(0, var(dt2[, sum(share_empl), by = .(region, nace_code, gas)]$V1))
  expect_true(all(dt2[, var(share_pefa), by = .(region, nace_code, gas)]$V1 == 0, na.rm = TRUE))
})

dt2[, share := share_pefa * share_empl]
dt2[, scaling := sum(share), by = .(region, gas)]
dt2[, scaling := 1 - scaling]
dt2[, share := share + (scaling * share)]


test_that('combined shares still sum to one', {
  expect_equal(0, var(dt2[, sum(share), by = .(region, gas)]$V1))
})

dt2 <- dt2[, list(share = sum(share)), by = .(region, gas, target, time)]

dt2 <- dt2[, list(proxies_pefa_empl = list(data.table(
  'industry_code' = target, 
  'share' = share
))), by = .(region, gas)]

############################################################################## # 
##### 2. Employment only #############################################################
############################################################################## # 
# all non-EU countries where allocation based on EMPL only
empl_primary <- readRDS(file.path(path2output, 'prepare_EMPLOYMENT_proxies_primary.RData'))

# Combine both
#dt_comb <- rbindlist(list('pefa' = dt2, 'nonpefa' = empl_primary), fill = TRUE)
dt_comb <- merge(dt2, empl_primary, by = c('region', 'gas'), 
                 all = TRUE)

# prefer: proxies coming from PEFA + EMpl
dt_comb[sapply(proxies_pefa_empl, is.data.table), proxies := proxies_pefa_empl]
dt_comb[!sapply(proxies_pefa_empl, is.data.table), proxies := proxies_empl]

test_that("proxy data for 49 regions available", {
  expect_equal(49, dt_comb$region %>% unique %>% length)
})


############################################################################## # 
##### save results #############################################################
############################################################################## # 
save_results(dt_comb[, .(region, gas, proxies)])

# THE END ---------------------------------------------------------------------
