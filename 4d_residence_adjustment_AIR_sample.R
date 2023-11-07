#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-12-14 15:49:45
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
library(truncnorm)
library(testthat)


############################################################################## # 
##### functions #################################################################
############################################################################## # 

source('./src/functions.R')
source('./src/functions_dirichlet.R')

#source('./src/functions_dirichlet2.R')

############################################################################## # 
##### settings #################################################################
############################################################################## # 
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output


path2shares <- file.path(path2output, 'residence_adjustment_AIR_prepare.RData')
path2sharesEU <- file.path(path2output, 'residence_adjustment_AIR_prepare_eurostat.RData')
path2edgar <- file.path(path2output, 'parse_EDGAR_emissions2015_parsed.RData')
path2edgar_un <- file.path(path2output, 'parse_EDGAR_uncertainty.RData') 

# read config
config <- config::get()
N <- config$sample_size
YEAR <- config$year



############################################################################## # 
##### 1. Aggregate #############################################################
############################################################################## # 

# _a) load data ================================================================

# Total emissions from international sea transport from EDGAR
edgar <- readRDS(path2edgar)
edgar <- edgar[year == YEAR & country_code == 'AIR']
edgar[, category_code := substr(category_code, 1, 3)]

# Uncertainty of this data from Solazzo 2021
edgar_un <- readRDS(path2edgar_un)
edgar_un <- edgar_un[country_code == 'AIR']
edgar_un[, category_code := substr(category_code, 1, 3)]

# _b) Merge emissions with uncertainty data ====================================
data <- merge(edgar[, .(gas, year, category_code, emissions)], 
              edgar_un[, .(gas, category_code, emissions, dist, sd, cv, meanlog, sdlog)], 
              by = c('category_code', 'gas'), 
              suffixes = c('', '_solazzo'))



# _c) sampling ================================================================ 
# Sample each aggregate emission category/gas
data[
  dist == 'truncnorm'
  , sample := list(
    mapply(FUN = rtruncnorm, 
           n = N, 
           a = 0, 
           mean = emissions, 
           sd = sd, 
           SIMPLIFY = FALSE)
  )
] 

data[
  dist == 'lognorm'
  , sample := list(
    mapply(FUN = rlnorm, 
           n = N, 
           meanlog = meanlog, 
           sdlog = sdlog, 
           SIMPLIFY = FALSE)
  )
] 

# _d) sum samples by gas =======================================================
agg_samples <- data[, list(sample_agg = sum_samples(sample)), by = .(gas, year)]


# tests ===========
agg_samples[, list(lapply(sample_agg, median))]
data[, round(sum(emissions)), by = gas]



############################################################################## # 
##### 2. Disaggregate #############################################################
############################################################################## # 

# _a) load data ===============================================================
# Country specific use shares from Eurostat + Worldbank
country_shares_eu <- readRDS(path2sharesEU)
country_shares <- readRDS(path2shares)


# _b) Rescale regional (EU/nonEU) shares to global shares ==========================
# how much of global int. air emissions are covered by eurostat countries?
regio_covr <- merge(
  edgar[, list(emissions= sum(emissions)), by= gas],
  country_shares_eu[, list(eurostat_bridge= sum(eurostat_bridge)), by= gas],
  by = 'gas'
)

# calc scaling factors 
regio_covr[, EU := eurostat_bridge / emissions]
regio_covr[, nonEU := set_units(1, 1) - EU]
regio_covr <- melt(regio_covr, id.vars = c('gas'), 
                   measure.vars = c('EU', 'nonEU'), 
                   value.name = 'scaling_factor',
                   variable.factor = FALSE, variable.name = 'region')

# rescale share from EU/nonEU level to global level
country_shares <- merge(country_shares, 
                        regio_covr, 
                        by = c('gas', 'region'), 
                        all.x = TRUE)

country_shares[, share := share_regional * scaling_factor]
country_shares[, share := drop_units(share)]
country_shares <- country_shares[share != 0]

#_c) Sample shares (dirichlet) =================================================

# __i. Find MaxEnt Gamma =======================================================
samples_dt <- country_shares[, 
                             list(
                               nloptr = list(find_gamma_maxent2(shares =  share, 
                                                               eval_f = eval_f,
                                                               eval_grad_f = dirichlet_entropy_grad
                                                               )), 
                               shares = list(data.table(country = country, 
                                                   share = share))
                             ), 
                             by = .(gas, year)]

samples_dt[, gamma := sapply(nloptr, function(x) x$solution)]


# samples_dt <- country_shares[, list(shares = list(
#   data.table(country = country, 
#              share = share)
# )), by = .(gas, year)] %>% 
#   merge(samples_dt, ., by = c('gas', 'year'))


# __ii. Sample from dirichlet =================================================
samples_dt[, samples_disagg := list(
  mapply(FUN = function(N, shares, gamma) {
    return(
      data.table(
        country = shares[['country']],
        sample = rdirichlet_list(N, shares[['share']] * gamma)
      )
    )
  }, 
  shares = shares, 
  gamma = gamma, 
  MoreArgs = list(N = N),
  SIMPLIFY = FALSE)
)]


############################################################################## # 
##### 3. Combine agg & disagg samples ##########################################
############################################################################## # 
samples_dt <- merge(samples_dt, agg_samples, by = c('gas', 'year'))

# Multiply share samples with agg samples
samples_dt[, samples := list(
  Map(f = function(samples_disagg, sample_agg) {
    data.table(
      country = samples_disagg[['country']], 
      sample = lapply(samples_disagg[['sample']], function(x) x * sample_agg)
    )
  }, 
  samples_disagg = samples_disagg, 
  sample_agg = sample_agg)
)]

# test
test_that('all good', {
  expect_equal(samples_dt$samples[[1]]$sample %>% sum_samples() %>% unlist,
               samples_dt$sample_agg[[1]])
})

# Unnest
samples_dt2 <- samples_dt[, rbindlist(samples), by = .(gas, year)]

# EXIOBASE sector: i62 Air transport
samples_dt2[, EXIOBASE_code := 'i62']


############################################################################## # 
##### Tests #############################################################
############################################################################## # 

gas_mean <- samples_dt2[, list(mean = mean(unlist(sum_samples(sample)))), 
                            by = gas] %>% 
  setorder(gas) %>% 
  setkey()

orig_emissions <- data[, list(mean = drop_units(sum(emissions))), by = gas] %>% 
  setorder(gas) %>% 
  setkey()



test_that('all good', {
  
  #expect_true(zero_range(country_shares[, sum(share), by = .(gas, year)]$V1))
  #expect_equal(country_samples[, sum(share)], 3)
  
  # expect_equal(
  #   gas_mean,
  #   agg_samples[, list(mean = mean(unlist(sum_samples(sample)))), by = gas] %>% 
  #     setorder(gas)
  # )
  
  expect_equal(
    gas_mean, 
    orig_emissions, 
    tolerance = 0.1
  )
})


############################################################################## # 
##### save results #############################################################
############################################################################## # 
save_results(samples_dt2)

# THE END ---------------------------------------------------------------------
