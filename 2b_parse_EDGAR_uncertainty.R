#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-01-21 09:03:11
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(tidyverse)
library(units)
library(rio)
library(data.tree)
library(rriskDistributions)

############################################################################## # 
##### functions #########################################################
############################################################################## # 

source(file.path('src', 'functions.R'))
source(file.path('src', 'functions_trees.R'))

source(file.path('src', 'functions_dirichlet.R'))
############################################################################## # 
##### general settings #########################################################
############################################################################## # 

# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output

path2uncertainty <- config$path2edgar_uncertainty
path2edgar_coherent <- file.path(path2output, 'parse_EDGAR_emissions_coherent.RData')

############################################################################## # 
##### 2. load uncertainty data #############################################################
############################################################################## # 
data_un_raw <- import(path2uncertainty)
data_un_raw <- as.data.table(data_un_raw)

setnames(data_un_raw, c('L1', 'country', 'emi', 'sector'), 
         c('gas', 'country_code', 'emissions', 'category_code'))

# select relevant cols and rows
data_un <- data_un_raw[, 
                       .(country_code, category_code, gas, emissions, 
                         rel.unc.min, rel.unc.max)]
data_un[grepl('1.A.5', category_code)]
data_un$category_code %>% unique

# merge with official emissions
data_em <- readRDS(path2edgar_coherent)
data_un <- merge(data_un, data_em, by = c('country_code', 'category_code', 'gas'), 
      all.x = TRUE, suffixes = c('_solazzo', ''))
data_un[, emissions := drop_units(emissions)]
# data cleaning
data_un <- data_un[emissions >= 0]

# assign distribution: truncnorm for symetric uncertainty, lognormal for asymmetric
data_un[rel.unc.max != rel.unc.min, 
        dist := 'lognorm']
data_un[rel.unc.max == rel.unc.min, 
        dist := 'truncnorm']

# get distribution parameters: calculate (truncnorm), fit (lognormal)
data_un[, q2.5 := (emissions - rel.unc.min*emissions)]
data_un[, q97.5 := (emissions + rel.unc.max*emissions)]
data_un[q2.5 < 0 & dist == 'truncnorm', q2.5 := 0]
data_un[q2.5 < 0 & dist == 'lognorm', q2.5 := 1E-4]

# truncnorm parameters (mean, sd)
data_un[dist == 'truncnorm', 
        sd := (rel.unc.min / 1.96) * emissions]
data_un[dist == 'truncnorm', 
        cv := sd / (emissions)]

# lognorm paramters (meanlog, sdlog)
my_get.lnorm.par <- function(q2.5, q50, q97.5, show.output = FALSE, 
                             tol = 0.001,
                             plot = FALSE) {
  fit <-  try(suppressMessages(
    rriskDistributions::get.lnorm.par(
    p = c(0.025,0.5,0.975), 
    q = c(q2.5, q50, q97.5), 
    show.output = show.output, 
    plot = plot,
    tol = tol
  )))
  return(fit)
}

data_un[
  dist == 'lognorm', 
  lnorm_pars := list(Map(
    f = my_get.lnorm.par, 
    q2.5 = q2.5, 
    q50 = emissions, 
    q97.5 = q97.5
  ))
]

data_un[dist == 'lognorm' & sapply(lnorm_pars, length) != 2,]
data_un[dist == 'lognorm' & sapply(lnorm_pars, length) != 2,
        lnorm_pars := list(Map(
          f = my_get.lnorm.par, 
          q2.5 = q2.5, 
          q50 = emissions, 
          q97.5 = q97.5, 
          tol = 0.1
        ))
        ]
data_un[dist == 'lognorm' & sapply(lnorm_pars, length) != 2,]

# data cleaning 2
data_un[dist == 'lognorm' & sapply(lnorm_pars, length) != 2, 
        `:=`(dist = 'truncnorm', 
             sd = rel.unc.max, 
             cv = rel.unc.max / emissions)]



data_un[dist == 'lognorm', 
        `:=`(meanlog = unlist(lapply(lnorm_pars, function(x) as.numeric(x[1]))), 
             sdlog = unlist(lapply(lnorm_pars, function(x) as.numeric(x[2])))) ]
data_un[, lnorm_pars := NULL]

data_un[dist == 'lognorm' & is.na(sdlog)]
#data_un[country_code == 'SVK' & category_code == '1.B.1']


# Save results =================================================================

#saveRDS(data_em, file.path('temp_results', '3_EDGAR_emissions_cleaned.RData'))
#saveRDS(data_un, file.path('temp_results', '3_EDGAR_uncertainty_cleaned.RData'))
save_results(data_un)


# The End ======================================================================







