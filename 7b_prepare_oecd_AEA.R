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
library(mRio)
library(countrycode)
############################################################################## # 
##### settings #################################################################
############################################################################## # 
source(file.path('src', 'functions.R'))
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

dt <- fread(config$path2oecd)
dt <- dt[ACTIVITY %in% c('IND-TOTAL', 'HH-TOTAL')]
dt[, emissions_OECD := Value / 1E3]
dt[, country := countrycode(COUNTRY, 'iso3c', 'iso2c')]
dt <- dt[, .(country, POLLUTANT, ACTIVITY, emissions_OECD)]
setnames(dt, c('POLLUTANT', 'ACTIVITY'), c('gas', 'activity'))
dt

save_results(dt)


