#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-12-02 10:28:49
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

############################################################################## # 
##### settings #################################################################
############################################################################## # 

source('./src/functions.R')

# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())

# paths to inventory data
path2unfccc <- file.path(path2output, "UNFCCC_get_EXIOBASE_proxies.RData")
path2edgar <- file.path(path2output, "EDGAR_get_proxies.RData")

# paths to residence adjustments
path2sea <- file.path(path2output, 'residence_adjustment_SEA_sample.feather')
path2air <- file.path(path2output, 'residence_adjustment_AIR_sample.feather')
# path2unfccc <- './temp_results/5a_UNFCCC_samples_with_EXIOBASE_proxies.RData'
# path2edgar <- './temp_results/6a_EDGAR_samples_with_EXIOBASE_proxies.RData'
 
############################################################################## # 
##### functions #################################################################
############################################################################## # 



############################################################################## # 
##### load data #############################################################
############################################################################## # 

data_list <- list()
data_list$unfccc <- readRDS(path2unfccc)
data_list$edgar <- readRDS(path2edgar)
data_list$res_adj_SEA <-  read_feather(path2sea)
data_list$res_adj_AIR <- read_feather(path2air)



# Prepare before merge
data_list$res_adj_SEA[, sample := as.list(sample)]
data_list$res_adj_AIR[, sample := as.list(sample)]
setnames(data_list$unfccc, 
         c('party', 'emissions_CRF', 'sd_CRF'), 
         c('country_code', 'emissions', 'sd'))


data_list$edgar[, classification := 'total_for_category']
data_list$res_adj_AIR[, classification := 'total_for_category']
data_list$res_adj_SEA[, classification := 'total_for_category']

data_list$res_adj_AIR[, category_code := '0.A']
data_list$res_adj_SEA[, category_code := '0.B']

setnames(data_list$res_adj_AIR, 'country', 'country_code')
setnames(data_list$res_adj_SEA, 'country', 'country_code')
#data_list$edgar[, emissions := drop_units(emissions)]


lapply(data_list, colnames)

data_list$unfccc <- data_list$unfccc[, .(year, country_code, #EXIOBASE_region_code, 
        gas, category_code, classification, emissions, 
        sample, proxies, EXIOBASE_code)]

data_list$edgar <- data_list$edgar[, .(year, country_code, #EXIOBASE_region_code, 
                                         gas, category_code, classification, emissions, 
                                         sample, proxies, EXIOBASE_code)]


data <- rbindlist(data_list, idcol = 'database', fill = TRUE, use.names = TRUE)

setkey(data, database, year, country_code, #EXIOBASE_region_code, 
       gas, 
       category_code, classification)



# Kick out EDGAR whenever UNFCCC is avaialbel ================================
country_dups <- data[database %in% c('edgar', 'unfccc'), 
                     .(database, country_code)] %>% 
  unique %>% 
  .[, length(database), by = country_code] %>% 
  .[V1 == 2] %>% 
  .$country_code

data[, duplicate := FALSE]
data[database %in% c('edgar', 'unfccc') & country_code %in% country_dups, duplicate := TRUE]

data_cleaned <- data[!(duplicate == TRUE & database == 'edgar')]

data_cleaned[, .(country_code, database)] %>% unique


############################################################################## # 
##### save results #############################################################
############################################################################## # 
save_results(data_cleaned)
save_results(data, suffix = '_full')
#saveRDS(data, './temp_results/7_UNFCCC_EDGAR_samples_combined.RData')

# THE END ---------------------------------------------------------------------















