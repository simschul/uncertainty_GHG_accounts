#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-09-12 14:23:28
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
library(pbmcapply)
library(testthat)
library(svMisc)
library(pbapply)
library(arrow)





############################################################################## # 
##### settings #################################################################
############################################################################## # 

source(file.path('src', 'functions.R'))
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())

n_cores <- config$n_cores
RhpcBLASctl::blas_set_num_threads(4)
RhpcBLASctl::blas_get_num_procs()


############################################################################## # 
##### functions #############################################################
############################################################################## # 


############################################################################## # 
##### load data #############################################################
############################################################################## # 
# meta data
F_rownames <- readRDS(file.path(path2output, 'convert_to_matrix_rownames.RData'))
EB_regions <- parse_EB3_metadata(config$path2exiobaseIOT) %>% 
  .[['indices_Y']] %>% 
  .[['colnames']] %>% 
  .[['region']] %>% 
  unique %>% 
  data.table(EB_region = .) %>% 
  .[, id := 1:.N] %>% 
  .[]


# read data
files_national <- list.files(file.path(path2output, 'footprints'), 
                             'national', full.names = TRUE)

fp_national <- pbmclapply(files_national, read_feather, mc.cores = n_cores)
fp_national <- rbindlist(fp_national)

fp_national <- merge(fp_national, F_rownames, 
                     by.x = 'i', by.y = 'row')


############################################################################## # 
##### calculate summary statistics #############################################################
############################################################################## # 

# _a. national fps by country ===============================================================

fp_by_country <- fp_national[, list(value = sum(value)), 
                             by = .(gas, j, run)]

fp_by_country <- fp_by_country[, list(mean = mean(value), 
                                      median = median(value), 
                                      sd = sd(value), 
                                      cv = sd(value) / mean(value), 
                                      CI2.5 = quantile(value, probs = 0.025), 
                                      CI97.5 = quantile(value, probs = 0.975)), 
                               by = .(gas, j)]
fp_by_country[, mean_rel := mean / sum(mean), by = .(gas)]

fp_by_country <- merge(fp_by_country, EB_regions, by.x = 'j', by.y= 'id', 
      sort = FALSE)[, j := NULL]
setcolorder(fp_by_country, c('gas', 'EB_region'))
fp_by_country[EB_region == 'CY']

# _b. national fps by country and category ===================================================

fp_by_country_cat <- fp_national[, list(mean = mean(value), 
                                        median = median(value), 
                                        sd = sd(value), 
                                        cv = sd(value) / mean(value), 
                                        CI2.5 = quantile(value, probs = 0.025), 
                                        CI97.5 = quantile(value, probs = 0.975)), 
                                 by = .(gas, j, category_code2)]
fp_by_country_cat[, mean_rel := mean / sum(mean), by = .(gas)]
fp_by_country_cat <- merge(fp_by_country_cat, EB_regions, by.x = 'j', by.y= 'id', 
                       sort = FALSE)[, j := NULL]
setcolorder(fp_by_country_cat, c('gas', 'EB_region'))



# save results ==========================================

save_results(fp_by_country, suffix = '_national', type = '.feather')
save_results(fp_by_country_cat, suffix = '_national_by_category', type = '.feather')
#save_results(fp_national, suffix = '_national_detailed', type = '.feather')




