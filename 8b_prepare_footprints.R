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
#library('RPostgreSQL')

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
calculate_summary_statistics2 <- function(dt) {
  out <- dt[, list(
    mean = mean(value), 
    median = median(value), 
    sd = sd(value), 
    cv = sd(value) / mean(value), 
    CI97.5 = quantile(value, probs = 0.975),
    CI2.5 = quantile(value, probs = 0.025)
  ), by = key(dt)]
  out[, mean_rel := mean / sum(mean), by = gas]
  
  return(out[])
}


############################################################################## # 
##### load data #############################################################
############################################################################## # 

# meta data
meta <- parse_EB3_metadata(config$path2exiobaseIOT)
F_rownames <- readRDS(file.path(path2output, 'convert_to_matrix_rownames.RData'))

# 1. national footprints footprint =============================================
fp_detailed <- read_feather(file.path(path2output, 'calculate_footprints_national.feather'))

# rows
fp_detailed <- merge(fp_detailed, F_rownames, by.x = 'i', by.y = 'row', all.x = TRUE, 
                sort = FALSE)

# columns
EB_regions <- data.table(
  EB_region = meta$indices_Y$colnames$region %>% unique, 
  j = 1:49)
fp_detailed <- merge(fp_detailed, EB_regions, by = 'j')


# summary statistics
fp_by_reg <- fp_detailed[, list(value = sum(value)), by = .(EB_region, gas, run)]
setkey(fp_by_reg, EB_region, gas)

fp_by_reg_crf <- fp_detailed[, list(value = sum(value)), 
                             by = .(EB_region, gas, category_code2, run)]
setkey(fp_by_reg_crf, EB_region, gas, category_code2)

fp_by_reg2 <- calculate_summary_statistics2(fp_by_reg)
fp_by_reg_crf2 <- calculate_summary_statistics2(fp_by_reg_crf)

# save results
save_results(fp_by_reg2, suffix = '_by_region', type = '.feather')
save_results(fp_by_reg_crf2, suffix = '_by_region_and_CRF', type = '.feather')

rm(fp_by_reg, fp_by_reg2, fp_by_reg_crf, fp_by_reg_crf2)



# 2. sector footprints footprint =============================================
fp_detailed <- read_feather(file.path(path2output, 
                                      'calculate_footprints_multiplier.feather'))

# rows
fp_detailed <- merge(fp_detailed, F_rownames[, .(row, gas)], 
                     by.x = 'i', by.y = 'row', all.x = TRUE, 
                     sort = FALSE)

# sum by gas (no detail on CRF needed at sector level)
fp_detailed <- fp_detailed[, list(value = sum(value)), 
                           by = .(j, gas, run)]

# summary statistics
setkey(fp_detailed, j, gas)
fp_by_sec <- calculate_summary_statistics2(fp_detailed)

# columns
EB_sectors <- merge(meta$indices_S$colnames[, j := 1:.N], 
                    meta$industries[, .(Name, CodeNr)], 
                    by.x = 'sector', by.y = 'Name', 
                    sort = FALSE)
EB_sectors <- EB_sectors[, .(j, region, CodeNr)]
setnames(EB_sectors,c('region', 'CodeNr'), c("EB_region", 'industry_code'))

fp_by_sec <- merge(fp_by_sec, EB_sectors, by = 'j')

# save results
save_results(fp_by_sec, suffix = '_by_sector', type = '.feather')



# end ==========================================================================












