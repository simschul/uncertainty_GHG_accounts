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
library('RPostgreSQL')


# row <- 33
# col <- 7987
# run <- 5000
# zeroes <- 0.15
# n <- row * col * run * (1-zeroes)
# 
# object.size(124242.23445345)
# test <- data.table(
#   id = 1:n, 
#   row = n:1,
#  # col = letters,
#   run = 1:run,
#   value = 1.2
#   )
# 
# 
# object.size(test) %>% format(units = 'GiB')



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

F_rownames <- readRDS(file.path(path2output, 'convert_to_matrix_rownames.RData'))


pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user = 'postgres', 
                dbname = 'template1', 
                host = 'localhost',
                password = '123')

blocks <- seq(1, 7987, 1000)

dt_list <- vector('list', length(blocks)-1)
i <- 1
for (i in 1:(length(blocks)-1)) {
  query <- paste0("select * from footprint_multiplier where j between ", 
                  blocks[i], ' and ', blocks[i+1]-1)
  dt <- dbGetQuery(con, query) #TODO: use rpostgres (https://github.com/r-dbi/RPostgres)
  dt <- merge(dt, F_rownames, by.x = 'i', by.y = 'row') #TODO
  dt <- dt[, list(value = sum(value)), 
           by = .(gas, j, run)]
  dt <- dt[, list(mean = mean(value), 
                  median = median(value), 
                  sd = sd(value), 
                  cv = sd(value) / mean(value), 
                  CI2.5 = quantile(value, probs = 0.025), 
                  CI97.5 = quantile(value, probs = 0.975)), 
           by = .(gas, j)]
  
  dt_list[[i]] <- dt
  
  progress(i,length(blocks)-1)
}


pbmclapply(1:7987, function(x) {
  dt <- dbGetQuery(con, paste0("select * from footprint_multiplier where j = ", x))
})



# _a. aggregate multiplier by sector =====================================================
fp_multiplier_by_sector <- dt[, list(value = sum(value)), 
                              by = .(gas, id, run)]
fp_multiplier_by_sector <- fp_multiplier_by_sector[, list(mean = mean(value), 
                                                          median = median(value), 
                                                          sd = sd(value), 
                                                          cv = sd(value) / mean(value), 
                                                          CI2.5 = quantile(value, probs = 0.025), 
                                                          CI97.5 = quantile(value, probs = 0.975)), 
                                                   by = .(gas, id)]
fp_multiplier_by_sector[, mean_rel := mean / sum(mean), by = .(gas)]
#fp_multiplier_by_sector[, sum(mean_rel), by = gas]
save_results(fp_multiplier_by_sector, suffix = '_multiplier', type = '.feather')
rm(fp_multiplier_by_sector)

# _b. multiplier by sector and categoory =========================================
fp_multiplier_by_sector_cat <- dt[, list(mean = mean(value), 
                                         median = median(value), 
                                         sd = sd(value), 
                                         cv = sd(value) / mean(value), 
                                         CI2.5 = quantile(value, probs = 0.025), 
                                         CI97.5 = quantile(value, probs = 0.975)), 
                                  by = .(gas, id, category_code2)]
fp_multiplier_by_sector_cat[, mean_rel := mean / sum(mean), by = .(gas)]

save_results(fp_multiplier_by_sector_cat, suffix = '_multiplier_by_category', type = '.feather')
rm(fp_multiplier_by_sector_cat)



