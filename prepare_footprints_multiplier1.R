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

files_multiplier <- list.files(file.path(path2output, 'footprints'), 
                               'multiplier', full.names = TRUE)

fp_multiplier <- pbmclapply(files_multiplier, read_feather, mc.cores = n_cores)
fp_multiplier <- rbindlist(fp_multiplier)
setorder(fp_multiplier, gas, j, run)

############################################################################## # 
##### calculate summary statistics #############################################################
############################################################################## # 


fp_multiplier_by_sector <- fp_multiplier[, list(mean = mean(value), 
                                                          median = median(value), 
                                                          sd = sd(value), 
                                                          cv = sd(value) / mean(value), 
                                                          CI2.5 = quantile(value, probs = 0.025), 
                                                          CI97.5 = quantile(value, probs = 0.975)), 
                                                   by = .(gas, j)]
fp_multiplier_by_sector[, mean_rel := mean / sum(mean), by = .(gas)]

save_results(fp_multiplier_by_sector, type = '.feather')
rm(fp_multiplier_by_sector)

# end ======







# 2. FP multiplier

# fp_multiplier <- pbmclapply(X = files_multiplier, FUN = fread, 
#                             nThread = 1,
#                             header = TRUE,
#                             mc.cores = 1)

# fp_multiplier <- pblapply(X = files_multiplier, FUN = fread, 
#                             nThread = 4,
#                             header = TRUE)

#read_reshape_and_save_data <- function(file) 

# begin sQL
#library('RPostgreSQL')


# dtab = dbGetQuery(con, "select * from iris")
# summary(dtab)
# 
# test <- lapply(1:10, function(x) data.table(
#   i = 1:10, 
#   j = 1:10, 
#   value = runif(10)
# ))
# 
# dbCreateTable(con, 'test', test[[1]])
# dbReadTable(con, 'test')  
# dbSendQuery(con, "drop table iris")
# dbRemoveTable(con, 'test')
# for (i in 1:length(test)) {
#   dbWriteTable(con, 'test', test[[i]], append = TRUE, 
#                row.names = FALSE)
# }
# dt <- dbGetQuery(con, "select * from test where i = 1")
# 
# dbReadTable(con, 'test')  

# pg = dbDriver("PostgreSQL")
# con = dbConnect(pg, user = 'postgres', 
#                 dbname = 'template1', 
#                 host = 'localhost',
#                 password = '123')
# dbRemoveTable(con, 'footprint_multiplier')
# dbDisconnect(con)
# for (irun in 1:length(files_multiplier)) {
#   x <- fread(files_multiplier[[irun]], nThread = 6, header = TRUE)
#   x <- melt(x[,i:=.I], 
#             id.vars="i", 
#             variable.name = 'j')
#   x <- x[value>0]
#   x[,j:=as.integer(j)]
#   x[, run := irun]
#   dbWriteTable(con, 'footprint_multiplier', x, 
#                append = TRUE, 
#                row.names = FALSE)
#   if (irun %in% seq(0, config$sample_size, 100)){
#     gc()
#     cat(irun, '')
#   } 
# }
# 
# # _a. aggregate multiplier by sector
# x <- 1
# pbmclapply(1:7987, function(x) {
#   dt <- dbGetQuery(con, paste0("select * from footprint_multiplier where j = ", x))
# })
# 
# dt <- dbGetQuery(con, "select * from test where j = 1")






# end sql


# unlink(list.files(path2output, 'multiplier_sparse.csv', full.names = TRUE))  
# 
# for (irun in 1:length(files_multiplier)) {
#   x <- fread(files_multiplier[[irun]], nThread = 1, header = TRUE)
#   x <- melt(x[,i:=.I], 
#             id.vars="i", 
#             variable.name = 'j')
#   x <- x[value>0]
#   x[,j:=as.integer(j)]
#   x[, run := irun]
#   suppressMessages(save_results(x,
#                                 suffix = paste0('multiplier_sparse'), 
#                                 type = '.csv', 
#                                 append = TRUE))
#   if (i %in% seq(0, config$sample_size, 100)){
#     gc()
#     cat(i, '')
#   } 
# }


# reshape_data <- function(x) {
#   setnames(x, paste0('V', 1:7987))
#   x <-  set(x, j = c("gas", 'category_code2'), 
#             value = F_rownames[, .(gas, category_code2)])
#   
#   x <- melt(x, variable.name = 'id', variable.factor = FALSE, 
#             measure.vars = paste0('V', 1:7987),
#             id.vars = c('gas', 'category_code2'))
#   x <- x[value > 0]
#   x[, id := as.numeric(gsub('V', '', id))]
#   return(x)
#   }
#object.size(fp_multiplier) %>% format(units = 'GiB')

# for (i in 1:length(fp_multiplier)) {
#   #fp_multiplier[[i]] <- reshape_data(fp_multiplier[[i]])
#   out <- reshape_data(fp_multiplier[[i]])
#   
#   if (i %in% seq(1, config$sample_size, 100)){
#     gc()
#     cat(i, '')
#   } 
# }

#fp_multiplier <- pblapply(fp_multiplier, reshape_data)


# fp_multiplier <- pbmclapply(fp_multiplier, function(x) {
#   x <- as.data.table(x)
#   x <- cbind(x, F_rownames[, .(gas, category_code2)])
#   x <- melt(x, variable.name = 'id', variable.factor = FALSE, 
#             id.vars = c('gas', 'category_code2'))
#   x[, id := as.numeric(gsub('V', '', id)) - 2]
#   x <- x[value > 0]
#   return(x)
# }, mc.cores = 1)

# test_that('ids are correct', {
#   expect_equal(min(fp_multiplier$id), 1)
#   expect_equal(max(fp_multiplier$id), 7987)
# })

# gc()
# 
# 
# 
# fp_multiplier <- rbindlist(fp_multiplier, idcol = 'run')[value > 0]
# 
# 
# 
# save_results(fp_multiplier, suffix = '_multiplier_detailed', type = '.feather')
# 
# # _a. aggregate multiplier by sector
# fp_multiplier_by_sector <- fp_multiplier[, list(value = sum(value)), 
#                                          by = .(gas, id, run)]
# fp_multiplier_by_sector <- fp_multiplier_by_sector[, list(mean = mean(value), 
#                                                           median = median(value), 
#                                                           sd = sd(value), 
#                                                           cv = sd(value) / mean(value), 
#                                                           CI2.5 = quantile(value, probs = 0.025), 
#                                                           CI97.5 = quantile(value, probs = 0.975)), 
#                                                    by = .(gas, id)]
# fp_multiplier_by_sector[, mean_rel := mean / sum(mean), by = .(gas)]
# #fp_multiplier_by_sector[, sum(mean_rel), by = gas]
# save_results(fp_multiplier_by_sector, suffix = '_multiplier', type = '.feather')
# rm(fp_multiplier_by_sector)
# 
# # _b. multiplier by sector and categoory
# fp_multiplier_by_sector_cat <- fp_multiplier[, list(mean = mean(value), 
#                                                     median = median(value), 
#                                                     sd = sd(value), 
#                                                     cv = sd(value) / mean(value), 
#                                                     CI2.5 = quantile(value, probs = 0.025), 
#                                                     CI97.5 = quantile(value, probs = 0.975)), 
#                                              by = .(gas, id, category_code2)]
# fp_multiplier_by_sector_c