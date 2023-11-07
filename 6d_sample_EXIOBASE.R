#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-07-20 10:31:39
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
library(ggthemes)

library(logr)
library(rlang)
library(pbmcapply)
library(testthat)
############################################################################## # 
##### functions #################################################################
############################################################################## # 
source(file.path('src', 'functions_plot.R'))
source(file.path('src', 'functions.R'))


unnest_dt <- function(tbl, col) {
  
  tbl <- as.data.table(tbl)
  
  col <- ensyms(col)
  
  clnms <- syms(setdiff(colnames(tbl), as.character(col)))
  
  tbl <- as.data.table(tbl)
  
  tbl <- eval(
    expr(tbl[, as.character(unlist(!!!col)), by = list(!!!clnms)])
  )
  
  colnames(tbl) <- c(as.character(clnms), as.character(col))
  
  tbl
}

sample_dirichlet <- function(sample_aggregate,
                             proxies, 
                             gamma = 1) {
  if (!is.data.table(proxies)) stop('proxies must be data.table')
  if (!identical(names(proxies), c('industry_code', 'share'))) stop('proxies must have columns "industry code" and "share"')
  
  N <- length(sample_aggregate)
  sample_shares <- gtools::rdirichlet(N, proxies$share * gamma)
  sample <- sample_shares * sample_aggregate
  
  sample_dt <- data.table(
    industry_code = proxies$industry_code, 
    sample = as.list(as.data.table(sample))
  ) 
  return(sample_dt)
}


sample_dirichlet_uninformative <- function(sample, 
                                           target) {
  
  N <- length(sample)
  sample_shares <- gtools::rdirichlet(N, rep(1, length(target)))
  sample_target <- sample_shares * sample
  sample_dt <- data.table(
    industry_code = target, 
    sample = as.list(as.data.frame(sample_target))
  ) 
  return(sample_dt)
}





sample_1to1 <- function(sample_aggregate, 
                        proxies) {
  if (!is.data.table(proxies)) stop('proxies must be data.table')
  if (!identical(names(proxies), c('industry_code', 'share'))) stop('proxies must have columns "industry code" and "share"')
  if (nrow(proxies) != 1) stop('no 1 to 1 correspondence!')
  
  
  sample_dt <- data.table(
    industry_code = proxies$industry_code, 
    sample = as.list(as.data.frame(sample_aggregate))
  ) 
  return(sample_dt)
}


sample_dt <- function(dt, 
                      type) {
  
  if (type == 'dir') {
    dt[, sample_EB := pbmcmapply(FUN = sample_dirichlet, 
                                               sample_aggregate = sample, 
                                               proxies = proxies, 
                                               gamma = gamma, 
                                               SIMPLIFY = FALSE, 
                                               mc.cores = 1)]
  } else if (type == '1to1_1') {
    dt[, sample_EB := mapply(FUN = sample_1to1, 
                             sample_aggregate = sample, 
                             proxies = proxies, 
                             SIMPLIFY = FALSE)]
  } else if (type == '1to1_2') {
    dt[, sample_EB := list(mapply(function(sample, target) data.table(industry_code = target, 
                                                                    sample = list(sample)), 
                                target = EXIOBASE_code, 
                                sample = sample, 
                                SIMPLIFY = FALSE))]
  } else {
    stop('type not implemented')
  }
  
  # unnest
  keys <- key(dt)
  dt <- dt[, c(keys, 'sample_EB', 'gamma', 'emissions'), with = FALSE]
  dt <- as.data.table(unnest(dt, cols = 'sample_EB'))
  setkeyv(dt, c(keys, 'industry_code'))
  
  # return results 
  return(dt)
}




############################################################################## # 
##### settings #################################################################
############################################################################## # 
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())

my_scale_fill <-scale_fill_colorblind()
my_cols <- (colorblind_pal()(8))
#scales::show_col(my_cols)

RhpcBLASctl::blas_set_num_threads(config$n_cores)
getDTthreads()
setDTthreads(1)
options(error=recover) 

############################################################################## # 
##### load data #############################################################
############################################################################## # 

dt <- readRDS(file.path(path2output, 'run_optimizer.RData'))
dt

# 1. Subset data ===============================================================
# needed because of memory constraints

dt_dir <- dt[sapply(proxies, is.data.table) & !is.na(gamma)]
dt_1to1_1 <- dt[sapply(proxies, function(x) is.data.table(x) && nrow(x) == 1)]
dt_1to1_2 <- dt[optim_needed == FALSE & !sapply(proxies, is.data.table)]

test_that("Subsets have together same number of rows than original data", {
  expect_equal(nrow(dt), nrow(dt_dir) + nrow(dt_1to1_1) + nrow(dt_1to1_2))
})

# further divide dt_dir into junks of 1000 rows each
seq <- c(seq.int(0, nrow(dt[sapply(proxies, is.data.table) & !is.na(gamma)]), 
                 by = 1000), nrow(dt[sapply(proxies, is.data.table) & !is.na(gamma)]))

dt_dir_list <- lapply(1:(length(seq)-1), function(i) {
  return(dt_dir[(seq[i]+1):seq[i+1]])
})

test_that("Subsets have together same number of rows than original data, 2", {
  expect_equal(nrow(dt_dir), sum(sapply(dt_dir_list, nrow)))
})

rm(dt_dir)
rm(dt)

# 2. sample in for loop ===========================================================
# _a) sample from dirichlet where necessary ====================================

for (i in 1:length(dt_dir_list)) {
  dt_dir_list[[i]] <- sample_dt(dt_dir_list[[i]], 
                                type = 'dir')
  save_results(dt_dir_list[[i]], type = '.feather', suffix = paste0('_dir', i))
  dt_dir_list[[i]] <- 0
  gc()
}

rm(dt_dir_list)

# _b) sample 1:1 correspondences ===============================================
# (correspondences which map in theory to more than one industry, but in practice 
# all but one industry have a zero proxy value)
dt_1to1_1 <- sample_dt(dt_1to1_1, type = '1to1_1')
save_results(dt_1to1_1, type = '.feather', suffix = paste0('_1to11'))
rm(dt_1to1_1)

# c) sample real 1 to 1 correspondences ===============================================
dt_1to1_2 <- sample_dt(dt_1to1_2, type = '1to1_2')
save_results(dt_1to1_2, type = '.feather', suffix = paste0('_1to12'))
rm(dt_1to1_2)



# 2. sample SUT proxies ===========================================================
# dt[sapply(proxies, is.data.table) & is.na(gamma)]
# 
# # _a) sample from dirichlet where necessary ====================================
# 
# 
# # dt[sapply(proxies, is.data.table) & !is.na(gamma)
# #    , sample_EB := mapply(FUN = sample_dirichlet, 
# #                          sample_aggregate = sample, 
# #                          proxies = proxies, 
# #                          gamma = gamma, 
# #                          SIMPLIFY = FALSE)]
# 
# dt[sapply(proxies, is.data.table) & !is.na(gamma)]
# 
# 
# 
# 
# 
# 
# for (i in 1:(length(seqs)-1)) {
#   cat(i,'')
#   dt_subset <- dt[sapply(proxies, is.data.table) & !is.na(gamma)][(seqs[i]+1):seqs[i+1]]
#   dt_subset[, sample_EB := pbmcmapply(FUN = sample_dirichlet, 
#                                       sample_aggregate = sample, 
#                                       proxies = proxies, 
#                                       gamma = gamma, 
#                                       SIMPLIFY = FALSE, 
#                                       mc.cores = 4)]
#   save_results(dt_subset, suffix = i)
#   rm(dt_subset)
# }
# library(arrow)
# 
# dt_subset %>% object.size() %>% format(units = 'MiB')
# dt_subset[, .(country_code, gas, database, category_code, 
#               emissions)] %>% object.size() %>% format(units = 'MiB')
# 
# dt[, sum(sapply(EXIOBASE_code, length))] * 5000 * 56 / (1024^3)
# 
# (object.size(runif(5000)) * dt[, sum(sapply(EXIOBASE_code, length))]) %>% 
#   format(units = 'GiB')
# 
# keys <- key(dt)
# dt2 <- dt_subset[, c(key(dt), 'sample_EB', 'gamma', 'emissions'), with = FALSE]
# dt3 <- unnest_dt(dt2, 'sample_EB')
# dt4 <- as.data.table(unnest(dt2, cols = 'sample_EB'))
# 
# 
# setkeyv(dt4, c(key(dt), 'industry_code'))
# write_feather(dt4, file.path(path2output, 'temp.feather'))
# saveRDS(dt4, file.path(path2output, 'temp.RData'))
# temp <- read_feather(file.path(path2output, 'temp.feather'))
# 
# 
# # _b) sample 1:1 correspondences ===============================================
# # (correspondences which map in theory to more than one industry, but in practice 
# # all but one industry have a zero proxy value)
# dt[sapply(proxies, function(x) is.data.table(x) && nrow(x) == 1)]
# 
# dt[sapply(proxies, function(x) is.data.table(x) && nrow(x) == 1)
#    , sample_EB := mapply(FUN = sample_1to1, 
#                          sample_aggregate = sample, 
#                          proxies = proxies, 
#                          SIMPLIFY = FALSE)]
# 
# 
# # sample 1 to 1 correspondences ===============================================
# dt[optim_needed == FALSE & !sapply(sample_EB, is.data.table)]
# dt[optim_needed == FALSE & !sapply(sample_EB, is.data.table), 
#    sample_EB := list(mapply(function(sample, target) data.table(industry_code = target, 
#                                                                 sample = list(sample)), 
#                             target = EXIOBASE_code, 
#                             sample = sample, 
#                             SIMPLIFY = FALSE))]
# 
# 
# dt[!sapply(sample_EB, is.data.table)]
# 
# 
# 
# # unnest data.table =========================================================
# #unnest_dt(dt, col = 'sample_EB', by = )
# 
# keys <- key(dt)
# dt2 <- dt[, c(key(dt), 'sample_EB', 'gamma', 'emissions'), with = FALSE]
# rm(dt)
# dt2 <- as.data.table(unnest(dt2, cols = 'sample_EB'))
# 
# dt3 <- unnest_dt(dt2, 'sample_EB')
# 
# setkeyv(dt2, c(key(dt), 'industry_code'))
# 
# 
# 
# 
# 
# 
# 
# 
# # Save results ================================================================
# save_results(dt2, suffix = '_detailed')
# 
# 
# 
# 
# 
# 



# THE END ---------------------------------------------------------------------

# junk 
# 1. sample PEFA proxies: =========================================================
# 2 steps: CRF --> NACErev2 --> EXIOBASE 

# # rename columns of proxy data tables
# dt[sapply(proxies_NACErev2, is.data.table)
#    , proxies_NACErev2 := list(lapply(proxies_NACErev2, 
#                                      function(x) setnames(x, c('industry_code', 'share'))))]
# 
# # _a) Step 1: Sample NACErev2 industries ===========================================
# dt[sapply(proxies_NACErev2, is.data.table)
#    , sample_NACE := mapply(FUN = sample_dirichlet, 
#                            sample_aggregate = sample, 
#                            proxies = proxies_NACErev2, 
#                            gamma = gamma, 
#                            SIMPLIFY = FALSE)]
# 
# dt[sapply(proxies_NACErev2, function(x) is.data.table(x) && nrow(x) == 1)]
# # should be empty dt. if not uncomment follwoing lines:
# 
# dt[sapply(proxies_NACErev2, function(x) is.data.table(x) && nrow(x) == 1)
#    , sample_NACE := mapply(FUN = sample_1to1,
#                            sample_aggregate = sample,
#                            proxies = proxies_NACErev2,
#                            SIMPLIFY = FALSE)]


# # _b) Step 2: Sample EXIOBASE ======================================================
# 
# # load correspondence table: NACE --> EXIOBASE
# ct_nace_eb <- readRDS(config$path2CT_NACE_EXIOBASE_parsed)
# ct_nace_eb <- ct_nace_eb[, list(EXIOBASE_code = list(target)), by = source] %>% 
#   as.data.table
# 
# # attach to data.table
# dt[sapply(proxies_NACErev2, is.data.table)
#    , sample_NACE := list(lapply(sample_NACE, function(x) {
#      merge(x, ct_nace_eb, by.x = 'industry_code', by.y = 'source', 
#            all.x = TRUE)
#    }))]
# 
# # uninformative sample EXIOBASE (no proxy data available)
# dt[sapply(proxies_NACErev2, is.data.table)
#    , sample_EB := list(lapply(sample_NACE, function(x) {
#      x[sapply(EXIOBASE_code, function(y) length(y) > 0)
#        , sample_EB := list(mapply(FUN = sample_dirichlet_uninformative, 
#                                   sample = sample, 
#                                   target = EXIOBASE_code, 
#                                   SIMPLIFY = FALSE))]
#      return(rbindlist(x$sample_EB))
#    })) 
# ]

#dt[sapply(proxies_NACErev2, is.data.table)]$sample_EB[[1]]


# test[sapply(EXIOBASE_code, function(x) length(x) > 0)
#      , list(rbindlist(mapply(FUN = sample_dirichlet_uninformative, 
#                              sample = sample, 
#                              target = EXIOBASE_code, 
#                              SIMPLIFY = FALSE)))]
# 
# test[sapply(sample_EB, is.data.table)]$sample_EB %>% rbindlist












