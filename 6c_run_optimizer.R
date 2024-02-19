#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-07-19 15:47:26
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
# library(mRio)
library(testthat)
library(logr)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source(file.path('src', 'functions.R'))
source(file.path('src', 'functions_mRio.R'))


# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())

path2samples <- file.path(path2output, 'residence_adjustment_ROAD_sample.RData')
#path2samples <- './temp_results/7_UNFCCC_EDGAR_samples_combined.RData'

############################################################################## # 
##### functions #################################################################
############################################################################## # 

source(file.path('src', 'functions_dirichlet.R'))

# sample_aggregate <- dt$sample[[1]]
# proxies <- dt$proxies[[1]]
# N <- 100


find_gamma_maxent3 <- function(proxies, eval_f = eval_f, ...) {
  if (nrow(proxies) == 1) return(NA)
  tryCatch(
    find_gamma_maxent2(shares = proxies$share, eval_f = eval_f, ...), 
    error = function(e) print(e)
  )
  
}


############################################################################## # 
##### load data #############################################################
############################################################################## # 

dt <- readRDS(path2samples)

dt$EXIOBASE_code %>%
  unlist %>% 
  unique %>% 
  sort

# determine which rows need optimisation (= all that have proxies with a length of > 1)
dt[, optim_needed := sapply(proxies, function(x) is.data.table(x) && nrow(x) > 1)]

# # optimisation happens either in 2 steps 
# # (in case of road transport emissions for EU countries which have PEFA data)
# # or in 1 step (all other)
# dt[sapply(proxies, is.data.table) & sapply(proxies_NACErev2, is.data.table), 
#    optim_steps := 2]
# dt[sapply(proxies, is.data.table) & !sapply(proxies_NACErev2, is.data.table), 
#    optim_steps := 1]

dt[optim_needed == FALSE & is.na(EXIOBASE_code)]
setkey(dt, year, country_code, gas, database, category_code, classification)

############################################################################## #
# 1. run optimizer (direct correspondences: CRF --> EB) ==========================
############################################################################## # 
# _a) first run (standard=strict settings) ================================================================

{
  pb <- txtProgressBar(min = 0, 
                       max = nrow(dt[sapply(proxies, is.data.table)]), 
                       style = 3)
  dt[sapply(proxies, is.data.table), 
     nloptr := {
       setTxtProgressBar(pb, .GRP); list(mapply(FUN = find_gamma_maxent3, 
                                                #shares = proxies,
                                                proxies = proxies,
                                                MoreArgs = list(eval_f = eval_f, 
                                                                bounds = c(0.001, 300)),
                                                SIMPLIFY = FALSE))
     }
     , by = key(dt)]
  
  close(pb)  
}

# <simpleError in find_gamma_maxent2(shares = proxies$share, eval_f = eval_f, ...): Error: Could not find an initial value x0 which is defined by eval_f and/or eval_grad_f. Either increase x0_n_tries (defaul: 100), or increase the parameter space with the bounds argument>


# _b) Rerun with non-converged proxies using less strict settings ============== 
# 1. kick out very small shares
# 2. increase bounds where to look for opt solution


dt[sapply(nloptr, function(x) 'error' %in% class(x))]

{
  pb <- txtProgressBar(min = 0, 
                       max = nrow(dt[sapply(nloptr, function(x) 'error' %in% class(x))]), 
                       style = 3)
  dt[sapply(nloptr, function(x) 'error' %in% class(x)),
     nloptr := {
       setTxtProgressBar(pb, .GRP); list(mapply(FUN = find_gamma_maxent3, 
                                                #shares = proxies,
                                                proxies = proxies,
                                                MoreArgs = list(eval_f = eval_f, 
                                                                bounds = c(0.001, 500), 
                                                                shares_lb = 1E-3),
                                                SIMPLIFY = FALSE))
     }
     , by = key(dt)]
  
  close(pb)  
}

## all ran smoothly now??
test_that("all optims converged", {
  expect_equal(0, nrow(dt[sapply(nloptr, function(x) 'error' %in% class(x))]))
})





# Extract solution ==============================================================

dt[sapply(nloptr, function(x) 'nloptr' %in% class(x))
   , gamma := map_dbl(nloptr, function(x) x$solution)]




############################################################################## # 
##### Tests #############################################################
############################################################################## # 
#dt[optim_needed == TRUE & is.na(gamma) && sapply(proxies, function(x) nrow(x) > 1)]

#dt[sapply(proxies, is.data.table) && sapply(proxies, function(x) nrow(x)) > 1]


# TODO: check is all fitted gammas are there
all(unlist(dt[is.na(gamma), sapply(proxies, nrow)]) == 1)


test_that("all 1 to N concordances have fitted gamma", {
  expect_equal(0, 
               nrow(dt[is.na(gamma) 
                       & optim_needed == TRUE 
                       & sapply(proxies, function(x) is.data.table(x) && nrow(x) > 1)])
  )  
})


# save results =================================================================
save_results(dt)
#saveRDS(dt, './temp_results/7b_UNFCCC&EDGAR_samples_with_EXIOBASE_proxies_and_gamma.RData')



# THE END ---------------------------------------------------------------------

# ############################################################################## # 
# # 2. run optimizer (indirect correspondences via NACE: CRF --> NACE) ===========
# ############################################################################## # 
# 
# # _a) CRF --> NACE =============================================================
# {
#   pb <- txtProgressBar(min = 0, 
#                        max = nrow(dt[sapply(proxies_NACErev2, is.data.table)]), 
#                        style = 3)
#   dt[sapply(proxies_NACErev2, is.data.table), 
#      nloptr := {
#        setTxtProgressBar(pb, .GRP); list(mapply(FUN = find_gamma_maxent3, 
#                                                 proxies = proxies_NACErev2,
#                                                 MoreArgs = list(eval_f = eval_f),
#                                                 SIMPLIFY = FALSE))
#      }
#      , by = key(dt)]
#   
#   close(pb)
# }
# 
# 
# # _b) NACE --> EB ==============================================================
# 
# test1 <- dt[optim_steps == 2]$proxies[[1]]
# test2 <- dt[optim_steps == 2]$proxies_NACErev2[[1]]
# 


