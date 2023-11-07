#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2023-01-17 11:30:47
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
library(data.tree)
library(testthat)

############################################################################## # 
##### load functions ############################################################
############################################################################## # 

source(file.path('src', 'functions.R'))
source(file.path('src', 'functions_dirichlet.R'))
source(file.path('src', 'functions_trees.R'))


############################################################################## # 
##### general settings #########################################################
############################################################################## # 

# read config and setup log script
config <- setup_config_and_log()

path2output <- config$path2output

############################################################################## # 
##### settings #################################################################
############################################################################## # 

N <- config$sample_size

############################################################################## # 
##### load data #################################################################
############################################################################## # 
crf_dt <- readRDS(file.path(path2output, 'prepare_UNFCCC_CRF.RData'))
nir_dt <- readRDS(file.path(path2output, 'prepare_UNFCCC_uncertainty.RData'))

fix_needed <- nir_dt[grepl('\\&', category_code)]
log_print(fix_needed[, .(year, party, gas, category_code)] %>% unique)

fix_needed$category_code %>% unique

# 1. Fix category codes ========================================================

# AUS  CH4/CO2/N2O  1.A.2&4&5 =========================================
crf_dt[
  party == 'AUS' 
  & grepl('^1.A.(2|4|5)', category_code) 
  & !grepl('\\&', category_code)
  , id := str_replace(id, '^I.1.A', 'I.1.A.2&4&5')
]
# AUT  CH4/CO2/N2O  1.A.1&1.A.2&1.A.4&1.A.5.a =========================================
crf_dt[
  party == 'AUT' 
  & grepl('^1.A.(1|2|4)', category_code)
  & !grepl('\\&', category_code)
  , id := str_replace(id, '^I.1.A', 'I.1.A.1&2&4')
]


# CAN  CH4/CO2/N2O 1.B.2.a&b =========================================
crf_dt[
  party == 'CAN' 
  & grepl('^1.B.2.(a|b)', category_code) 
  & !grepl('\\&', category_code)
  , id := str_replace(id, '^I.1.B.2', 'I.1.B.2.a&b')
]


# CHE    N2O         3.D.1.a&b&c&d&e&g =========================================
crf_dt[
  party == 'CHE' & gas == 'N2O'
  & grepl('^3.D.1.(a|b|c|d|e|g)', category_code) 
  & !grepl('\\&', category_code)
  , id := str_replace(id, '^I.3.D.1', 'I.3.D.1.a&b&c&d&e&g')
]

# GBR    CH4/N2O/CO2               1.A.1&2&4&5 =========================================
crf_dt[
  party == 'GBR' 
  & gas %in% c("CH4", 'N2O', 'CO2')
  & grepl('^1.A.(1|2|4|5)', category_code) 
  & !grepl('\\&', category_code)
  , id := str_replace(id, '^I.1.A', 'I.1.A.1&2&4&5')
]
# GBR    CH4                 1.B.2.a&c =========================================
crf_dt[
  party == 'GBR' & gas == 'CH4' 
  & grepl('^1.B.2.(a|c)', category_code) 
  & !grepl('\\&', category_code)
  , id := str_replace(id, '^I.1.B.2', 'I.1.B.2.a&c')
]

# NLD    CO2           1.A.4.a&1.A.4.b ========================================
crf_dt[
  party == 'NLD' & gas == 'CO2' 
  & grepl('^1.A.4.(a|b)', category_code) 
  & !grepl('\\&', category_code)
  , id := str_replace(id, '^I.1.A.4', 'I.1.A.4.a&b')
]

# Check ids visually ==========================================================
crf_dt[grepl('\\&', id)]$id

# update category_codes =======================================================
crf_dt[grepl('\\&', id), category_code := gsub('I\\.', '', id)]
crf_dt$category_code %>% unique


# fill all "&" categories through aggregation ============================
crf_dt[grepl('\\&', id)]

# create cateogry tree
crf_tree <- CRF_table2tree(crf_dt, type = 'category')

# fill "&"-categories, but only for classifications: totals + one level further (solid, liquid, gas)
# more detail causes problems (e.g. with gasoline/diesel)
crf_tree[
  sapply(str_split(classification_hierarchy, '\\|'), length) <= 2
  , tree := list(lapply(tree, function(tree) {
  tree$Do(function(node) {
    print(node$pathString)
    node$value <-Aggregate(node, 'value', aggFun = function(x) sum(x, na.rm = TRUE))
  }, filterFun = function(node) grepl('\\&', node$name))
  return(tree)
}))]


############################################################################## # 
##### Tests #############################################################
############################################################################## # 
test_that("category trees are coherent", {
  expect_true(all(crf_tree[,sapply(tree, is_coherent_tree4, attribute = 'value', tol = 0.01)]))
})

############################################################################## # 
##### save results #############################################################
############################################################################## # 
# back to data.table
crf_dt2 <- CRF_tree2table(crf_tree)
crf_dt2[grepl('\\&', id)]

save_results(crf_dt2, type = '.RData')

# THE END ---------------------------------------------------------------------
