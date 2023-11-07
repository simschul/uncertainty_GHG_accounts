#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-06-23 12:58:14
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
library(data.tree)
library(collapsibleTree)
library(testthat)


library(logr)


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

ggplot2::reset_theme_settings()

############################################################################## # 
##### settings #################################################################
############################################################################## # 

N <- config$sample_size

############################################################################## # 
##### load data #################################################################
############################################################################## # 

dt <- readRDS(file.path(path2output, 'merge_CRF_NIR_data.RData'))

dt[!is.na(cv_NIR) & classification == 'total_for_category'] %>% 
  ggplot(aes(x = gas, y = cv_NIR)) + 
  scale_y_log10() +
  geom_boxplot()  
### workaround: handle missing uncertainty estimates
### calculate average CV for each CRF category

dt_cvmean <- dt[, list(cv_DEFAULT = mean(cv_NIR, na.rm = TRUE)), 
                by = .(gas, category_code)] %>%
  na.omit 

dt <- merge(dt, dt_cvmean, by = c('gas', 'category_code'),
      all.x = TRUE, sort = FALSE)
#dt[is.na(cv_DEFAULT)]$category_code %>% unique
# dt_cvmean %>%
#   ggplot(aes(y = category_code, x = cv_mean)) +
#   geom_point() +
#   facet_wrap(~gas, nrow = 1, scales = 'free') +
#   theme_bw()

# end workaround



dt[is.na(emissions_CRF) & is.na(cv_NIR) & is.na(sd_NIR) & is.na(is_leaf_leaf)]

dt[!is.na(sd_NIR), dist := 'truncnorm']




# Problem: For DEU CO2 the current implementation does not work because: 
# Uncertainty data is only available for Total for category, but not by fuel
# type. 
# the below implementation works for these cases. 
# But does NOT work for other cases, where unceratinty data is avaialbel 
# by classification (e.g. AUS CH4, but probably most others)

#dt <- dt[party == 'DEU' & grepl('^1.A.1', category_code) & gas == 'CO2']
# dt[is_leaf_leaf == TRUE]
# # create Node Path from Category and Classification
# dt[, id2 := create_master_id_crf(id, classification_hierarchy)]
# 
# # select only: (1) Leaves = most detailed levels, (2) Nodes with uncertainty info
# dt <- dt[is_leaf_leaf == TRUE | !is.na(cv_NIR)]
# 
# # convert to tree by country, gas and year
# test <- CRF_table2tree2(dt, attributes = c('emissions_CRF', 'emissions_NIR', 
#                                            'cv_NIR', 'sd_NIR', 'is_leaf_leaf', 
#                                            'dist'), 
#                         type = 'combined')
# 
# tree <- test$tree[[1]]
# tree <- make_tree_coherent(tree, emissions = 'emissions_CRF')
# tree$Do(function(node) {
#   if (has_attribute_tree(node, 'cv_NIR') & has_attribute_tree(node, 'emissions_CRF')) {
#     node[['sd_CRF']] <- node$cv_NIR * node$emissions_CRF
#   }
# })
# sd <- 'sd_CRF'
# emissions <- 'emissions_CRF'
# N = 5
# sample = 'sample'
# 
# #tree <- propagate_sd(tree, sd = sd)
# tree <- sample_nodes_with_sd(tree, emissions = emissions, 
#                              sd = sd, sample = sample, N = N)
# tree <- sample_nodes_disaggregate(tree, emissions = emissions, 
#                                   sd = sd, sample = sample, N = N)
# tree <- sample_nodes_missing(tree, emissions = emissions, 
#                              sd = sd, sample = sample, N = N)
# tree <- aggregate_sample(tree, sample = sample)
# collapsibleTree2(tree, collapsed = FALSE, 
#                  attributes = c('emissions_CRF', 'sd_CRF', 'is_leaf_leaf', 'sample'), 
#                  fillby = 'sd_CRF',nodeSize = 'emissions_CRF', nodeSize_log = TRUE)
# 
# tree %>% convert_tree_to_dt()
# 
# is_coherent_tree(tree, 'sample', tol= 1E-12)
# 

# end test

# _a) Build category tree  =====================================================

dt <- CRF_table2tree2(dt, attributes = c('emissions_CRF', 'emissions_NIR', 
                                         'cv_NIR', 'sd_NIR', 'is_leaf_leaf', 
                                         'dist', 'cv_DEFAULT'), 
                      type = 'category')

#dt[5]$tree[[1]] %>% collapsibleTree2(attributes = c('cv_NIR', 'emissions_CRF'), collapsed = FALSE)

# _b) Calculate sd_CRF = cv_NIR * emissions_CRF ================================
dt[, tree := list(lapply(tree, 
                         function(tree) {
                           tree$Do(function(node) {
                             if (has_attribute_tree(node, 'cv_NIR') 
                                 & has_attribute_tree(node, 'emissions_CRF')) {
                               node[['sd_CRF']] <- node$cv_NIR * node$emissions_CRF
                             }
                             if (has_attribute_tree(node, 'cv_DEFAULT') &
                                 has_attribute_tree(node, 'emissions_CRF')) {
                               node[['sd_DEFAULT']] <- node$cv_DEFAULT * node$emissions_CRF
                             }
                           })
                           return(tree)
                         }))
]


# test <- dt[party == 'AUS' & gas == 'CO2' & classification == 'liquid_fuels']$tree[[1]]
# test %>% 
#   convert_tree_to_dt()
# is_coherent_tree4(test, 'emissions_CRF')

# _h) **Sample nodes** =============================================================
# TODO: incl: setkey(dt) in `CRF_tabel2tree2`.. functions

{
  pb <- txtProgressBar(min = 0, 
                       max = nrow(dt), 
                       style = 3)
  dt[, tree := {setTxtProgressBar(pb, .GRP); 
    list(lapply(tree, function(x) {
      tryCatch(sample_tree(tree = x, N = N, sd = 'sd_CRF', sd_default = 'sd_DEFAULT'), 
               error = function(e) print(e))
    } ))}, 
    by = key(dt)]
  close(pb)
}

#dt$tree[[1]] %>% convert_tree_to_dt()


# _i) check coherence ===========================================================
log_print('Not coherent trees: ')

non_coherent_trees <- dt[
  sapply(dt$tree,
         function(x) ifelse(
           "Node" %in% class(x), 
           !is_coherent_tree(x, attribute = 'sample', tol = 1E-6), 
           FALSE
         ))   
] 

log_print(non_coherent_trees)
test_that('all trees are coherent (with NA ignore)', {
  expect_equal(nrow(non_coherent_trees), 0)
})

# _j) Convert tree back to data.table ==========================================

dt <- CRF_tree2table(dt, na.omit = FALSE)
#dt[!sapply(tree, is.node)]$party %>% unique
#dt$tree[[1]] %>% convert_tree_to_dt()


test_that('all items where sampled', {
  expect_equal(nrow(dt[is.empty.list(sample)]), 0)
})


# _______ Tests _____________ ===================================================

sep('Tests')
# No zero emissions in samples
log_print("Summary statistics of all samples after sampling classification trees:")
log_print(dt$sample %>% unlist %>% summary)

test_that("there are no negative samples",{
  expect_true(min(unlist(dt$sample), na.rm = TRUE) >= 0)
})

# Compare with national totals
leaves <- dt[is_leaf_leaf == TRUE,
             list(sample_leaves = (sum_samples(sample)), 
                  emissions_CRF_leaves = sum(emissions_CRF, na.rm = TRUE)), 
             by = .(year, party, gas)]
leaves[, means_leaves := sapply(sample_leaves, mean)]

NATIONAL_TOTALS <- readRDS(file.path(path2output, 'prepare_UNFCCC_CRF_NationalTotals.RData'))

check <- merge(
  leaves, 
  NATIONAL_TOTALS[, .(year, party, gas, value)], 
  by = c('year', 'gas', 'party'), 
  all = TRUE
)
check[, absdif := emissions_CRF_leaves - value]
check[, reldif := absdif / value]
log_print("Summary of abs and rel differences between sum of all leaves and national totals")
log_print(summary(check$absdif))
log_print(summary(check$reldif))

log_print('parties with a relative difference of more than 1%')
log_print(check[abs(reldif) > 0.01])

# Mean samples should be similar enough to 'real' emissions
leaves[, absdif := means_leaves - emissions_CRF_leaves]
leaves[, reldif := absdif / emissions_CRF_leaves]

log_print("Summary of abs and rel differences between 'real' emissions and sample means")
log_print(summary(leaves$absdif))
log_print(summary(leaves$reldif))

log_print('parties with a relative difference of more than 1%')
log_print(leaves[abs(reldif) > 0.1])



# _______End Test _____________ ================================================



# _k) Extract Leaves only =====================================================

dt <- dt[is_leaf_leaf == TRUE]

# _l) Get rid of '&'-categories again =========================================
test <- dt[grepl('\\&', category_code), ]$category_code


dt[grepl('\\&', category_code), 
   category_code := remove_helper_CRF_categories(category_code)]

dt[grepl('\\&', id), 
   id := remove_helper_CRF_categories(id)] 


# clean data.table


############################################################################## # 
##### save results #############################################################
############################################################################## # 

save_results(x = dt[,.(year, party, gas, category_code, 
                       classification, emissions_CRF, 
                       sd_CRF, sample)], 
             type = '.RData')
log_close()

# the end
