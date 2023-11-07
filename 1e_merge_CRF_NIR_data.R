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
#library(my.utils)
library(data.tree)
library(collapsibleTree)
library(logr)
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
##### load data #############################################################
############################################################################## # 
crf_dt <- readRDS(file.path(path2output, 'repair_CRF_categories.RData'))
nir_dt <- readRDS(file.path(path2output, 'prepare_UNFCCC_uncertainty.RData'))

setkey(crf_dt, year, party, gas, category_code, classification)
setkey(nir_dt, year, party, gas, category_code, classification)

crf_dt[, classification := classification %>% tolower %>% gsub(' ', '_', .)]
setnames(crf_dt, 'value', 'emissions_CRF')

crf_dt <- crf_dt[!grepl('^0', category_code)]

# Remove all combined categories
nir_dt[grepl('\\&', category_code)]
crf_dt[grepl('\\&', id)]
crf_dt[party == 'AUS' & grepl('1.A.', category_code)]
#nir_dt <- nir_dt[!grepl('\\&', category_code)] # TODO: include them

# Normalise classiciations (should not include "/", otherwise problems with data.tree)
nir_dt[grepl('\\/', classification), 
       classification := gsub('\\/', '-', classification)]

crf_dt[grepl('\\/', classification), 
       classification := gsub('\\/', '-', classification)]


# 0. pre select rows ====
crf_dt <- crf_dt[party %in% (nir_dt$party %>% unique)] # parties without uncertainty info


# TODO: continuer here!!
# temp <- crf_dt[classification == 'biomass', .(year, party, gas, category_code)]
# temp[, biomass := 'incl']
# nir_dt <- merge(nir_dt, temp, by = c('year', 'party', 'gas', 'category_code'), all.x = TRUE)
# nir_dt[classification == 'biomass' && biomass != 'incl']

# 1. Add Classification Path (hierarchy of fuel/animal types) =====================================================

# load and normalize data.table with info on paths
class_ct <- readRDS(file.path(path2output, 'create_CT_CRF_classification_table.RData'))
class_ct[grepl('\\/', CRF_class), 
         CRF_class := gsub('\\/', '-', CRF_class)]
class_ct[grepl('\\/', pathString), 
         pathString := gsub('\\/', '-', pathString)]

# NIR
nir_dt <- merge(nir_dt, class_ct[, .(CRF_class, main_class, pathString)], 
                by.x = 'classification', by.y = 'CRF_class', 
                all.x = TRUE, sort = FALSE)
nir_dt[is.na(main_class)]$classification %>% unique # TODO: fix all combined classifcation (animal only)
nir_dt <- nir_dt[!is.na(main_class)] # TODO: 
setnames(nir_dt, 'pathString', 'classification_hierarchy')


# CRF
crf_dt <- merge(crf_dt, class_ct[, .(CRF_class, main_class, pathString)], 
                by.x = 'classification', by.y = 'CRF_class', 
                all.x = TRUE, sort = FALSE)
crf_dt[is.na(main_class)]$classification %>% unique
setnames(crf_dt, 'pathString', 'classification_hierarchy')



# _e) aggregate all NIR classifications that have more detail than CRF =========
# are all classifications from NIR part of CRF data?
# e.g. POL: dairy cattle + non-dairy cattle --> cattle
classes_crf <- unique(crf_dt[, .(year, party, gas, classification)])
classes_crf[, source := 'crf']

classes_nir <- unique(nir_dt[, .(year, party, gas, classification)])
classes_nir[, source := 'nir']

test <- merge(classes_crf, classes_nir, 
              by = c('year', 'party', 'gas', 'classification'),
              all = TRUE)
classes_missing_in_CRF <- test[is.na(source.x), .(year, party, gas, classification)]

if (nrow(classes_missing_in_CRF) > 0) {
  # extract all rows where a classification does not have a CRF correspondence
  setkey(nir_dt, year, party, gas, classification)
  temp <- copy(nir_dt[classes_missing_in_CRF])
  
  # Build classification tree
  temp <- CRF_table2tree2(temp, attributes = c("emissions_NIR", 'cv_NIR', 'sd_NIR'),
                          type = 'classification')
  
  # Aggregate emissions + uncertainty (error propagation)
  temp[, tree := lapply(tree, make_tree_coherent, 
                        emissions = 'emissions_NIR', 
                        sd = 'sd_NIR', 
                        cv = 'cv_NIR')]
  
  # Convert back to data.table
  temp <- CRF_tree2table(temp)
  
  # Only take classifation from level 1 (= cattle)
  # TODO: this is very case specific (POL, cattle)
  temp <- temp[str_count(classification_hierarchy, '\\|') == 1]
  temp[, main_class := classification]
  temp[, isleaf := TRUE]
  
  # Replace rows in nir_dt
  setcolorder(temp, neworder = names(nir_dt))
  nir_dt <- replace_DT_rows(data = nir_dt, 
                            rows = nir_dt[classes_missing_in_CRF, which = TRUE], 
                            new_row = temp)
  
}

# Test again
classes_nir <- unique(nir_dt[, .(year, party, gas, classification)])
classes_nir[, source := 'nir']

test <- merge(classes_crf, classes_nir, 
              by = c('year', 'party', 'gas', 'classification'),
              all = TRUE)
classes_missing_in_CRF <- test[is.na(source.x), .(year, party, gas, classification)]

test_that('NIR and CRF classifications all match', {
  expect_equal(nrow(classes_missing_in_CRF), 0)
})


# _ other fixes ===============================================================

# __ Australia ============================================================
# remove 2.C.7 (other): because NIR has more detail on that level
crf_dt <- crf_dt[!(party == 'AUS' & gas == 'CO2' & grepl('^2.C.', category_code))]

# 2. Category trees (fuel/animal type) ============================

# _a) Build trees (one tree for each category) =================================

nir_dt2 <- CRF_table2tree2(nir_dt, 
                           attributes = c("emissions_NIR", 'cv_NIR', 'sd_NIR'),
                           type = 'category')
crf_dt2 <- CRF_table2tree2(crf_dt, 
                           attributes = c("emissions_CRF"),
                           type = 'category')

setkeyv(nir_dt2, c('year', 'party', 'gas', 'classification', 'classification_hierarchy'))
setkeyv(crf_dt2, c('year', 'party', 'gas', 'classification', 'classification_hierarchy'))

# _b) Make NIR tree coherent (emissions_NIR only) ==============================
nir_dt2[, tree := lapply(tree, 
                         make_tree_coherent, 
                         emissions = 'emissions_NIR', 
                         sd = NULL, cv = NULL)]

# _b) merge NIR and CRF data.tables ==============================================
dt <- merge(nir_dt2, crf_dt2, all = TRUE, suffixes = c('_nir', '_crf'))

# _c) Kick out Biomass where not part of total =================================================

dt[sapply(tree_crf, function(x) {
  if ('Node' %in% class(x)) 'biomass' %in% x$Get('name')
  else NA
})]


# _e) Merge NIR and CRF trees ===============================================================

dt[is.node.list(tree_nir) & is.node.list(tree_crf)
   , merged_tree := list(mapply(merge_trees, 
                                x = tree_nir, 
                                y = tree_crf))]

test_that("again, all classifications of NIR match CRF classificiatno", {
  expect_equal(nrow(dt[!is.node.list(merged_tree) & !is.node.list(tree_crf)]), 0)
})

dt[!is.node.list(merged_tree), merged_tree := tree_crf]




# _f) Disaggregate emissions ===================================================
# TODO:
dt[, tree := list(lapply(merged_tree,
                         disaggregate_emissions3,
                         emissions = 'emissions_CRF',
                         proxy_data = 'emissions_NIR'))]


dt[, tree_crf_height :=  sapply(tree_crf, function(x) x$height)]
dt[is.node.list(tree_nir), 
   tree_nir_height :=  unlist(sapply(tree_nir, function(x) x$height))]
dt[, tree_merged_height := sapply(merged_tree, function(x) x$height)]
dt[tree_nir_height >= tree_crf_height]
dt[tree_merged_height > tree_nir_height]

# now each leaf of the merged trees should have emissions_CRF attribute

dt[, leaves_have_emissions := sapply(tree, function(x) {
  emissions <- x$Get('emissions_CRF', filterFun = isLeaf)
  return(is.numeric(emissions) && all(!is.na(emissions)))
})]
log_print(dt[leaves_have_emissions == FALSE])

# _g) Prune all branches without emissions_CRF =================================

while(nrow(dt[leaves_have_emissions == FALSE]) > 0) {
  dt[
    , tree := list(lapply(tree,
                          function(tree, attribute) {
                            tree$Do(fun = prune_empty_subtrees2, 
                                    attribute = 'emissions_CRF', 
                                    traversal = 'pre-order')
                            return(tree)
                          }))]
  
  dt[, leaves_have_emissions := sapply(tree, function(x) {
    emissions <- x$Get('emissions_CRF', filterFun = isLeaf)
    return(is.numeric(emissions) && all(!is.na(emissions)))
  })]
  log_print(dt[leaves_have_emissions == FALSE])
  
}

# Check coherence =============================================================

non_coherent_trees <- dt[!sapply(tree, 
                                 is_coherent_tree4, 
                                 tolerance = 0.001,
                                 attribute = 'emissions_CRF')]

log_print('Non coherent trees: ')
log_print(non_coherent_trees)


test_that('all trees are coherent (with NA ignore)', {
  expect_equal(nrow(non_coherent_trees), 0)
})


# 3. Mark tree leaves =============================================================
# _a) Mark category leaves (i.e. most detailed category for each year/party/gas combindation) =====

# Convert from category tree to table
dt2 <- CRF_tree2table(copy(dt[, .(year, party, gas, classification, 
                                  classification_hierarchy, tree)]), 
                      'tree', na.omit = FALSE)
dt2 <- dt2[!is.na(emissions_CRF)]

# Convert from table to classification tree
dt3 <- CRF_table2tree2(copy(dt2), attributes = c('emissions_CRF', "emissions_NIR", 
                                                 'cv_NIR', 'sd_NIR'), 
                       type = 'classification')

# Make tree coherent (not sure if needed)
dt3[, tree := lapply(tree, make_tree_coherent, 
                     emissions = 'emissions_CRF', 
                     cv = NULL
)]

# check if all trees are coherent
dt3[!sapply(tree, 
            is_coherent_tree4, 
            attribute = 'emissions_CRF', 
            tolerance = 0.01)]

# Mark all "leaf-categories" (== most detailed categories per party and gas) 
dt3[, is_category_leaf := is_leaf_pathString(id), by = .(year, party, gas)]

# _b) Mark classification leaves (i.e. most detailed classifications for each year/party/gas combindation) =====
# --> but only for category-leaves
dt3[is_category_leaf == TRUE, 
    tree := list(lapply(tree, function(x) {
      x$Do(function(node) {
        if (has_attribute_tree(node, 'emissions_CRF')) {
          node[['is_classification_leaf']] <- TRUE
        } 
      }, filterFun = isLeaf)
      return(x)
    }))]

# Check coherence (again)
non_coherent_trees <- dt3[!sapply(tree, 
                                  is_coherent_tree4, 
                                  attribute = 'emissions_CRF', 
                                  tolerance = 0.001)]

log_print('Non coherent trees: ')
log_print(non_coherent_trees[is_category_leaf == TRUE])

test_that('all trees are coherent (with NA ignore)', {
  #expect_equal(nrow(non_coherent_trees[is_category_leaf == TRUE]), 0)
})
# TODO: some problems for:
# 1:   2015    BGR    CH4  1.A.2.g.viii I.1.A.2.g.viii
# 2:   2015    BGR    CO2  1.A.2.g.viii I.1.A.2.g.viii
# 3:   2015    BGR    N2O  1.A.2.g.viii I.1.A.2.g.viii

# _c) Mark leaf-leaves ========================================================
# leaf-leaf == category-leaf && classification-leaf

# Convert from classification tree to table 
dt4 <- CRF_tree2table(copy(dt3), na.omit = FALSE)

# mark leaf leaves
dt4[is_category_leaf == TRUE & is_classification_leaf == TRUE, 
    is_leaf_leaf := TRUE]

# 4. Prune all subtrees which are already covered by other sub-classifications =======
# i.e. all subtrees which are no 'leaf-leaves'
# esp. relevant for 1.A.3 transport emissions which have more detail (gasoline, diesel)

# convert table to category tree
dt5 <- CRF_table2tree2(copy(dt4), 
                       attributes = c("emissions_CRF", "emissions_NIR", 
                                      'cv_NIR', 'sd_NIR', 'is_leaf_leaf'),
                       type = 'category')

# prune all subtrees already coverd by other sub-classificaitons
dt5[, tree := lapply(tree, prune_empty_subtrees2, 
                     attribute = 'is_leaf_leaf')]

# make coherent again (liquid total --> all liquid emissions wihtout gasoline/diesel other subcategories)
dt5[, tree := lapply(tree, make_tree_coherent,
                     emissions = 'emissions_CRF', 
                     sd = NULL, cv= NULL, 
                     overwrite = TRUE)]

# Convert from category tree to table
dt6 <- CRF_tree2table((dt5), na.omit = FALSE)

# Remove all NA columns
dt6 <- dt6[!(is.na(emissions_CRF) & is.na(emissions_NIR) 
             & is.na(cv_NIR) & is.na(sd_NIR)
             & is.na(is_leaf_leaf))]


############################################################################## # 
##### Tests #############################################################
############################################################################## # 
NATIONAL_TOTALS <- readRDS(file.path(path2output, 'prepare_UNFCCC_CRF_NationalTotals.RData'))

# Compare with national totals

check <- merge(
  dt6[is_leaf_leaf == TRUE, 
      list(emissions = sum(emissions_CRF, na.rm = TRUE)), 
      by = .(year, party, gas)], 
  NATIONAL_TOTALS[, .(year, party, gas, value)], 
  by = c('year', 'gas', 'party'), 
  all = TRUE
)
check[, absdif := emissions - value]
check[, reldif := absdif / value]
log_print('Tests')
log_print("Summary of abs and rel differences between sum of all leaves and national totals")
log_print(summary(check$absdif))
log_print(summary(check$reldif))

log_print('parties with a relative difference of more than 1%')
log_print(check[abs(reldif) > 0.01])

# TODO: problems with Swedish inventory!!! (esp co2, reldif of 0.15!)

# Which parties got lost on the way?
log_print("ANNEX 1 Parties that got lost: ")
log_print(check[is.na(emissions), list(gas = paste(gas, collapse = ',')), 
                by = .(party, year)])

# Tests
test_that('all good', {
  expect_equal(na.omit(check)$emissions, (na.omit(check)$value), 
               tolerance = 0.01)
})

############################################################################## # 
##### save results #############################################################
############################################################################## # 

# Prepare for save
dt6 <- dt6[, .(year, party, gas, category_code, id, classification, 
               classification_hierarchy, emissions_CRF, emissions_NIR, 
               cv_NIR, sd_NIR, is_leaf_leaf)]

# save
save_results(x = dt6, type = '.RData')
log_close()
# The end ======================================================================


