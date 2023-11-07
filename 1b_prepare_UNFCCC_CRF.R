#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-01-21 09:03:11
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
library(countrycode)
library(data.tree)
library(collapsibleTree)
library(paletteer)
library(logr)
library(testthat)

############################################################################## # 
##### general settings #########################################################
############################################################################## # 

source(file.path('src', 'functions.R'))
source(file.path('src', 'functions_trees.R'))


# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output

############################################################################## # 
##### settings #########################################################
############################################################################## # 

path2data <- config$path2UNFCCC_emissions


path2ct = file.path(path2output, 'create_CT_CRF_classification_table.RData')
gases <- config$gases
years <- config$year

parties <- c("AUS", "AUT", "BLR", "BEL", "BGR", "CAN", "HRV", 
             "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", 
             "ISL", "IRL", "ITA", "JPN", "KAZ", "LVA", "LIE", "LTU", "LUX", 
             "MLT", "MCO", "NLD", "NZL", "NOR", "POL", "PRT", "ROU", "RUS", 
             "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "UKR", "GBR", "USA")

# temp <- parse_UNFCCC_country_data(path2data, party = 'AUS',
#                           years = c('2015'),
#                           gases = c('CH4', 'N2O', 'CO2'))




############################################################################## # 
##### functions #########################################################
############################################################################## # 

data <- arrow::read_parquet(file.path('/home/simon/Documents/PhD_PROSET/data/UNFCCC/data-2020-10-25/data/annexI', 
                                      paste0('DEU', '.parquet'))) %>% 
  as.data.table

# path <- '/home/simon/Documents/PhD_PROSET/data/UNFCCC/data-2020-10-25/data/annexI'
# gases <- c('CO2', 'CH4', 'N2O')
# years <- c('2015')
# party <- 'DEU'
# LULUCF <- FALSE
# path2ct = './temp_results/0a_CT_CRF-NIR_classification_table.RData'

parse_UNFCCC_country_data <- function(
    path, party, years, gases, LULUCF = FALSE,
    path2ct, #= './temp_results/0a_CT_CRF-NIR_classification_table.RData', 
    international_bunkers = TRUE
) {
  data <- arrow::read_parquet(file.path(path, paste0(party, '.parquet'))) %>% 
    as.data.table
  data <- data[, gas := chartr("₀₁₂₃₄₅₆₇₈₉", "0123456789", gas)] %>% 
    # select relevant gases
    .[gas %in% gases] %>% 
    # select relevant year
    .[year %in% years] %>% 
    # # only net emissions (leave out emission factors, AD, etc.)
    .[measure == 'Net emissions/removals'
      | (measure == 'Indirect emissions' # see email from Johannes Guetschow, 13.10.2022
         & category == '3.B  Manure Management')] %>%
    # exclude Biomass (not included in totals, assumed to have zero emissions)
    #.[classification != 'Biomass'] %>%
    # throw out 1.AB and 1.AD to avoid double counting (relevant total is 1.AA)
    .[!(category %in% c('1.AB  Fuel Combustion - Reference Approach',
                        '1.AD  Feedstocks, Reductants and Other Non-energy Use of Fuels'))] 
  if (isFALSE(LULUCF)) {
    # exclude LULUCF
    data <- data[!grepl('^4.', category)]  
  }
  
  # Exclude CO2 emissions from 5.C.1.a and 5.C.2.a Biogenic Waste Incineration / open burning (not included in Totals)
  # data <- data[!grepl('^5.C.1.a', category) | (party == 'JPN' & gas == 'N2O')]
  data <- data[!(grepl('^5.C.1.a', category) & gas == 'CO2')]
  data <- data[!(grepl('^5.C.2.a', category) & gas == 'CO2')]
  
  # Include 3.B. Indirect N2O emissions into tree strucutre (see mail from Guetschow, above)
  data[measure == 'Indirect emissions'
       , `:=`(classification = 'Indirect emissions')]
  
  # Split category column into two
  data <- cbind(data, split_UNFCCC_category_column(data$category))
  
  # Normalise classiciations (should not include "/", otherwise problems with data.tree)
  data[, classification := normalize_UNFCCC_classfications(classification)]
  
  if (isTRUE(international_bunkers)) {
    # International bunker emissions
    data[category == 'International Aviation'
         , `:=`(category_code = '0.A', 
                category_name = category)]
    data[category == 'International Navigation'
         , `:=`(category_code = '0.B', 
                category_name = category)]  
  }
  
  # Remove unnecessary rows
  data <- data[!(category_name == '' 
                 & category_code != 'Total GHG emissions without LULUCF')]
  data[category_name == '', 
       `:=`(category_code = '', 
            category_name = 'Total GHG emissions without LULUCF')]
  
  data[category_code == '1.AA', category_code := '1.A']
  
  # set units
  data[, numberValue := set_units(numberValue, Gg)]
  
  # throw out unecessary columns
  data[,category := NULL]
  data[,unit := NULL]
  data[,stringValue := NULL]
  data[,measure := NULL]
  
  # set column names and col order
  setnames(data, 'numberValue', 'value')
  setcolorder(data, c('party', 'year', 'gas', 
                      'category_code', 'category_name', 
                      'classification', 'value'))
  
  # country-specific corrections
  if (TRUE) {
    if (party == 'BLR') {
      # do correction for Belarus emission data:
      log_print('undertaking correction\t')
      data <- data[!(party == "BLR" & category_code %in% c("1.A.3.b.v"))]
    }
    
    if (party == 'JPN') {
      # do correction for japenese emission data:
      # log_print('undertaking correction\t')
      # 
      # data <- data[!(party == "JPN" & category_code %in% c("2.B.4.a"))]
      # #data <- data[!(party == "JPN" & gas == "CO2" & category_code %in% c("5.C.1.a"))]
      #data <- data[!(party == "JPN" & gas == "CO2" & category_code %in% c("5.C.1.b"))]
    }
    
    if (party == 'SWE' & '2015' %in% years) {
      # Swedish 2015 emission have no 'total for category' item for categories 1.A.2.b&d&e
      # (in the UNFCCC data interface these categories are marked as Confidential)
      # --> create total
      # log_print('undertaking correction\t')
      # categories_affected <- c('1.A.2.b','1.A.2.d', '1.A.2.e')    
      # temp <- data[category_code %in% categories_affected 
      #              & classification != 'total_for_category'
      #              & year == '2015'
      #              , list(value = sum(value, na.rm = TRUE), 
      #                     classification = 'total_for_category')
      #              , by = .(party, year, gas, category_code, category_name)]
      # 
      # # remove original totals (NA rows) 
      # data <- data[!(category_code %in% categories_affected 
      #                & classification == 'total_for_category'
      #                & year == '2015')]
      # data <- rbindlist(list(data, temp), use.names = TRUE)
      
      
    }
    
    if (party == 'SWE' & years %in% as.character(2015:2020)) {
      # 1.B.2.a Oil is not coherent (remainder very likely in 1.B.2.a.iv)
      # --> remove sub categories
      # data <- data[!(party == 'SWE' & category_code == '1.B.2.a.iii')]
    }
    
    if (party == 'KAZ') {
      # 1.A.2 biomass is not coherent
      # --> remove sub categories
      data <- data[!(party == 'KAZ' & grepl('^1.A.2.', category_code))]
    }
  }
  
  
  
  # create id variable (later used to construct data.tree from data)
  data[!(category_code %in% c('0.A', '0.B')), id := paste0("I.", category_code)]
  data[(category_code %in% c('0.A', '0.B')), id := category_code]
  data[category_code == '', id := 'I']
  
  # add classfiication hierachy
  class_hierarchy <- readRDS(path2ct)
  class_hierarchy[, classification := normalize_UNFCCC_classfications(CRF_class)]
  class_hierarchy[, classification_hierarchy := normalize_UNFCCC_classfications(pathString)]
  data <- merge(data, class_hierarchy[, .(classification, classification_hierarchy)], 
                by = 'classification',
                all.x = TRUE, sort = FALSE)
  
  # remove all biomass emission that are not included in totals
  data <- exclude_biomass(data)
  
  # test ===================================================================== =
  # test_results <- test_coherence_UNFCCC_hierarchy(data)
  # 
  # test_results[[1]][, `:=`(category_code = NULL)]
  # test_results[[2]][, `:=`(id = 'I', 
  #                          classification = NULL, 
  #                          classification_hierarchy = NULL)]
  # test_results2 <- rbindlist(test_results, use.names = TRUE)
  # test_results2[, test := 'failed']
  # 
  # data <- merge(data, test_results2, 
  #               by = c('year', 'party', 'gas', 'id'), 
  #               all.x = TRUE, sort = FALSE)
  # data[is.na(test), test := 'passed']
  #data[is.empty.list(tree), test := 'passed']
  #data[!is.empty.list(tree), test := 'failed']
  
  # end test ================================================================= =
  
  data[, value := drop_units(value)]
  setorderv(data, c('year', 'category_code', 'classification'))
  setcolorder(data, c('year', 'party', 'gas', 'category_code', 'category_name', 'classification'))
  cat('\n')
  return(data[])
}




#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
split_UNFCCC_category_column <- function(x) {
  # make seperate cols for code and name of categories
  xnew <- as.data.table(str_split_fixed(x, '  ', n = 2))
  setnames(xnew, c('category_code', 'category_name')) 
  
  # category codes should never end with '.' (e.g. 1., 5.)
  xnew[suppressWarnings(substr_right(category_code, 1)) == ".", 
       category_code := substr(category_code, 1, nchar(category_code) - 1)]
  
  return(xnew[])  
}


# biomass_included(test1)
# convert_tree_to_dt(test1)
# 
# sapply(data2$tree[[121]][[1]], biomass_included, tol = 0.1)
# biomass_included(data2$tree[[121]])





test_coherence_UNFCCC_hierarchy <- function(x) {
  # exclude rows that not covered in totals
  x <- x[(grepl('^I', id))] 
  # exclude NA's in value
  x <- x[!is.na(value)]
  
  tree_class <- create_UNFCCC_classification_tree(x)
  tree_cat <- create_UNFCCC_category_tree(x[classification == 'total_for_category'])
  
  not_coherent_class <- tree_class[!sapply(tree, is_coherent_tree2, attribute = 'value')]
  not_coherent_cat <- tree_cat[!sapply(tree, is_coherent_tree2, attribute = 'value')]
  
  return(list(not_coherent_class, not_coherent_cat))
  
  # if (nrow(not_coherent_class) == 0 & nrow(not_coherent_cat) == 0) return(TRUE)
  # else {
  #   return(list(not_coherent_class, not_coherent_cat))
  #   # if (nrow(not_coherent_class) > 0 & nrow(not_coherent_cat) > 0) {
  # }
  # if (nrow(not_coherent_class) > 0) {
  #   return(not_coherent_class)
  #   # warning(paste('Incoherent tree for category', 
  #   #               not_coherent_class$category_code, 
  #   #               '(gas:', not_coherent_class$gas,
  #   #               'party:', not_coherent_class$party,
  #   #               ')')) 
  # }
  # if (nrow(not_coherent_cat) > 0) {
  #   return(not_coherent_cat)
  #         # warning(paste('Incoherent tree for classification', 
  #   #               not_coherent_cat$classification,
  #   #               '(gas:', not_coherent_class$gas, 
  #               'party:', not_coherent_class$party,
  #               ')'))
  #} 
  #return(FALSE)
  # }
}




is_coherent_tree2 <- function(tree, attribute, tolerance = 0.01) {
  value_leaves <- tree$Get(attribute, filterFun = isLeaf)
  sum_leaves <- sum(value_leaves, na.rm = TRUE)
  value_root <- as.numeric(tree$Get(attribute, filterFun = isRoot))
  return(isTRUE(all.equal(sum_leaves, value_root, tolerance = tolerance)))
}

# tree <- temp
# attribute <- 'value'
# tolerance <- 0.01
is_coherent_tree3 <- function(tree, attribute, tolerance = 0.01) {
  tree$Do(function(node) {
    node$sum_agg <- Aggregate(node, attribute, aggFun = sum, na.rm = TRUE)
    node$test <- ifelse(isTRUE(all.equal(node[[attribute]], node[['sum_agg']])), 
                        'passed', 'failed')
  })
  tree %>% convert_tree_to_dt()
}



#' Reconcile a tree. 
#' 
#' Makes a tree coherent in a way that the value of Root node is preserved and
#' all other nodes a scaled so that they sum up to the Root.
#'
#' @param tree 
#' @param value_var 
#' @param copy if False (default) no copy is made (all modifications happen on the base root), set to TRUE to avoid weird behaviour
#'
#' @return
#' @export
#'
#' @examples
#' 
reconcile_tree <- function(tree, value_var = 'value', copy = FALSE) {
  #value_var <- 'value'
  if (isTRUE(copy))  tree <- Clone(tree)
  total_true <- tree$Get(value_var, filterFun = isRoot)
  total_sum <- tree$Get(function(node) Aggregate(node, value_var, aggFun = sum), 
                        filterFun = isRoot)
  
  if (isTRUE(all.equal(total_true, total_sum))) {
    print('here')
    return(tree)
  } else {
    tree$Do(function(node) {
      node[[value_var]] <- total_true * (node[[value_var]] / total_sum)
    }, filterFun = isNotRoot)  
  }
  return(tree)
}


parse_UNFCCC_data <- function(
    path, 
    parties = 'all', 
    years, 
    gases, 
    LULUCF = FALSE,
    international_bunkers = TRUE,
    path2ct, #= './temp_results/0a_CT_CRF-NIR_classification_table.RData', 
    make_coherent = TRUE, 
    tolerance = 0.05
    
){
  
  # if parties set to 'all', look for which countries data is available
  if (parties == 'all') {
    parties <- str_split(list.files(path2data, pattern = '.parquet$'), 
                         '\\.', simplify = TRUE)[,1]
  }
  
  # parse data for each countries
  data_list <- lapply(parties, function(x) {
    log_print(paste0('Party: ', x), blank_after = TRUE)
    data <- parse_UNFCCC_country_data(path2data, party = x, 
                                      years = years, 
                                      gases = gases, 
                                      LULUCF = LULUCF, 
                                      international_bunkers = international_bunkers,
                                      path2ct = path2ct)
    # make data coherent if required
    if (isTRUE(make_coherent)) {
      data <- make_CRF_tree_coherent(data, tolerance = tolerance)
    }
    
    return(na.omit(data))
  })
  
  # bind one large data.table 
  data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  # set row order
  setorder(data, year, party, id, gas, classification_hierarchy)
  
  return(data)
  
}




make_CRF_tree_coherent <- function(x, tolerance = 0.05) {
  categories <- unique(x[, .(category_name, id)])
  
  # exclude rows that not covered in totals
  x_rest <- x[!(grepl('^I', id))]
  x <- x[(grepl('^I', id))] 
  # exclude NA's in value
  x <- x[!is.na(value)]
  # drop units
  #x <- x[, value := drop_units(value)]
  
  # classification tree ==========================================================
  x <- CRF_table2tree(x, type = 'classification')
  #(fails1 <- x[!sapply(tree, is_coherent_tree, attribute = 'value', tol = tolerance)])
  #visualize_coherence(fails1$tree[[4]], attribute = 'value')
  
  # make coherent = add missing nodes (through aggregation)
  x[, tree := lapply(tree, make_tree_coherent, 
                     emissions = 'value', 
                     overwrite = FALSE)]
  #(fails1 <- x[!sapply(tree, is_coherent_tree, attribute = 'value', tol = tolerance)])
  
  # mark incoherent subtrees
  x[, tree := lapply(tree, mark_incoherent_subtrees, 
                     attribute = 'value', 
                     prune_empty_subtrees = FALSE)]
  #(fails1 <- x[!sapply(tree, is_coherent_tree, attribute = 'value', tol = tolerance)])
  #visualize_coherence(fails1$tree[[1]], attribute = 'value')
  
  #(fails1b <- x[!sapply(tree, is_coherent_tree, attribute = 'value', tol = tolerance)])
  
  # back to data.table
  x <- CRF_tree2table(x, simplify = TRUE)
  
  # kick out problematic sub categories
  match <- unique(x[coherent == FALSE]$category_code) 
  if (length(match) > 0) {
    log_print(paste0('kick out all sub categories below: ', paste(match, collapse = ', ') ), 
              blank_after = TRUE)
    match <- match %>%  
      paste0('(^', .) %>% 
      paste0('.)') %>% 
      paste(collapse = '|')
    x <- x[!str_detect(category_code, pattern = match)]
  }
  
  
  # category tree ================================================================
  x <- CRF_table2tree(x, type = 'category')
  #(fails1 <- x[!sapply(tree, is_coherent_tree4, attribute = 'value', tol = tolerance)])
  #visualize_coherence(fails1$tree[[1]], attribute = 'value')
  
  # make coherent = add missing nodes (through aggregation), only: Totals
  x[sapply(str_split(classification_hierarchy, '\\|'), length) <= 1, 
    tree := lapply(tree, make_tree_coherent,
                   emissions = 'value',
                   overwrite = FALSE)]
  
  #(fails1 <- x[!sapply(tree, is_coherent_tree, attribute = 'value', tol = tolerance)])
  
  # mark incoherent subtrees
  x[, tree := lapply(tree, mark_incoherent_subtrees, 
                     attribute = 'value', 
                     prune_empty_subtrees = FALSE)]
  
  
  # back to data.table
  x <- CRF_tree2table(x, simplify = TRUE)
  
  # kick out problematic sub categories
  match <- unique(x[coherent == FALSE]$category_code) 
  if (length(match) > 0) {
    log_print(paste0('kick out all sub categories below: ', paste(match, collapse = ', ') ), 
              blank_after = TRUE)
    match <- match %>%  
      paste0('(^', .) %>% 
      paste0('.)') %>% 
      paste(collapse = '|')
    x <- x[!str_detect(category_code, pattern = match)]
  }
  # tests =========================================
  
  x <- CRF_table2tree(x, type = 'classification')
  test_that('all classification trees are coherent', {
    expect_true(all(x[, sapply(tree, is_coherent_tree, 
                               attribute = 'value', tol = tolerance)]))
  })
  
  
  x <- CRF_tree2table(x, simplify = TRUE)
  x <- CRF_table2tree(x, type = 'category')
  
  test_that('all category trees are coherent', {
    expect_true(all(x[, sapply(tree, is_coherent_tree4, 
                               attribute = 'value', tol = tolerance)]))
  })
  
  # convert back to data.table
  x <- CRF_tree2table(x, simplify = TRUE)
  # attach category names (which somehow got lost)
  x <- merge(x, categories, by = 'id', all.x = TRUE)
  # attach emissions from int. aviation/shipping
  x <- rbindlist(list(x, x_rest), use.names = TRUE)
  
  return(x)
}

############################################################################## # 
##### load data #############################################################
############################################################################## # 

# 1. Parse UNFCCC CRF data -----------------------------------------------------
parties <- 'all'
data <- parse_UNFCCC_data(path = path2data, years = years, 
                          path2ct = path2ct,
                          parties = parties, gases = gases, 
                          make_coherent = TRUE, 
                          international_bunkers = FALSE)


# # Tests ========================================================================
# data[category_code %in% c('0.A', '0.B')]
# 
# log_print("Cases with incoherent CRF hierarchy (category or classification):", 
#           blank_after = FALSE)
# log_print(data[test == 'failed'])
# 
# 
# # 1. correct categories where test failed ==================================
# trees_failed <- create_UNFCCC_classification_tree(data[test == 'failed' & id != 'I'], 
#                                                   by = c('year', 'party', 'gas', 
#                                                          'category_code', 'category_name', 'id'))
# trees_failed[, tree_reconciled := list(lapply(tree, function(x) {
#   reconcile_tree(x, copy = TRUE)
# }))]
# trees_failed[!sapply(tree_reconciled, is_coherent_tree2, attribute = 'value')]
# 
# x <- copy(trees_failed)
# x[, data := list(lapply(tree_reconciled, convert_tree_to_dt, 
#                         attributes = c('value', 'name'),
#                         levelName = FALSE,
#                         pathString = TRUE))]
# x <- x[, unlist(data, recursive = FALSE), 
#        by = .(year, party, gas, category_code, category_name, id)]
# x[, `:=`(value = set_units(unlist(value), Gg),
#          name = unlist(name),
#          pathString = pathString %>% 
#            unlist %>% 
#            gsub('\\/', '\\|', .))]
# setnames(x, c('pathString', 'name'), c('classification_hierarchy', 
#                                        'classification'))
# 
# 
# x[, test := 'fixed (upscaled)']
# log_print("Correct incoherent CRF hierarchy for:", 
#           blank_after = FALSE)
# log_print(x)
# 
# # 2. Replace =======================================================
# data2 <- rbindlist(list(data[!(test == 'failed' & id != 'I')], x), 
#                    use.names = TRUE, fill = TRUE)
# setorderv(data2, c('year', 'party', 'category_code', 'classification'))
# 
# 
# UNFCCC_as_table <- function(x, attributes = 'value') {
#   # TODO
# }

# Remove units ================================================================
# TODO: data.tree does not handle units very well (make use of sapply a lot --> units get lost)
#data2[, value := drop_units(value)]


############################################################################## # 
##### save results #############################################################
############################################################################## # 
save_results(x = data, type = '.RData')
save_results(x = data[id == 'I' & classification == 'total_for_category', 
                      .(year, party, gas, category_code, category_name, 
                        classification, value)], 
             type = '.RData', 
             suffix = '_NationalTotals')

log_close()


#data <- readRDS('./temp_results/UNFCCC_datawithclassification_cleaned.RData')


# THE END ---------------------------------------------------------------------
