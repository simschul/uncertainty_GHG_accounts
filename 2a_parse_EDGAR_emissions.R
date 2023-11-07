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
library(rio)
library(data.tree)
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

############################################################################## # 
##### settings #################################################################
############################################################################## # 
path2edgar <- config$path2edgar 
iyear <- config$year

############################################################################## # 
##### functions #########################################################
############################################################################## # 


############################################################################## # 
##### 1. load emissions data #############################################################
############################################################################## # 
path_list <- list()

(path_list$CO2 <- list.files(path2edgar, pattern = '(CO2_excl).*?(.xls)$', 
                             full.names = TRUE, recursive = TRUE))
(path_list$CH4 <- list.files(path2edgar, pattern = '(CH4).*?(.xls)$', 
                             full.names = TRUE, recursive = TRUE))
(path_list$N2O <- list.files(path2edgar, pattern = '(N2O).*?(.xls)$', 
                             full.names = TRUE, recursive = TRUE))

data_em_raw <- lapply(path_list, function(x) {
  rio::import(x, which = 1, skip = 9, col_names = TRUE, 
              na = c('NA', 'NULL')) %>% 
    as.data.table
})
data_em_raw$CO2


data_em <- lapply(data_em_raw, function(x){ 
  melt(x,id.vars = colnames(x)[1:6], 
       variable.name = 'year', value.name = 'emissions', 
       variable.factor = FALSE )
  
})

data_em <- rbindlist(data_em, idcol = 'gas', use.names = TRUE)


# set units
#data_em[, emissions := set_units(emissions, Mt)]
data_em[, emissions := set_units(emissions, Gg)]

# set names
setnames(data_em, c('ISO_A3', 
                    'Name', 
                    'IPCC', 
                    'IPCC_description'), 
         c('country_code', 'country_name', 'category_code', 'category_name'))
names(data_em)

# remove NAs
data_em <- na.omit(data_em)

# remove LULUCF
data_em[grepl('^5', category_code)]

setkey(data_em, country_code, category_code, gas, year)

# Make EDGAR 2015 coherent =================================================================
data2015 <- data_em[year == iyear]
data2015[, id := paste0('I.', category_code)]

trees <- data2015[,
           list(tree = list(
             list(
               emissions = emissions,
               pathString = id
             ) %>%
               as.data.table %>%
               as.Node(.,
                       pathDelimiter = ".")
           ))
           , by = .(country_code, country_name, gas, year)]
trees$tree[[1]] %>% convert_tree_to_dt()

trees[, tree := lapply(
  tree, 
  FUN = make_tree_coherent, 
  emissions = 'emissions', 
  cv = NULL, 
  sd = NULL, 
  overwrite = FALSE
)]


trees[, data := list(lapply(tree, convert_tree_to_dt, 
                           attributes = c('emissions'),
                           #filterFun = isLeaf,
                           levelName = FALSE,
                           pathString = TRUE))]

# Convert trees to data.table columns 
data_coherent <- trees[, unlist(data, recursive = FALSE), 
              by = .(year, country_code, country_name, gas)]


data_coherent[, category_code := pathString %>% 
        gsub('I\\/', '', .) %>% 
        gsub('\\/', '.', .)] 
data_coherent[, pathString := NULL]

data_coherent[, emissions := set_units(unlist(emissions), Gg)]


# Save results =================================================================
save_results(x = data_em)
save_results(x = data_em[year == iyear], suffix = paste0(config$year, '_parsed'))
save_results(x = data_coherent, suffix = '_coherent')
# saveRDS(data_em, './temp_results/EDGARv50_parsed.RData')
# saveRDS(data_em[year == iyear], 
#         paste0('./temp_results/EDGARv50_', iyear, '_parsed.RData'))

# saveRDS(data_coherent, paste0('./temp_results/EDGARv50_', iyear, '_coherent.RData'))
# The end


















