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
library(rriskDistributions)


############################################################################## # 
##### functions #########################################################
############################################################################## # 

source(file.path('src', 'functions.R'))
source(file.path('src', 'functions_trees.R'))
source(file.path('src', 'functions_dirichlet.R'))

############################################################################## # 
##### general settings #########################################################
############################################################################## # 

# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output

N <- config$sample_size
theme_set(theme_bw())
############################################################################## # 
##### 1. load data #############################################################
############################################################################## # 

data_em <- readRDS(file.path(path2output, 'parse_EDGAR_emissions2015_parsed.RData'))
data_un <- readRDS(file.path(path2output, 'parse_EDGAR_uncertainty.RData'))

############################################################################## # 
##### 3. merge both #############################################################
############################################################################## # 


data_em[, id := paste0('I.', category_code)]
data_un[, id := paste0('I.', category_code)]

# Create trees by country for both datasets 
data_em2 <- data_em[, list(tree = list(
  list(
    emissions = emissions,
    pathString = id
  ) %>%
    as.data.table %>%
    as.Node(.,
            pathDelimiter = ".")
)), by = .(country_code, gas, year)]
#data_em2$tree[[1]] %>% convert_tree_to_dt()

data_un2 <- data_un[, list(tree = list(
  list(
    emissions_un = emissions,
    dist = dist,
    cv = cv,
    meanlog = meanlog, 
    sdlog = sdlog,
    pathString = id
  ) %>%
    as.data.table %>%
    as.Node(.,
            pathDelimiter = ".")
)), by = .(country_code, gas)]
#data_un2$tree[[1]] %>% convert_tree_to_dt()

# Aggregate emissions
data_em2[, tree := list(lapply(tree, 
                               make_tree_coherent, 
                               emissions = 'emissions', 
                               overwrite = FALSE))]

# Merge data.tables 
data <- merge(data_em2, data_un2, by = c('country_code', 'gas'), 
              suffixes = c('_em', '_un'), 
              all = TRUE)
data[!sapply(tree_em, function(x) 'Node' %in% class(x))] # should be empty
data[!sapply(tree_un, function(x) 'Node' %in% class(x))] # should be empty

# throw out the Isle of Man (anyway should be part of UK??)
data <- data[country_code != 'IMN']

# merge trees
data[, tree := list(mapply(merge_trees, 
                           x = tree_em, 
                           y = tree_un))]
#convert_tree_to_dt(data$tree[[1]])




# calculate SD from CV
# TODO
data[
  , tree := list(lapply(tree, 
                        function(tree) {
                          tree$Do(function(node) {
                            if (has_attribute_tree(node, 'cv') & has_attribute_tree(node, 'emissions')) {
                              node[['sd']] <- node$cv * node$emissions
                            }
                            
                          })
                          return(tree)
                        }))
]

#data$tree[[1]] %>% convert_tree_to_dt()


#temp <- data.table::copy(data)
#data <- data.table::copy(temp)
#data <- data[country_code == 'CHN' & gas == 'CO2']
#saveRDS(data, './tmp/temp.RData')


# sample ======================================================================
{
  pb <- txtProgressBar(min = 0, 
                     max = nrow(data[is.node.list(tree)]), 
                     style = 3)

data[
  , tree2 := {setTxtProgressBar(pb, .GRP); 
    list(lapply(tree, function(x) {
      tryCatch(sample_tree(x, N = N, emissions = 'emissions', 
                   sd = 'sd', meanlog = 'meanlog', sdlog = 'sdlog'),  
               error = function(e) print(e))
    } ))}, 
  by = country_code]
close(pb)
}






# _c) extract leaves (classifications) for each category ====================== 
data[, data := list(lapply(tree2, convert_tree_to_dt, 
                         attributes = c('emissions','sd', 'meanlog', 'sdlog', 
                                        'dist', 'sample', 
                                        'gamma'),
                         filterFun = isLeaf,
                         levelName = FALSE,
                         pathString = TRUE))]

# warnings: In FUN(X[[i]], ...) : different class for same attribute
data$data[[1]]


# _d) Convert trees to data.table columns =====================================
data2 <- data[, unlist(data, recursive = FALSE), 
           by = .(year, country_code, gas)]


data2[, category_code := pathString %>% 
      gsub('I\\/', '', .) %>% 
      gsub('\\/', '.', .)] 

data2$sample %>% unlist %>% summary

# clean data.table
# data2[, emissions := set_units(unlist(emissions), Gg)]
# data2[, sd := set_units(unlist(sd), Gg)]
data2[, emissions := unlist(emissions)]
data2[, sd := unlist(sd)]
data2[, gamma := unlist(gamma)]
data2[, dist := unlist(dist)]

data2[sample(1:nrow(data2), 9)] %>% 
  unnest(col = 'sample') %>% 
  as.data.table %>%  
  ggplot(aes(x = sample)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = emissions)) + 
  geom_vline(aes(xintercept = emissions + sd), linetype = 'dotted') + 
  geom_vline(aes(xintercept = emissions - sd), linetype = 'dotted') + 
  facet_wrap(~country_code + gas, scales = 'free')

p <- data2[, sum(emissions), by = dist] %>% 
  ggplot(aes(x = dist, y = V1)) + 
  geom_col()
save_plot(plot = p, suffix = '_distribution_occurence')


# Save results =================================================================
# saveRDS(data2[, .(year, country_code, gas, category_code, emissions, 
#                   sd, sdlog, meanlog, dist, sample)], 
#         './temp_results/3b_EDGAR_samples.RData')
save_results(data2[, .(year, country_code, gas, category_code, emissions, 
                       sd, sdlog, meanlog, dist, sample)])

# data$tree2[[100]] %>% convert_tree_to_dt()
# 
# 
# 
# collapsibleTree2(data$tree2[[1]], 
#                  attributes = c('emissions', 'sample'), 
#                  nodeSize = 'emissions',
#                  nodeSize_log = TRUE,
#                  fillby = 'rel.unc.max',
#                  collapsed = FALSE
# )


















