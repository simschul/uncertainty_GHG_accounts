#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-05-23 10:37:50
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
library(logr)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source('./src/functions.R')

setDTthreads(threads = 5)
# read config and setup log script
config <- setup_config_and_log()

path2output <- config$path2output
path2eb <- config$path2exiobaseSUT
############################################################################## # 
##### load data #############################################################
############################################################################## # 

sut <- parse_EB3_SUT(path2eb, Y = TRUE, va = FALSE, metadata = TRUE, path2meta = NULL)

supply <- sut$V
use <- sut$U
fd <- sut$Y

dim(supply)
dim(use)
dim(fd)


attr(supply, 'rownames') <- merge(attr(supply, 'rownames'), sut$metadata$products, 
                                  by.x = 'sector', by.y = 'Name', sort = FALSE)
attr(supply, 'colnames') <- merge(attr(supply, 'colnames'), sut$metadata$industries, 
                                  by.x = 'sector', by.y = 'Name', sort = FALSE)

attr(use, 'rownames') <- merge(attr(use, 'rownames'), sut$metadata$products, 
                               by.x = 'sector', by.y = 'Name', sort = FALSE)
attr(use, 'colnames') <- merge(attr(use, 'colnames'), sut$metadata$industries, 
                               by.x = 'sector', by.y = 'Name', sort = FALSE)

attr(fd, 'rownames') <- merge(attr(fd, 'rownames'), sut$metadata$products, 
                              by.x = 'sector', by.y = 'Name', sort = FALSE)
attr(fd, 'colnames') <- merge(sut$metadata$indices_Y$colnames, sut$metadata$finaldemands, 
                              by.x = 'category', by.y = 'Name', 
                              sort = FALSE, all.x = TRUE)

attributes(fd)


#sut$metadata$industries %>% view_excel()


# meta <- list()
# meta$supply$colnames <- fread(file.path(path2eb, 'supply.csv'), nrows = 2, 
#                               drop = c(1,2)) %>% t %>% as.data.table
# meta$supply$rownames <- fread(file.path(path2eb, 'supply.csv'), skip = 3,
#                               select = c(1,2)) 
# 
# meta$use$colnames <- fread(file.path(path2eb, 'use.csv'), nrows = 2, 
#                            drop = c(1,2)) %>% t %>% as.data.table
# meta$use$rownames <- fread(file.path(path2eb, 'use.csv'), skip = 3,
#                            select = c(1,2)) 
# 
# temp <- meta$use$rownames[grepl('Basic iron', V2), which = TRUE]
# 
# cbind(meta$use$rownames, EB3_metadata$colnames200[, .(country_code1, product200_name, product200_code)]) %>% 
#   view_excel()
# 
# temp$V2 %>% unique
# use[temp,] %>% rowSums()
# 

colnames(supply) <- NULL
colnames(use) <- NULL
colnames(fd) <- NULL

supply <- as.sparse.matrix(supply, 
                           rownames = attr(supply, 'rownames'), 
                           colnames = attr(supply, 'colnames'))
supply <- supply[value > 0]

use <- as.sparse.matrix(use, 
                        rownames = attr(use, 'rownames'), 
                        colnames = attr(use, 'colnames'))
use <- use[value > 0]

fd <- as.sparse.matrix(fd, 
                       rownames = attr(fd, 'rownames'), 
                       colnames = attr(fd, 'colnames'))
fd <- fd[value > 0]

# combine USE and FInal demand tables =========================================
setnames(fd, 'sector', 'sector.row')
setnames(fd, 'category', 'sector.col')
use2 <- rbindlist(list(use, fd), use.names = TRUE)
#use2$CodeNr.col %>% unique

# --> use2 covers the USE of all products in industries (163) and final demand categories 

# change column names ==========================================================

setnames(use2, 
         c('region.row', 'CodeNr.row', 'sector.row'), 
         c('country_product', 'product_code', 'product_name'))

setnames(use2, 
         c('region.col', 'CodeNr.col', 'sector.col'), 
         c('country_industry', 'industry_code', 'industry_name'))

setnames(supply, 
         c('region.row', 'CodeNr.row', 'sector.row'), 
         c('country_product', 'product_code', 'product_name'))

setnames(supply, 
         c('region.col', 'CodeNr.col', 'sector.col'), 
         c('country_industry', 'industry_code', 'industry_name'))

use2 <- use2[, .(row, col, country_product, product_code, product_name, 
               country_industry, industry_code, industry_name, value)]
supply <- supply[, .(row, col, country_product, product_code, product_name, 
                     country_industry, industry_code, industry_name, value)]

# Sum over Row Regions =========================================================
# it only matters how much of product j is used in industry i in country a (not where j comes from)  
use2 <- use2[, list(value = sum(value)), 
           by = .(country_industry, industry_code, industry_name, 
                  product_code, product_name)]



# save results =================================================================
save_results(supply, suffix = '_supply')
save_results(use2, suffix = '_use')


# THE END ---------------------------------------------------------------------
