#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2023-04-25 09:48:52
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
library(testthat)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source('./src/functions.R')

setDTthreads(threads = 5)
# read config and setup log script
config <- setup_config_and_log()

path2output <- config$path2output
path2eb <- config$path2exiobaseIOT
############################################################################## # 
##### functions #################################################################
############################################################################## # 


############################################################################## # 
##### load data #############################################################
############################################################################## # 

# f matrix
Fmat <- read_EB3_F(file.path(path2eb, 'satellite', 'F.txt'))

# meta data
F_meta <- read_EB3_F_meta(file.path(path2eb, 'satellite', 'F.txt'))
meta <- parse_EB3_metadata(path2eb)
F_meta$rownames[, id := 1:.N]

F_meta$colnames <- merge(F_meta$colnames, meta$industries, 
                         by.x = 'sector', by.y = 'Name', 
                         sort = FALSE)


# first inspection
Fmat[1:10, 1:10]
dim(Fmat)

# 1. Extract Employment hours from F mat =======================================

# get row numbers
ids <- F_meta$rownames[grepl("Employment hours", category)]
ids <- ids[grepl('male', category)]
ids <- ids$id

# extract rows
empl <- colSums(Fmat[ids, ])

# bind data table
dt <- cbind(
  F_meta$colnames[, .(region, CodeNr)], 
  employment_hours = empl
)
setnames(dt, 'CodeNr', 'industry_code')



# 2. calculate shares per country ==============================================
# _a) employment as secondary proxy (only to further split industries) =========

dt[, share := employment_hours / sum(employment_hours),
     by = region]
dt <- dt[share > 0]

proxies_secondary <- dt[, list(proxies_empl = list(data.table(
  industry_code, 
  share
))), by = region]

# _b) employment as primary proxy (to also split industries/houyseholds) =========

# load mean industry/household split accross all EU countries from PEFA 
mean_split <- readRDS(file.path(path2output, 
                                'prepare_PEFA_proxies_mean_ind-hous_split.RData'))

dt[, CO2 := mean_split[gas == 'CO2' & type == 'industry']$share]
dt[, CH4 := mean_split[gas == 'CH4' & type == 'industry']$share]
dt[, N2O := mean_split[gas == 'N2O' & type == 'industry']$share]

dt[, CO2 := share * CO2]
dt[, CH4 := share * CH4]
dt[, N2O := share * N2O]

households <- data.table(
  region = dt$region %>% unique, 
  industry_code = 'y01', 
  CO2 = mean_split[gas == 'CO2' & type == 'households']$share, 
  CH4 = mean_split[gas == 'CH4' & type == 'households']$share, 
  N2O = mean_split[gas == 'N2O' & type == 'households']$share
  
)

proxies_primary <- rbindlist(list(dt, households), fill = TRUE)
proxies_primary <- melt(proxies_primary, id.vars = c('region', 'industry_code'), 
     measure.vars = config$gases, variable.factor = FALSE, 
     variable.name = 'gas', value.name = 'share')

test_that("Primary proxies sum to one", {
 expect_equal(0, var(proxies_primary[, sum(share), by = .(region, gas)]$V1))  
})

proxies_primary <- proxies_primary[, list(proxies_empl = list(data.table(
  industry_code, 
  share
))), by = .(region, gas)]


############################################################################## # 
##### save results #############################################################
############################################################################## # 
save_results(proxies_secondary, suffix = '_secondary')
save_results(proxies_primary, suffix = '_primary')

# THE END ---------------------------------------------------------------------














