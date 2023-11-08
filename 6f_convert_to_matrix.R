#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-07-25 16:30:08
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
library(countrycode)
library(ggthemes)
library(ggrepel)
library(logr)
library(mRio)
library(arrow)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
source(file.path('src', 'functions_plot.R'))
source(file.path('src', 'functions.R'))

# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())

my_scale_fill <-scale_fill_colorblind()
my_cols <- (colorblind_pal()(8))
scales::show_col(my_cols)
RhpcBLASctl::blas_set_num_threads(config$n_cores)


############################################################################## # 
##### load data #############################################################
############################################################################## # 

# 1. meta data ================

#meta <- parse_EB3_metadata('/home/simon/Documents/PhD_PROSET/data/EXIOBASE3/IOT_1995_ixi')
meta <- parse_EB3_metadata(config$path2exiobaseIOT)
read_EB3_S_meta <- function(path) {
  colnames <- fread(file.path(path), nrows = 2,
                    drop = c(1), header = FALSE) %>%
    t %>%
    as.data.table %>%
    setnames(new = c('region', 'sector'))

  rownames <- fread(file.path(path), skip = 26,
                    select = c(1)) %>%
    setnames(new = c('category'))

  return(list(colnames = colnames, rownames = rownames))

}
indices_S <- read_EB3_S_meta(file.path(config$path2exiobaseIOT, 'satellite', 'F.txt'))
indices_S$colnames[, col := 1:.N]
indices_S$rownames[, row := 1:.N]
indices_S$colnames <- merge(indices_S$colnames, meta$industries, by.x = 'sector', by.y = 'Name',
                            sort = FALSE)
#indices_S$colnames[, region := countrycode(region, origin = 'iso2c', destination = 'iso3c')]

indices_S$colnames$region %>% unique
# 
# # 2. samples ===================================
# #dt3 <- readRDS('./temp_results/5c_EXIOBASE_samples.RData')
# dt3 <- readRDS(file.path(path2output, 'sample_EXIOBASE.RData'))
# 
# dt3[, country_code2 := countrycode(country_code, 'iso3c', 'iso2c')]
# dt3[!(country_code2 %in% (indices_S$colnames$region %>% unique)), 
#     region := countrycode(country_code, 'iso3c', 'region')]
# dt3[(country_code2 %in% (indices_S$colnames$region %>% unique)), 
#     EB_region := country_code2]
# 
# dt3$region %>% unique
# dt3[region %in% c("East Asia & Pacific", "South Asia"), EB_region := 'WA']
# dt3[region %in% c("Latin America & Caribbean", "North America"), EB_region := 'WL']
# dt3[region %in% c("Europe & Central Asia"), EB_region := 'WE']
# dt3[region %in% c("Sub-Saharan Africa"), EB_region := 'WF']
# dt3[region %in% c("Middle East & North Africa"), EB_region := 'WM']
# 
# dt3[is.na(EB_region)]$country_code %>% unique
# dt3$EB_region %>% na.omit %>%  unique %>% length
# # convert to matrix ============= ==============================================
# 

dt4 <- read_feather(file.path(path2output, 
                         'prepare_EXIOBASE_samples_by_industry_and_CRF.feather'), 
                    col_select = c('gas', 'EB_region', 'industry_code', 
                                   'category_code2', 'sample'))
dt4


dt4 <- merge(dt4, indices_S$colnames[, .(region, col, CodeNr)],
             by.x = c('EB_region', 'industry_code'),
             by.y = c('region', 'CodeNr'),
             all.x = TRUE)
dt4[is.na(col)]
#dt4 <- dt4[!is.na(col)] # TODO: include household emissions

row_ids <- dt4[, .(gas, category_code2)] %>% unique
setorder(row_ids, gas, category_code2)
row_ids[, row := 1:.N]
row_ids

#dt4[gas == 'CO2', row := 1]
#dt4[gas == 'CH4', row := 2]
# dt4[gas == 'N2O', row := 3]

dt4 <- merge(dt4, row_ids, by = c('gas', 'category_code2'), 
             all.x = TRUE, sort = FALSE)

dt5 <- dt4[, list(sample = sum_samples(sample)), by = .(row, col)]


rm(dt4)
gc()

# save as sparse matrix in dt format
save_results(dt5, type = '.feather', suffix = '_sparse')


# 
as_dense_matrix_list <- function(x, nrow, ncol, N) {

  array <- array(NA, dim = c(nrow, ncol ,N))
  #list <- vector('list', N)
  #list <- lapply(1:N, function(x) matrix(0, nrow = nrow, ncol = ncol))
  # TODO: make more efficient, avoid for loop
  for (i in 1:nrow(x)) {
    #list
    array[x[i,]$row, x[i,]$col, ] <- unlist(x[i]$sample)
  }
  return(array)
}

Fmat_list <- as_dense_matrix_list(dt5, nrow = dt5$row %>% unique %>% length,
                                  ncol = 7987,
                                  N = config$sample_size)

rm(dt5)
gc()

dim(Fmat_list)

# convert to data.table
#Fmat_dt <- as.data.table(Fmat_list)

#Fmat_list[1:10, 1:10, 1:100]

#Fmat_list[1,,][is.na(Fmat_list[1,,])]
for (i in 1:dim(Fmat_list)[1]) {
  cat(i, '')
  Fmat_list[i,,][is.na(Fmat_list[i,,])] <- 0
  gc()
}

Fmat_list[is.na(Fmat_list)] <- 0

Fmat_list2 <- plyr::alply(Fmat_list, 3)
(Fmat_list2$`1`)[1:3, 1:10]

Fmat_list2[[1]] %>%
  rowSums()
# 
# 
# ############################################################################## # 
# ##### save results #############################################################
# ############################################################################## # 
save_results(Fmat_list2)
save_results(row_ids, suffix = '_rownames')
#save_results_xlsx(Fmat_list2)


#saveRDS(Fmat_list2, './temp_results/5d_F_samples.RData')


# THE END ---------------------------------------------------------------------
