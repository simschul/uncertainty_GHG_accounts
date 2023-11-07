#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2023-10-25 09:52:55.678906
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
# library(hdf5r)
library(HiClimR)
library(ggthemes)
library(mRio)
library(arrow)
############################################################################## # 
##### settings #################################################################
############################################################################## # 

source(file.path('src', 'functions.R'))
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())

n_cores <- config$n_cores
RhpcBLASctl::blas_set_num_threads(1)
RhpcBLASctl::blas_get_num_procs()
############################################################################## # 
##### functions #################################################################
############################################################################## # 
reshape_data <- function(x) {
  if (inherits(x, "dgeMatrix")) {
    x <- as.matrix(x)
  }
  x <- as.data.table(x)
  x <- melt(x[, i := .I], 
            id.vars = "i", 
            variable.name = 'j')
  x <- x[abs(value) > 0]
  x[, j := as.integer(j)]
  return(x[])
}

create_dense_matrix <- function(dt, nrow, ncol, i) {
  m <- matrix(0, nrow = nrow, ncol = ncol)
  m[cbind(as.factor(dt$row), as.factor(dt$col))] <- dt[, sapply(sample, '[', i)]
  return(m)
}

create_dense_matrix2 <- function(dt, row='row', col='col', value='value', 
                                 nrow, ncol) {
  m <- matrix(0, nrow = nrow, ncol = ncol)
  m[cbind(as.factor(dt[[row]]), as.factor(dt[[col]]))] <- dt[[value]]
  return(m)
}

############################################################################## # 
##### load data #############################################################
############################################################################## # 
# meta data
F_rownames <- readRDS(file.path(path2output, 'convert_to_matrix_rownames.RData'))
meta <- parse_EB3_metadata(config$path2exiobaseIOT)
indices_S <- read_EB3_S_meta(file.path(config$path2exiobaseIOT, 'satellite', 'F.txt'))
indices_S$colnames[, col := 1:.N]
indices_S$rownames[, row := 1:.N]
indices_S$colnames <- merge(indices_S$colnames, meta$industries, by.x = 'sector', by.y = 'Name',
                            sort = FALSE)



# samples
f_list <- readRDS(file.path(path2output, 'convert_to_matrix.RData'))

f_dt <- readRDS(file.path(path2output, 'prepare_EXIOBASE_samples_by_industry_and_CRF.RData'))

# convert mean + cv to matrix
f_dt2 <- merge(f_dt, indices_S$colnames[, .(region, col, CodeNr)],
             by.x = c('EB_region', 'industry_code'),
             by.y = c('region', 'CodeNr'),
             all.x = TRUE)
f_dt2 <- merge(f_dt2, F_rownames, by = c('gas', 'category_code2'), 
               all.x = TRUE, sort = FALSE)

f_summary_dt <- f_dt2[, .(row, col, mean, median, cv)]

f_mean <- create_dense_matrix2(f_summary_dt, value= 'mean', 
                               nrow = nrow(F_rownames), ncol = 7987)
dim(f_mean)
f_mean[1:10, 1:10]

f_cv <- create_dense_matrix2(f_summary_dt, value= 'cv', 
                               nrow = nrow(F_rownames), ncol = 7987)
f_cv[1:10, 1:10]

f_median <- create_dense_matrix2(f_summary_dt, value= 'median', 
                               nrow = nrow(F_rownames), ncol = 7987)
dim(f_median)
f_median[1:10, 1:10]


# create directories to save data
path2zenodo <-file.path(path2output, 'zenodo')
path2samples <- file.path(path2zenodo, 'samples')
dir.create(path2zenodo)
dir.create(path2samples)


# save F matrices
lapply(1:length(f_list), function(i){
  save_results(as.data.table(f_list[[i]]), 
               path = path2samples, 
               filename = paste0('F_', i), 
               type = '.feather')
  return(NULL)
})
# zip
zip(paste0(path2samples, '.zip'), 
    list.files(path2samples, full.names = TRUE))


# save mean and CV matrices

write_feather(as.data.table(f_mean), file.path(path2zenodo, 'F_mean.feather'))
write_feather(as.data.table(f_median), file.path(path2zenodo, 'F_median.feather'))
write_feather(as.data.table(f_cv), file.path(path2zenodo, 'F_cv.feather'))



# save F meta data
setnames(F_rownames, c('category_code2', 'row'), c('IPCC_category_2006_2019', 'id'))
F_rownames[, unit := 'Gg']
fwrite(F_rownames[, .(id, gas, IPCC_category_2006_2019, unit)], 
       file = file.path(path2zenodo, 'index_rows.csv'))

F_colnames <- meta$indices_A$colnames
F_colnames[, id := 1:.N]
fwrite(F_colnames, file = file.path(path2zenodo, 'index_cols.csv'))

# calculate correlations

dt <- lapply(f_list, reshape_data) %>% 
  rbindlist(idcol = 'run')
dt[, run := as.integer(run)]

dt <- merge(dt, F_rownames[, .(gas, row)], by.x = 'i', by.y = 'row', 
            sort = FALSE)
dt2 <- dt[, list(value = sum(value)), by = .(run, j, gas)]

dt_list <- split(dt2, by = 'gas')
dt_list <- lapply(dt_list, function(x) dcast(x, run ~ j, value.var = 'value'))

# system.time({
#   test <- fastCor(dt3[,2:100], optBLAS = TRUE, upperTri = TRUE, nSplit = 3)  
# })

system.time({
  cormat_list <- lapply(dt_list, function(x) cor(x[,-'run']))  
})




cormat2sparse <- function(x) {
  x[upper.tri(x, diag = TRUE)] <- NA
  return(reshape_data(x))
}

system.time({
  cormat_list2 <- lapply(cormat_list, cormat2sparse)  
})
lapply(cormat_list2, summary)

cormat_dt <- rbindlist(cormat_list2, idcol = 'gas')
setnames(cormat_dt, 'value', 'pearson_correlation_coefficient')



# save
write_feather(cormat_dt, file.path(path2zenodo, 'correlation_table.feather'))


cormat_dt[value < 0]
# plot
cormat_dt[abs(value) > 0.25] %>% 
  ggplot(aes( fill=gas, col = gas, x = value)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_colorblind() + 
  scale_color_colorblind()



library(corrplot)
corrplot.mixed(cormat[1:10, 1:10], upper = 'ellipse', order = 'AOE', 
               lower = 'number')


# trying out hdf5 ========================================================

test_file <- tempfile(fileext=".h5")
file.h5 <- H5File$new(test_file, mode="w")

data(cars)
file.h5$create_group("test")
file.h5[["test/cars"]] <- cars
cars_ds <- file.h5[["test/cars"]]
h5attr(cars_ds, "rownames") <- rownames(cars)

## Close the file at the end
## the 'close' method closes only the file-id, but leaves object inside the file open
## This may prevent re-opening of the file. 'close_all' closes the file and all objects in it
file.h5$close_all()


############################################################################## # 
##### save results #############################################################
############################################################################## # 
install.packages(
  'hdf5r',
  configure.args = '--with-hdf5=/home/simon/anaconda3/bin/h5cc'
)

install.packages(
  'hdf5r',
  configure.args = 'with-hdf5',
  configure.vars = '[/home/linuxbrew/.linuxbrew/bin/h5cc]'
)
install.packages('hdf5r')
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("rhdf5")
Sys.getenv()
install.packages("hdf5r", configure.args="--with-hdf5=/usr/local/hdf5/bin/h5cc")
install.packages("hdf5r", configure.args="--with-hdf5=/usr/bin/h5cc")

install_github(repo = 'hhoeflin/hdf5r')


"/usr/lib/x86_64-linux-gnu/hdf5/serial/lib"
"/usr/lib/x86_64-linux-gnu/hdf5"

"/home/linuxbrew/.linuxbrew/Cellar/hdf5/1.14.2"

system('which h5cc')

/home/simon/anaconda3/bin/h5cc

system('export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5.so:/usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5_hl.so')


# THE END ---------------------------------------------------------------------













