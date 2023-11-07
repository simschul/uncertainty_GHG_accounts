#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-09-12 14:23:28
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
library(pbmcapply)
library(testthat)
library(svMisc)
library('RPostgreSQL')

############################################################################## # 
##### settings #################################################################
############################################################################## # 

source(file.path('src', 'functions.R'))
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())

n_cores <- config$n_cores
RhpcBLASctl::blas_set_num_threads(4)
RhpcBLASctl::blas_get_num_procs()


############################################################################## # 
##### functions #############################################################
############################################################################## # 

calculate_S <- function(E, x) {
  # calculate Stressor matrix
  # x_hat <- diag(1/x)
  # x_hat[is.infinite(x_hat)] <- 0
  # S <- E %*% x_hat
  S <- Rfast::eachrow(E, x,'/')
  S[!is.finite(S)] <- 0
  return(S)
}

############################################################################## # 
##### load data #############################################################
############################################################################## # 

F_samples <- readRDS(file.path(path2output, 'convert_to_matrix.RData'))
xvec <- readRDS(file.path(path2output, 'prepare_EXIOBASE_x.RData'))

# calculate S
S_samples <- lapply(F_samples, function(x) calculate_S(x, xvec))
rm(F_samples)
gc()


Ymat <- readRDS(file.path(path2output, 'prepare_EXIOBASE_Y.RData'))
Lmat <- readRDS(file.path(path2output, 'prepare_EXIOBASE_L.RData'))

# Calculate Footprints ========================================================
#unlink(file.path(path2output, 'footprints'), recursive = TRUE)
#dir.create(file.path(path2output, 'footprints'))

pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user = 'postgres', 
                dbname = 'template1', 
                host = 'localhost',
                password = '123')
dbRemoveTable(con, 'footprint_multiplier')
dbRemoveTable(con, 'footprint_national')


calculate_footprints <- function(x, n_cores) {
  
}

reshape_data <- function(x, irun) {
  x <- as.data.table(x)
  x <- melt(x[,i:=.I], 
            id.vars="i", 
            variable.name = 'j')
  x <- x[value>0]
  x[,j:=as.integer(j)]
  x[, run := irun]
  return(x[])
}



for (i in 1:length(S_samples)) {
  fp_multiplier <- S_samples[[i]] %*% Lmat
  fp_national <- fp_multiplier %*% Ymat
  
  fp_multiplier <- reshape_data(fp_multiplier, irun = i)
  fp_national <- reshape_data(fp_national, irun = i)
  
  dbWriteTable(con, 'footprint_multiplier', 
               fp_multiplier, 
               append = TRUE, 
               row.names = FALSE)
  
  dbWriteTable(con, 'footprint_national', 
               fp_national, 
               append = TRUE, 
               row.names = FALSE)
  
  # suppressMessages(save_results(fp_multiplier,
  #                               path = file.path(path2output, 'footprints'),  
  #                               suffix = paste0('multiplier', i), 
  #                               type = '.csv'))
  # suppressMessages(save_results(fp_national,
  #                               path = file.path(path2output, 'footprints'),  
  #                               suffix = paste0('national', i), 
  #                               type = '.csv'))
   progress(i,length(S_samples))
}

dbDisconnect(con)

# fp_multiplier <- pbmclapply(S_samples, function(x) {
#   x %*% Lmat
# }, mc.cores = 1)
# 
# fp_national <- pbmclapply(fp_multiplier, function(x) {
#   x %*% Ymat
# }, mc.cores = 1)




# plots ==============================

# fp_national2[, region3 := countrycode(region, 'iso2c', 'iso3c')]
# fp_national2[, temp := quantile(cv, 0.975, na.rm = TRUE), by = gas]
# fp_national2[cv >= temp, label := region3]
# fp_national2[i!s.na(label)]
# fp_national2[!is.na(mean)] %>% 
#   .$region %>% unique %>% length
# # TODO: there is s.th. wrong with the country names!!
# 
# 
# ggplot(fp_national2, aes(x = gas, y = cv)) + 
#   stat_summary(fun.data =  percentile, geom = 'errorbar', width = .2, 
#                size = 1, col = my_cols[3]) +
#   stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2,
#                fill = 'white') +
#   geom_jitter(aes(size = mean_rel), shape=16, size = 2,col = "grey40", alpha = 0.4,
#               position=position_jitter(0.1, seed=1)) +
#   geom_text_repel(aes(label = label), position = position_jitter(0.1, seed = 1))+
#   geom_violin(trim = TRUE, alpha = 0.2, fill = my_cols[7], col = NA) + 
#   stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2, 
#                size = 0.7, alpha=0.4, fill = my_cols[3], col = my_cols[3]) + 
#   stat_summary(fun.data = give.n, geom = "text") +
#   theme_bw() +
#   scale_color_viridis_c(direction = -1) + 
#   ggtitle('Distributions of the Relative Uncertainties of national footprints')
# 
# 
# fp_multiplier2$cv %>% summary
# ggplot(fp_multiplier2[cv > 0.001], aes(x = gas, y = cv)) + 
#   stat_summary(fun.data =  percentile, geom = 'errorbar', width = .2, 
#                size = 2, col = my_cols[3]) +
#   stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2,
#                fill = 'white') +
#   geom_jitter(shape=16, size = 0.3,col = "grey40", alpha = 0.3,
#               position=position_jitter(0.1)) +
#   geom_violin(trim = TRUE, alpha = 0.2, fill = my_cols[7], col = NA) + 
#   stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2, 
#                size = 1, alpha=0.4, fill = my_cols[3], col = my_cols[3]) + 
#   stat_summary(fun.data = give.n, fun.args = list(y = exp(0.01)), geom = "text") +
#   scale_y_log10() + 
#   #scale_x_continuous(trans="log10", limits=c(1E-32,NA))+
#   theme_bw() +
#   scale_color_viridis_c(direction = -1) + 
#   ggtitle('Distributions of the Relative Uncertainties of multipliers')
# 
# ggplot(fp_national2, 
#        aes(x = mean_rel, y = cv, col = gas)) + 
#   geom_point(alpha = 0.70) + 
#   #scale_x_continuous(trans="log10", limits=c(1E-32,NA))+
#   xlab('Mean contribution to global emissions')+
#   scale_color_colorblind() + 
#   #geom_vline(xintercept = 1E-6, linetype = 'dashed', size = 1.5, col = my_cols[7]) +
#   ggtitle('Rel. Uncertainties of country footprints against their mean share of global emissions')
# 
# 
# # random sampling without correlations =========================================
# 
# 
# Emat_list_random <- lapply(Emat_list2, function(x) {
#   apply(x, 1, sample) %>% t
# })
# 
# fp_national_rand <- lapply(Emat_list_random, function(x) {
#   x %*% Lmat %*% Ymat_new
# })
# 
# 
# fp_multiplier_rand <- lapply(Emat_list_random, function(x) {
#   x %*% Lmat
# })
# 
# fp_national_rand <- lapply(fp_national_rand, function(x) {
#   rownames(x) <- c('CO2', 'CH4', 'N2O') # TODO: richtig machen nith hard coden
#   colnames(x) <- unique(colnames_Y$region)
#   x <- as.data.table(x, keep.rownames = 'gas')
#   x <- melt(x, variable.name = 'region', variable.factor = FALSE, 
#             id.vars = c('gas'))
#   return(x)
# })
# fp_national_rand <- rbindlist(fp_national_rand, idcol = 'run')
# 
# 
# fp_multiplier_rand <- lapply(fp_multiplier_rand, function(x) {
#   rownames(x) <- c('CO2', 'CH4', 'N2O') # TODO: richtig machen nith hard coden
#   #colnames(x) <- unique(colnames_Y$region)
#   x <- as.data.table(x, keep.rownames = 'gas')
#   x <- melt(x, variable.name = 'id', variable.factor = FALSE, 
#             id.vars = c('gas'))
#   x[, id := as.numeric(gsub('V', '', id))]
#   x <- x[value > 0]
#   return(x)
# })
# fp_multiplier_rand <- rbindlist(fp_multiplier_rand, idcol = 'run')[value > 0]
# 
# 
# fp_national_rand2 <- fp_national_rand[, list(mean = mean(value), 
#                                              median = median(value), 
#                                              sd = sd(value), 
#                                              cv = sd(value) / mean(value)), 
#                                       by = .(gas, region)]
# fp_national_rand2[, mean_rel := mean / sum(mean), by = .(gas)]
# 
# fp_multiplier_rand2 <- fp_multiplier_rand[, list(mean = mean(value), 
#                                                  median = median(value), 
#                                                  sd = sd(value), 
#                                                  cv = sd(value) / mean(value)), 
#                                           by = .(gas, id)]
# fp_multiplier_rand2[, mean_rel := mean / sum(mean), by = .(gas)]
# 
# 
# saveRDS(fp_national_rand2, './results/fp_national_rand_summary.RData')
# saveRDS(fp_multiplier_rand2, './results/fp_multiplier_rand_summary.RData')
# 
# # plots ==============================
# 
# ggplot(fp_national_rand2, aes(x = gas, y = cv)) + 
#   stat_summary(fun.data =  percentile, geom = 'errorbar', width = .2, 
#                size = 2, col = my_cols[3]) +
#   stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2,
#                fill = 'white') +
#   geom_jitter(aes(size = mean_rel, alpha = mean_rel), shape=16,# size = 0.3,col = "grey20",
#               position=position_jitter(0.1)) +
#   geom_violin(trim = TRUE, alpha = 0.2, fill = my_cols[7], col = NA) + 
#   stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2, 
#                size = 1, alpha=0.4, fill = my_cols[3], col = my_cols[3]) + 
#   stat_summary(fun.data = give.n, geom = "text") +
#   theme_bw() +
#   scale_color_viridis_c(direction = -1) + 
#   ggtitle('Distributions of the Relative Uncertainties of national footprints WITHOUT CORRELATIONS')
# 
# save_plot(suffix = '_1')
# 
# ggplot(fp_multiplier_rand2, aes(x = gas, y = cv)) + 
#   stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2,
#                fill = 'white') +
#   geom_jitter(aes(size = mean_rel, alpha = mean_rel), shape=16,# size = 0.3,col = "grey20",
#               position=position_jitter(0.1)) +
#   stat_summary(fun.data =  percentile, geom = 'errorbar', width = .2, 
#                size = 2, col = my_cols[3]) +
#   geom_violin(trim = TRUE, alpha = 0.2, fill = my_cols[7], col = NA) + 
#   stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2, 
#                size = 1, alpha=0.4, fill = my_cols[3], col = my_cols[3]) + 
#   stat_summary(fun.data = give.n, geom = "text") +
#   scale_y_log10() + 
#   #scale_x_continuous(trans="log10", limits=c(1E-32,NA))+
#   theme_bw() +
#   scale_color_viridis_c(direction = -1) + 
#   ggtitle('Distributions of the Relative Uncertainties of multipliers WITHOUT CORRELATIONS')
# save_plot(suffix = '_2')
# 
# 
# ggplot(fp_national2, 
#        aes(x = mean_rel, y = cv, col = gas)) + 
#   geom_point(alpha = 0.70) + 
#   #scale_x_continuous(trans="log10", limits=c(1E-32,NA))+
#   xlab('Mean contribution to global emissions')+
#   scale_color_colorblind() + 
#   #geom_vline(xintercept = 1E-6, linetype = 'dashed', size = 1.5, col = my_cols[7]) +
#   ggtitle('Rel. Uncertainties of country footprints against their mean share of global emissions')
# 
# save_plot(suffix = '_3')
# 

# Calculate Co variance matrix =================================================
# dt4
# sample_list <- dt4 %>% 
#   split(by = 'gas') %>% 
#   lapply(function(x) x$sample %>% as.data.table())
# cor_list <- lapply(sample_list, cor, method = 'pearson')
# 
# boxplot(cor_list)
# 
# 
# for (i in 1:length(cor_list)) {
#   cor_list[[i]][is.na(cor_list[[i]])] <- 0
#   cor_list[[i]][cor_list[[i]] < 0.7] <- 0
# }
# 
# cor_list <- lapply(cor_list, function(x) x[is.na(x)] <- 0)
# 
# cor_list <- lapply(cor_list, function(x) x[x < 0.7] <- 0)
# 
# 
# 
# library(corrplot)
# corrplot(cor_list[[1]], type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)
# corrplot(cor_list[[2]], type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)
# corrplot(cor_list[[3]], type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)
# 
# 
# # Compare to original EXIOBASE F matrix =========================================
# read_EB3_S <- function(path, metadata = FALSE) {
#   S <- fread(file.path(path), 
#              skip = 26, header = FALSE, drop = 1) %>% 
#     as.matrix
#   if (isTRUE(metadata)) {
#     meta <- read_EB3_S_meta(path)
#     attr(S, 'colnames') <- meta$colnames
#     attr(S, 'rownames') <-  meta$rownames
#   }
#   
#   return(S)
#   
# }
# 
# Emat_orig <- read_EB3_S('/home/simon/Documents/PhD_PROSET/data/EXIOBASE3/V3.8.2/IOT_2015_ixi/satellite/F.txt', 
#                         metadata = TRUE)
# dim(Emat_orig)
# attr(Emat_orig, 'rownames')[, id := 1:.N]
# attr(Emat_orig, 'rownames')
# attr(Emat_orig, 'rownames')[grepl('CO2', category)]
# Emat_orig2 <- rbind(
#   colSums(Emat_orig[attr(Emat_orig, 'rownames')[grepl('CO2', category)]$id,]), 
#   colSums(Emat_orig[attr(Emat_orig, 'rownames')[grepl('CH4', category)]$id,]), 
#   colSums(Emat_orig[attr(Emat_orig, 'rownames')[grepl('N2O', category)]$id,])  
# )
# dim(Emat_orig2)
# 
# rownames(Emat_orig2) <- c('CO2', 'CH4', 'N2O')
# 
# 
# as.data.table(rowSums(Emat_orig2), keep.rownames = 'gas') %>% 
#   setnames(c('gas', 'value_orig')) %>% 
#   merge(dt4[, list(myvalue = sum(mean)), by = gas], by = 'gas') %>% 
#   .[, myvalue / value_orig]
# 
# 
# lim <- 1E10
# plot(Emat[1,] * 1E6, Emat_orig[1,], xlim = c(0, lim), ylim = c(0, lim))
# abline(a = 0, b = 1)


# THE END ---------------------------------------------------------------------
