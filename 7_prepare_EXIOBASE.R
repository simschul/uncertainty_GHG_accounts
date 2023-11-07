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

############################################################################## # 
##### settings #################################################################
############################################################################## # 

source(file.path('src', 'functions.R'))
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output
theme_set(theme_bw())
RhpcBLASctl::blas_set_num_threads(config$n_cores)
############################################################################## # 
##### load data #############################################################
############################################################################## # 

# meta data =========

meta <- parse_EB3_metadata(config$path2exiobaseIOT)


# F =================
F_raw <- read_EB3_F(file.path(config$path2exiobaseIOT, 'impacts', 'F.txt'), 
                    metadata = TRUE)
dim(F_raw)

reference_impacts <- data.table(
  gas = c('CO2', 'CH4', 'N2O'), 
  exiobase_name = c(
    'Carbon dioxide (CO2) IPCC categories 1 to 4 and 6 to 7 (excl land use, land use change and forestry)', 
    'Methane (CH4) IPCC categories 1 to 4 and 6 to 7 (excl land use, land use change and forestry)',
    'Nitrous Oxide (N2O) IPCC categories 1 to 4 and 6 to 7 (excl land use, land use change and forestry)'
  )
)
reference_impacts[, exiobase_index := attributes(F_raw)$rownames[, .I[category %in% exiobase_name]]]

F_ref <- F_raw[reference_impacts$exiobase_index, ]
rownames(F_ref) <- reference_impacts$gas

# replace N2O from satellite (bug in characterized impacts: shoudl also include agricultural emissions, see email to konstantin 5 jul 2023 a las 16:54,)
F_raw2 <- read_EB3_F(file.path(config$path2exiobaseIOT, 'satellite', 'F.txt'), 
                     metadata = TRUE)
index_n2o <- attributes(F_raw2)$rownames[, .I[grepl('N2O', category)]]
F_new <- colSums(F_raw2[index_n2o,]) / 1E6
F_ref['N2O',] <- F_new
# end replacement

attributes(F_raw)$colnames[, id := 1:.N]

F_dt <- as.data.table(F_ref, keep.rownames = 'gas') %>% 
  melt(id.vars = 'gas', variable.name = 'id', variable.factor=FALSE) %>% 
  .[, id := as.numeric(substring(id, 2)) - 1] %>%
  merge(attributes(F_raw)$colnames, by = 'id', sort = FALSE) %>% 
  merge(meta$industries[, .(Name, CodeNr)], by.x = 'sector', by.y = 'Name', sort = FALSE) %>% 
  .[, .(gas, region, CodeNr, value)] %>% 
  setnames(c('CodeNr', 'region'), c('industry_code', 'EB_region')) %>% 
  .[]
F_dt
#F_dt[EB_region == 'DE' & industry_code == 'i40.11.a']
F_by_reg <- F_dt[, list(value = sum(value)), by = .(EB_region, gas)]

# x =================
xvec <- fread(file.path(config$path2exiobaseIOT, 'x.txt'))
xvec <- xvec$indout 
length(xvec)

# L =================
Amat <- read_EB3_A(file.path(config$path2exiobaseIOT, 'A.txt'), 
                   metadata = TRUE)
Lmat <- calculate_L(Amat)
attributes(Lmat) <- attributes(Amat)
dim(Lmat)

# Y =================

Ymat <- read_EB3_Y(file.path(config$path2exiobaseIOT, 'Y.txt'), 
                   metadata = TRUE)
dim(Ymat)

(colnames_Y <- attr(Ymat, 'colnames')[, id := 1:.N])
colnames(Ymat) <- colnames_Y$region

Ymat_new <- matrix(0, nrow = nrow(Ymat), ncol = length(colnames_Y$region %>% unique))


i <- 1
for (region in colnames_Y$region %>% unique) {
  Ymat_new[,i] <- rowSums(Ymat[, colnames(Ymat) == region])
  i <- i + 1
}
sum(Ymat_new) == sum(Ymat)

Yvec <- rowSums(Ymat_new)
length(Yvec)

colnames(Ymat_new) <- colnames_Y$region %>% unique

# calculate reference footprints ===============================================

# calculate reference footprints with exiobase =================================

S_ref <- calculate_S(F_ref, xvec)

# calculate multiplier
fp_multiplier_ref <- S_ref %*% Lmat
# calculate national footprints
fp_national_ref <-  fp_multiplier_ref %*% Ymat_new



# reshape multiplier
rownames(fp_multiplier_ref) <- reference_impacts$gas
fp_multiplier_ref <- as.data.table(fp_multiplier_ref, keep.rownames = 'gas')
fp_multiplier_ref <- melt(fp_multiplier_ref, variable.name = 'id', variable.factor = FALSE, 
                        id.vars = c('gas'))
fp_multiplier_ref[, id := (substring(id, 2) %>% as.numeric) - 2]
fp_multiplier_ref <- fp_multiplier_ref[value > 0]

fp_multiplier_ref <- merge(fp_multiplier_ref, 
      attributes(F_raw)$colnames[, id := 1:.N], 
      by = 'id', sort = FALSE, all.x = TRUE)
setcolorder(fp_multiplier_ref, c('region', 'sector', 'gas', 'id', 'value'))

fp_multiplier_ref <- merge(fp_multiplier_ref, meta$industries[, .(Name, CodeNr)], 
      by.x = 'sector', by.y = 'Name', sort = FALSE) %>%
  setnames(c('CodeNr', 'region'), c('industry_code', 'EB_region')) %>% 
  .[]

# reshape national footprnts
rownames(fp_national_ref) <- reference_impacts$gas
colnames(fp_national_ref) <- colnames(Ymat_new)
fp_national_ref <- as.data.table(fp_national_ref, keep.rownames = 'gas')
fp_national_ref <- melt(fp_national_ref, variable.name = 'region', variable.factor = FALSE, 
                        id.vars = c('gas'))
fp_national_ref[, sum(value), by = gas]
setnames(fp_national_ref, 'region', 'EB_region') 




############################################################################## # 
##### save results #############################################################
############################################################################## # 

save_results(xvec, suffix='_x')
save_results(Lmat, suffix='_L')
save_results(Ymat_new, suffix='_Y')
save_results(fp_national_ref, suffix='_fp_national', type = '.feather')
save_results(fp_multiplier_ref, suffix='_fp_multiplier', type = '.feather')

save_results(F_dt, suffix = '_gea', type = '.feather')
save_results(F_by_reg, suffix = '_gea_by_reg', type = '.feather')

# the end ======================================================================

















