#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-09-06 14:28:55
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
library(testthat)
library(janitor)
library(countrycode)
library(eurostat)
library(viridis)
library(ggthemes)






############################################################################## # 
##### functions #################################################################
############################################################################## # 

source('./src/functions.R')

############################################################################## # 
##### settings #################################################################
############################################################################## # 
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output

theme_set(theme_bw())
my_cols <- c((colorblind_pal()(8)), '#D3D3D3')

gases <- config$gases
year <- config$year
path2eurostat <- config$path2eurostat_bridging_items

############################################################################## # 
##### load data #############################################################
############################################################################## # 
data_full_code <- get_eurostat(id = 'env_ac_aibrid_r2', time_format = 'num', 
                     stringsAsFactors = FALSE, type = 'code')
data_full_code <- as.data.table(data_full_code)

data_full_label <- get_eurostat(id = 'env_ac_aibrid_r2', time_format = 'num', 
                      stringsAsFactors = FALSE, type = 'label')
data_full_label <- as.data.table(data_full_label)


lapply(data_full_code, function(x){
  if (!is.numeric(x)) unique(x)
})
data_full_code$time %>% unique

data_full <- cbind(data_full_code, airpol_label = data_full_label$airpol, 
              indic_env_label = data_full_label$indic_env, 
              unit_label = data_full_label$unit, 
              geo_label = data_full_label$geo)

#data_full[, .(indic_env, indic_env_label)] %>% unique %>% view

# 2. prepare data ====================================================

# select gases & right unit
data_full <- data_full[airpol %in% gases & unit == 'THS_T'] %>% 
  .[, values := set_units(values, Gg)] %>% 
  .[]

# check EU totals
data_full[grepl('ATR', indic_env) & time == as.numeric(year) & geo != 'EU27_2020'
     , sum(values)
     , by = .(airpol, indic_env)]

# select year
data <- data_full[time == year]
data <- data[indic_env %in% c('AEMIS_TER_NRES_LTR', 'AEMIS_RES_ABR_LTR')]
data[, country := countrycode(geo, origin = 'eurostat', 
                                     destination = 'iso3c')]
data <- data[!is.na(country)]

data <- dcast(data, airpol + time + country ~ indic_env, 
                     value.var = 'values', fill = 0)
data[, NET_LAND := AEMIS_RES_ABR_LTR - AEMIS_TER_NRES_LTR]

setnames(data, c('time', 'airpol'), c('year', 'gas'))


############################################################################## # 
##### 2. Attach uncertainties to it ################################################
############################################################################## # 
# TODO
# data[cv := 0.3]
# data[, sd := NET_LAND * cv]

# Test =========================================================================

# test_that('all good',{
#   expect_equal(datawb[, sum(share_regional)], length(GASES)) 
# })



############################################################################## # 
##### save results ################################################
############################################################################## # 
save_results(data[, .(gas, year, country, NET_LAND)][], type = '.feather')






# 3. plots =====================================================================
if (FALSE) {
  data2plot <- data_full
  data2plot[grepl('RES_ABR', indic_env), values_net := values]
  data2plot[grepl('TER_NRES', indic_env), values_net := -values]
  data2plot[grepl('ADJ', indic_env), values_net := -values]
  data2plot[indic_env  == 'AEMIS_TER', values_net := values]
  
  #data2plot <- data2plot[!(indic_env %in% c('TOT_NRA', 'TOT_NRES', 'TOT_NACE_HH'))]
  data2plot <- data2plot[!(indic_env %in% c('AEMIS_TER_LULUCF', 
                                            'AEMIS_RES_ABR', 
                                            'AEMIS_RES',
                                            'AEMIS_TER_NRES',
                                            'CRL_GRL', 
                                            'FORL', 
                                            'LULUCF', 
                                            'LULUCF_OTH'))]
  data2plot[indic_env == 'AEMIS_TER', indic_env := 'ZAEMIS_TER']
  
  ggplot(data2plot[airpol == 'CO2'], aes(x = time, 
                                         y = values_net, 
                                         fill = indic_env)) + 
    geom_col(position = 'stack') + 
    scale_fill_manual(values = my_cols) +
    facet_wrap(~geo_label, scales = 'free_y')
  ggsave('./figures/bridging_detailed_by_country_CO2.pdf', scale = 2.5)  
  
  ggplot(data2plot[airpol == 'CH4'], aes(x = time, y = values_net, fill = indic_env)) + 
    geom_col(position = 'stack') + 
    facet_wrap(~geo_label, scales = 'free_y') +
    scale_fill_manual(values = my_cols)
  ggsave('./figures/bridging_detailed_by_country_CH4.pdf', scale = 2.5)  
  
  ggplot(data2plot[airpol == 'N2O'], aes(x = time, y = values_net, fill = indic_env)) + 
    geom_col(position = 'stack') + 
    facet_wrap(~geo_label, scales = 'free_y') +
    scale_fill_manual(values = my_cols)
  ggsave('./figures/bridging_detailed_by_country_N2O.pdf', scale = 2.5)  
  
  # 4. data quality check -------------------------------------------------------
  
  
  data_wide <- dcast(data, airpol + unit + geo + time ~ indic_env, 
                     value.var = 'values')
  
  # TOT_NRA = NRA_AIR + NRA_FISH + NRA_LAND + NRA_WATER
  summary(na.omit(data_wide[, NRA_AIR + NRA_FISH + NRA_LAND + NRA_WATER] - data_wide$TOT_NRA))
  plot(data_wide[, NRA_AIR + NRA_FISH + NRA_LAND + NRA_WATER] - data_wide$TOT_NRA)
  
  # TOT_NRES = NRES_AIR + NRES_LAND + NRES_WATER
  summary(na.omit(data_wide[, NRES_AIR + NRES_LAND + NRES_WATER] - data_wide$TOT_NRES))
  plot(data_wide[, NRES_AIR + NRES_LAND + NRES_WATER] - data_wide$TOT_NRES)
  
  # TOT_NACE_HH = TOT_CONV + TOT_NRA - TOT_NRES - ADJ_OTH
  plot(data_wide[, TOT_CONV + TOT_NRA - ADJ_OTH - TOT_NRES]- data_wide$TOT_NACE_HH)
  abline(h = 0, col = 2)
  
  data_wide[, temp :=  TOT_NRA - TOT_NRES + TOT_CONV - ADJ_OTH]
  data_wide[, absdif := TOT_NACE_HH - temp]
  data_wide[, reldif := (TOT_NACE_HH - temp) / TOT_NACE_HH]
  data_wide[reldif > 0.1]
  data_wide[airpol %in% gases & reldif > 0.000001]
  
}




# THE END ---------------------------------------------------------------------
