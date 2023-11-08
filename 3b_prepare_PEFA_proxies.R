#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-09-01 15:02:32
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
library(eurostat)

############################################################################## # 
##### functions #################################################################
############################################################################## # 
source(file.path('src', 'functions.R'))
source(file.path('src', 'functions_eurostat.R'))

############################################################################## # 
##### settings #################################################################
############################################################################## # 
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output

theme_set(theme_bw())



############################################################################## # 
##### 1. load data #############################################################
############################################################################## # 
pefa <- get_eurostat(id = 'env_ac_pefasu', time_format = 'num', 
                     stringsAsFactors = FALSE, type = 'code') %>% 
  as.data.table
# 

dictionary <- get_my_eurostat_dic(pefa)
dictionary$prod_nrg[grepl('Transport', full_name)]
dictionary$prod_nrg[grepl('Motor spirit', full_name)]
dictionary$prod_nrg[grepl('Natural gas', full_name)]
# dictionary$nace_r2 %>% view

# P17 Transport diesel (without bio)
# P14 Motor spirit (without bio)
# P13 Natural gas (without bio)
# what data is available??

# data[stk_flow == 'ER_USE' 
#      & prod_nrg %in% c('P14', 'P17')] %>% 
#   #na.omit %>% 
#   .[, sum(values, na.rm = TRUE), by = .(geo, time)] %>%
#   .[V1 > 0] %>% 
#   as.dense.matrix() %>% 
#   view


dictionary <- get_my_eurostat_dic(pefa)
attr(pefa, 'metadata') <- dictionary
attributes(pefa)

pefa[, values := set_units(values, TJ)]


# 2. Select rows =================================================================

# P17 Transport diesel (without bio)
# P14 Motor spirit (without bio)

pefa <- pefa[stk_flow == 'ER_USE']
pefa <- pefa[prod_nrg %in% c('P14', 'P17')]
pefa <- pefa[!(nace_r2 %in% c('SD_SU', 'NRG_FLOW', 'L68A'))]

# kick out all non-transport household energy use
pefa <- pefa[!(grepl('^HH', nace_r2) & !(nace_r2 %in% c('HH_TRA', 'HH')))]


# 2 mappings: 
# 1) fuels: transport diesel --> liquid fuels, motor spirit --> liquid fuels
# 2) sectors
# TODO: how about gaseous fuels?? 
# Option 1: take P13 natural gas as proxy, Option 2: take sum of P17 + P14 as proxy
# Problem with (1): Natural gas probably mostly used for other stuff than transport!!! --> take option 2

# prepare PEFA data (impute missing years)
countries_all <- unique(pefa$geo)
countries_missing <- countries_all[!(countries_all %in% pefa[time == 2015]$geo)]
pefa[geo == 'MK' & time == 2019, time := 2015]
pefa[geo == 'RS' & time == 2016, time := 2015]

# select year
pefa <- pefa[time == 2015]
pefa[, time := as.character(time)]

# convert country codes to iso 3
pefa[, party := countrycode(geo, 'eurostat', 'iso3c')]
pefa <- na.omit(pefa)


# select leaves only 
pefa[, isleaf := is_NACErev2_leaf(nace_r2), by = .(party, time, prod_nrg)]
pefa <- pefa[isleaf == TRUE]

# 3. Convert Units =============================================================

# convert from Joule --> kg / Joule
pefa[prod_nrg == 'P14', 
     CO2 := values * get_emission_factors(fuel = 'motor_gasoline', 
                                          gas = 'CO2')]
pefa[prod_nrg == 'P14', 
     CH4 := values * get_emission_factors(fuel = 'motor_gasoline', 
                                          gas = 'CH4')]
pefa[prod_nrg == 'P14', 
     N2O := values * get_emission_factors(fuel = 'motor_gasoline', 
                                          gas = 'N2O')]
pefa[prod_nrg == 'P17', 
     CO2 := values * get_emission_factors(fuel = 'gas_diesel_oil', 
                                          gas = 'CO2')]
pefa[prod_nrg == 'P17', 
     CH4 := values * get_emission_factors(fuel = 'gas_diesel_oil', 
                                          gas = 'CH4')]
pefa[prod_nrg == 'P17', 
     N2O := values * get_emission_factors(fuel = 'gas_diesel_oil', 
                                          gas = 'N2O')]


# 4. reshape data =============================================================
# wide 2 long
pefa <- melt(pefa, id.vars = c('party', 'time', 'prod_nrg', 'nace_r2'), 
             measure.vars = c('CO2', 'CH4', 'N2O'), 
             variable.name = 'gas', variable.factor = FALSE)

pefa <- pefa[, list(value = sum(value)), by = .(party, time, gas, nace_r2)]

pefa[, proxy := value / sum(value), by = .(party, time, gas)]
pefa[, proxy := drop_units(proxy)]
pefa <- pefa[proxy > 0]

# put NACE code + proxy data into one list column
pefa2 <- pefa[, list(proxies = list(data.table(nace_code = nace_r2, 
                                               share = proxy))), 
              by = .(party, time, gas)]

# plot industry / household split
data2plot <- copy(pefa)
data2plot[nace_r2 == 'HH_TRA', type := 'households']
data2plot[is.na(type), type := 'industry']
data2plot <- data2plot[, list(share = sum(proxy), 
                              emissions = sum(value) %>% drop_units()), 
          by = .(party, time, gas, type)]

p <- ggplot(data2plot, aes(x = share, y = party, fill = type)) + 
  geom_col() + 
  facet_wrap(~gas)+ 
  ggtitle("Industry/Household split of fuel used for Road Transport, year 2015, source: PEFA")

save_plot(plot = p, suffix = '_ind_household_split')

# calculate weigted mean of industry/household split accros countires
data2plot[, weighted.mean(x = share, w = emissions), 
          by = .(time, gas, type)]

mean_split <- data2plot[, list(emissions = sum(emissions)), 
                        by = .(time, gas, type)]
mean_split[, share := emissions / sum(emissions), 
           by = .(time, gas)]

############################################################################## # 
##### save results #############################################################
############################################################################## # 
save_results(pefa2, type = '.feather')
save_results(mean_split, suffix = '_mean_ind-hous_split', type = '.feather')
#saveRDS(pefa2, './temp_results/4b_PEFA_proxies.RData')

# THE END ---------------------------------------------------------------------
