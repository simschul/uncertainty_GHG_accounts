#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-06-21 12:16:12
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
library(data.tree)
library(logr)

############################################################################## # 
##### general settings #################################################################
############################################################################## # 

# source utils
source(file.path('src', 'functions.R'))
# read config and setup log script
config <- setup_config_and_log()

path2output <- config$path2output

############################################################################## # 
##### settings #################################################################
############################################################################## # 

path2CT <- config$path2CT_CRF_EXIOBASE


############################################################################## # 
##### load data #############################################################
############################################################################## # 

# which classifications exist in emissions data?
classes_from_CT <- rio::import(path2CT) %>% 
  as.data.table %>% 
  .$CRF_class %>% 
  na.omit %>% 
  unique %>% 
  data.table(original = .)

classes_from_CT[, id := gsub(' ', '_', tolower(original))]

# how are these classifications named in NIR uncertainty tables? ===============
matching_classes <- vector('list', length(classes_from_CT$id)) %>% 
  setNames(classes_from_CT$id)
length(matching_classes)

matching_classes$solid_fuels <- c(
  'black_coal', 'brown_coal', 'bituminous_coal', 'coal', 'solid_fossil_fuels', 
  'solid_fuel', 'solids', "solid_f.", 'solid'
)

matching_classes$gaseous_fuels <- c(
  'natural_gas', 'gaseous_fossil_fuels', 'gaseous', 'gaseous_fuel', 
  "gaseous_f.", 'gas', 'cng'
)

matching_classes$liquid_fuels <- c(
  'liquid_fossil_fuels', 'liquid_f.', 'liquid', 'liquid_fuel', 
  'liquids', 'all_liquid_fuels'
)

matching_classes$other_liquid_fuels <- c(
  'other_liquid_fuels_(please_specify)', 
  "(stationary)_oil" , "fuel_oil", "petroleum_coke", 
  'lubricants', 'oil'
)

matching_classes$other_fossil_fuels <- c(
  'other_fossil_fuel', "other_f.", "other_fosssil_fuels", 
  "other_fossil_fuels_(please_specify)", 
  "other_fuels_(please_specify)", "other_fuels_(waste)", 
  "other_fossil" , "other_fuels", "other_(waste)" , 
  'waste_incineration', 'others', 'msw', 'waste'
)

#matching_classes$aviation_gasoline <- c()
matching_classes$jet_kerosene <- c('kerosene')
matching_classes$diesel_oil <- c('diesel', 'derv')
#matching_classes$`gas/diesel_oil` <- c()
matching_classes$gasoline <- c(
  'motor_gasoline',  "gasoline/_lpg" #'gas',
)
matching_classes$`liquefied_petroleum_gases_(lpg)` <- c(
  'lpg', "gasoline/_lpg"
)

matching_classes$residual_fuel_oil <- c(
  'residual_oil'
)


matching_classes$rabbit <- c(
  'rabbit', 'rabbits'
)
matching_classes$growing_cattle <- c(
  'young_cattle', "non-dairy_young_cattle_(younger_than_1_year)", 
  "non-dairy_young_cattle_(younger_than_1_year)", 
  "non-dairy_young_cattle_1-2_years"
)
matching_classes$cattle <- c(
  "bulls_(olther_than_2_years)", 
  'non-dairy_heifers_(older_than_2_years)', 
  "bulls_(older_than_2_years)"
)

matching_classes$reindeer <- c(
  'raindeer'
)

matching_classes$`fur-bearing_animals` <- c(
  'fur_animals'
)
matching_classes$swine <- c(
  'sw_ine', 'swaine'
)

matching_classes$other <- c(
  'other_livestock', 'other_animal'
)

matching_classes$biomass <- c(
  'wood'
)


# unsure: fuels, gas, fuel oil

unlist(matching_classes)

matching_classes$indirect_emissions <- c(
  'indirect'
)

# create one look up table from list ===========================================

ct_classes <- data.table('CRF_class' = names(matching_classes), 
                         'other_names' = '', 
                         'main_class' = names(matching_classes))
for (i in 1:length(matching_classes)) {
  ct_classes[i]$other_names <- paste(matching_classes[[i]], collapse = ', ')
}

ct_classes[CRF_class %in% c('liquid_fuels', 'aviation_gasoline',
                            'jet_kerosene', 'diesel_oil', 'gasoline', 
                            'gas/diesel_oil',
                            'liquefied_petroleum_gases_(lpg)', 
                            'other_liquid_fuels', 'residual_fuel_oil'), 
           main_class := 'liquid_fuels']

ct_classes[CRF_class %in% c('cattle', 'dairy_cattle', 'growing_cattle', 
                            'mature_dairy_cattle', 'non-dairy_cattle', 
                            'other_cattle', 'other_mature_cattle'),
           main_class := 'cattle']

#view_excel(ct_classes)

# create hierarchy ========================================
ct_classes[CRF_class == 'total_for_category', pathString := 'total_for_category']
ct_classes[CRF_class == main_class & CRF_class != 'total_for_category', 
           pathString := paste('total_for_category', 
                                                        CRF_class, 
                                                        sep = '|')]
ct_classes[CRF_class != main_class, pathString := paste('total_for_category', 
                                                        main_class,
                                                        CRF_class, 
                                                        sep = '|')]

# ct_classes[CRF_class== 'mature_dairy_cattle', 
#            pathString := 'total_for_category|cattle|dairy_cattle|mature_dairy_cattle']

# save CT tables/list ===========================================================
# saveRDS(matching_classes, file.path(path2output, paste0(filename, '_list.RData')))
# saveRDS(ct_classes, file.path(path2output, paste0(filename, '_table.RData')))
save_results(matching_classes, suffix = '_list' )
save_results(ct_classes, suffix = '_table')

log_close()
# THE END ---------------------------------------------------------------------
