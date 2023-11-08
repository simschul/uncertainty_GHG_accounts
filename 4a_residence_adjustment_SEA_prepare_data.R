#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-03-21 15:25:17
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
library(rio)
library(ggthemes)
library(countrycode)
library(arrow)
############################################################################## # 
##### settings #################################################################
############################################################################## # 

source(file.path('src', 'functions.R'))
# read config and setup log script
config <- setup_config_and_log()
path2output <- config$path2output


N <- config$sample_size
GASES <- config$gases
path2data <- config$path2selin2021

############################################################################## # 
##### load data #############################################################
############################################################################## # 

data <- import(path2data, which = 'All')
data <- as.data.table(data)

data <- melt(data, id.vars = c('ID', 'Name_ref', 'EU', 'national_ref', 
                               'National_MT'), 
             measure.vars = c('Bunker_MT', 
                              'Flag_MT', 
                              'Owner_MT', 
                              'Operator_MT', 
                              'Manager_MT'),
             variable.factor = FALSE)
data$EU <- ifelse(is.na(data$EU), FALSE, TRUE)
#data[, value := set_units(value, Mt)]
#data[, National_MT := set_units(National_MT, Mt)]

data <- na.omit(data)

data <- data[variable == 'Operator_MT']
data[, country := countrycode(national_ref, origin =  'country.name',destination = 'iso3c')]
data[national_ref == 'Micronesia', country := 'FSM']
data[, share := value / sum(value)]
data[, year := 2015]

data[, sum(share)]

# copy for each gas (assumption: same shares per gas)
data <- lapply(GASES, function(x) data) %>% 
  setNames(GASES) %>% 
  rbindlist(idcol = 'gas')


# save =========================================================================
save_results(data[, .(gas, country, year, share)], type = '.feather')



# Plots ========================================================================
if (FALSE) {
  data$value %>% sum
  
  data[variable == 'Operator_MT'] %>%
    setorderv('value', order = -1L) %>% 
    .[]
  
  data[, value_rel := value / National_MT]
  
  
  library(plotly)
  ggplot(data[variable != 'Flag_MT'], 
         aes(x = value_rel, y = Name_ref, col = variable)) + 
    geom_point(aes(size = National_MT), alpha = 0.5) + 
    scale_color_colorblind()
  ggplotly()
  
  ggplot(data, aes(x = National_MT, y = value_rel, col = variable)) + 
    geom_point(alpha = 0.5) + 
    scale_x_log10() + 
    scale_y_log10() +
    scale_color_colorblind()
  
  ggplot(data[Name_ref != 'Marshall Islands' & value_rel < 0.2], 
         aes(fill = variable, y = value_rel, x = variable)) + 
    geom_boxplot()
  
  dcast(data, Name_ref + National_MT ~ variable, value.var = 'value') %>% 
    ggplot(aes(x = Operator_MT, y = Owner_MT)) + 
    geom_point(aes(size = National_MT)) + 
    geom_abline(slope = 1, intercept = 0, col = 'red')
  
}



# THE END ---------------------------------------------------------------------












