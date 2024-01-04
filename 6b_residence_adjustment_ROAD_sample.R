#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-12-14 15:49:45
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
library(truncnorm)
library(testthat)
library(countrycode)
library(arrow)

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

path2bridge <-  file.path(path2output, 'residence_adjustment_ROAD_prepare.feather') 
# path2edgar_samples <- file.path(path2output, 'sample_EDGAR.RData')
# path2unfccc_samples <- file.path(path2output, 'sample_UNFCCC.RData')
path2inventory_samples <- file.path(path2output, 'combine_UNFCCC_EDGAR_samples.RData') 

N <- config$sample_size
YEAR <- config$year


############################################################################## # 
##### Load data #############################################################
############################################################################## # 

# load sampled inventories
inventory <- readRDS(path2inventory_samples)
setkey(inventory, database, year, country_code, #EXIOBASE_region_code, 
       gas, 
       category_code, classification)




# load bridging data
country_bridge <- read_feather(path2bridge)
country_bridge[, year := as.character(year)]
setnames(country_bridge, 'country', 'country_code')

############################################################################## # 
##### 1. Sample brigding items #############################################################
############################################################################## # 

# Attach the total road emission samples
# --> serve as lower boundary so that road emissions after adjustment cannot
# be negative (was the case for e.g. LUX)

road <- copy(inventory[grepl('^1.A.3', category_code), .(database, year, country_code, 
                                                         gas, category_code, classification, 
                                                         sample)])
setkey(road, database, year, country_code, gas, category_code, classification)

# calcluate national total road emissions
road_totals <- road[, list(sample_totals = sum_samples(sample)), by = .(country_code, gas)]

country_bridge <- merge(country_bridge, road_totals, by = c('country_code', 'gas'), 
                        all.x = TRUE, 
                        sort = FALSE)

# remove countries without match (SRB), is later used to allocate residuals:
country_bridge <- country_bridge[sapply(sample_totals, length) == config$sample_size]

# Sample 
if (config$default_sampling == 'exp') {
  # Sample from exponential distribution (no uncertainty data available) =========
  country_bridge[, 
                 sample := list(lapply(NET_LAND, function(x) {
                   sign(x) * rexp(N, rate = 1/abs(x))
                 }))]  
} else if (config$default_sampling == 'norm') {
  # Sample from Normal distribution ==============================================
  # country_bridge[, 
  #                sample := list(lapply(NET_LAND, function(x) {
  #                  rtruncnorm(n = N, a = (-1)*unlist(sample_totals), 
  #                             mean = x, sd = config$default_cv * abs(x))
  #                }))]
  
  for (i in 1:nrow(country_bridge)) {
    
    country_bridge[i, sample := list(
      rtruncnorm(n = N, a = (-1)*unlist(country_bridge[i]$sample_totals), 
                 mean = NET_LAND, sd = config$default_cv * abs(NET_LAND))
    )]
  }
}



# Plot to check ================================================================
# unnest
if (FALSE) {
  
  data2plot <- country_bridge %>% 
    unnest(cols = sample) %>% 
    as.data.table
  data2plot[, NET_LAND := drop_units(NET_LAND)]
  
  ggplot(data2plot, aes(x = country_code, y = sample)) +
    facet_wrap(~gas, scales = 'free_y') + 
    geom_boxplot() +
    #geom_jitter(alpha = 0.4) + 
    geom_point(aes(y = NET_LAND), col = 'lightgreen', shape = 4) + 
    ylab('Residence Adjustment Road Transport [Gg]') + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  
  
}


############################################################################## # 
##### 2. Distribute residual over other European Countries #############################################################
############################################################################## # 
# Distribute residual over European Countries which do not publish bridging items
# according to their total road transport emissions (see stadler2018, supp 2, p28)


# list all countries with inventory data on road transport emissions
countries <- inventory[grepl('^1.A.3', category_code), 
                       list(emissions_road = sum(emissions)), 
                       by = .(country_code, #EXIOBASE_region_code, 
                              database, gas)] 

# add continent
countries[, continent := countrycode(country_code, 
                                     origin = 'iso3c', 
                                     destination = 'continent')]

# merge with bridging items to filter EUROPEAN countries WITHOUT bridging items
countries2 <- merge(countries, 
                    unique(country_bridge[, .(country_code, NET_LAND)]),
                    by = 'country_code', all = TRUE, allow.cartesian = TRUE)

europe_countries_missing <- countries2[is.na(NET_LAND) & continent == 'Europe'] %>% 
  .[, .(country_code, gas, emissions_road)] 

# calculate these countries' share of total road transp emissions
europe_countries_missing[, share := emissions_road / sum(emissions_road), by = gas]

test_that('shares sum to 1', {
  expect_true(all(europe_countries_missing[, sum(share), by = gas]$V1 == 1))
})

# Calculate unallocated bridging items (residuals)


bridge_residuals <- country_bridge[, -c('sample_totals', 'NET_LAND')] %>% 
  unnest_sample %>% 
  .[, list(residual_emissions = sum(value)), 
    by = .(year, gas, run)]


  
# swap sign (--> negative sign for all EU countries mean positive bridging item for rest of Europe)
bridge_residuals[, residual_emissions := -residual_emissions]

bridge_res_allocated <- merge(bridge_residuals, 
                              europe_countries_missing[, .(country_code, gas, share)], 
                              allow.cartesian = TRUE)

bridge_res_allocated[, emissions := share * residual_emissions]

test_that('all residuals are allocated', {
  expect_equal(bridge_res_allocated[, list(residual_emissions = sum(emissions)), 
                                    by = .(year, gas, run)], 
               bridge_residuals
  )
})

bridge_res_allocated[, sum(share), by = .(gas, run)]

bridge_res_allocated <- bridge_res_allocated[, list(sample = list(emissions)), 
                                             by = .(year, gas, country_code)]

# attach to bridging items

bridging_items <- rbindlist(list(country_bridge, bridge_res_allocated), fill = TRUE)


# test: should be all zeros
bridging_item_sum <- bridging_items %>% 
  unnest_sample() %>% 
  .[, sum(value, na.rm = TRUE), by = .(gas, run)]

test_that('bridging items are balanced (i.e. emissions over Europe sum to 0)', {
  expect_equal(bridging_item_sum$V1, rep(0, nrow(bridging_item_sum)))
})




############################################################################## # 
##### 3. combine with EDGAR/UNFCCC samples #############################################################
############################################################################## # 

# Assumption: all transport sectors are equally affected
# for each sample add (substract) the share of the bridging item that 
# correspondece to transport sectr
# e.g. for MC-run no. 1 (for one country/gas/database): 
# - 1.A.3.a makes 0.3 of all Road transport emissions, 1.A.3.b makes 0.7
# - Total Road transport bridging item is +1000 Gg
# - emissions for 1.A.3.a are: X_adj = X_orig + (0.3 * 1000 Gg)
# - for 1.A.3.b: X_adj = X_orig + (0.7 * 1000 Gg)

# grab road transport sectors from inventory samples
road <- copy(inventory[grepl('^1.A.3', category_code), .(database, year, country_code, 
                                                         gas, category_code, classification, 
                                                         sample)])
setkey(road, database, year, country_code, gas, category_code, classification)

# unnest road emissions
road <- unnest_sample(road)

# calculate the share of each sub-sector on total road transport emissions 
road[, share := value / sum(value), by = .(country_code, database, gas, run)]

# unnest bridging data as well
bridging_items <- unnest_sample(bridging_items[, -'NET_LAND'])
gc()
# merge road emissions with bridging data
road <- merge(road, bridging_items, 
              by = c('year', 'country_code', 'gas', 'run'), 
              suffixes = c('', '_bridge'))

# calculate the adjusted value as: value_original + (share * value_bridge)
road[, value_adj := value + (share * value_bridge)]

test_that("Residuence adjustment does not deliver negative emissions from road trans", {
expect_equal(nrow(road[value_adj < 0]), 0)
  
})
road[value_adj < 0, value_adj := 0]

# Nest sample
road <- road[, 
             list(sample_adj = c(.SD)), 
             by=.(year, country_code, gas, database, category_code, 
                  classification), 
             .SDcols = c('value_adj')
]

# Merge with original (unaltered inventory)
inventory_adj <- merge(inventory, road, all = TRUE, sort = FALSE,
                       by = c('year', 'country_code', 'gas', 'database', 
                              'category_code', 'classification'))



############################################################################## # 
##### Tests #############################################################
############################################################################## # 

countries_covered <- inventory_adj[!sapply(sample_adj, is.empty.list), 
                                   .(country_code)] %>% 
  unique
countries_covered[, continent := countrycode(country_code, 
                                             origin = 'iso3c', 
                                             destination = 'continent')]

not_covered <- inventory_adj[sapply(sample_adj, is.empty.list) & grepl('^1.A.3', category_code), 
                             .(country_code)] %>% 
  unique
not_covered[, continent := countrycode(country_code, 
                                       origin = 'iso3c', 
                                       destination = 'continent')]


# tests before doing the actual adjustment
test_that('only (and all) European countries covered with the bridging items', {
  expect_equal(unique(countries_covered[country_code != 'CYP']$continent), 
               'Europe')
  expect_false('Europe' %in% unique(not_covered$continent))
})

test_that('only category 1.A.3 emissions adjusted', {
  expect_equal(0, nrow(inventory_adj[!sapply(sample_adj, is.empty.list) & 
                                       !grepl('^1.A.3', category_code)]))
})


samples_orig <- inventory_adj[!sapply(sample_adj, is.empty.list), .(sample)] %>% 
  unnest_sample
samples_adj <- inventory_adj[!sapply(sample_adj, is.empty.list), .(sample_adj)] %>% 
  setnames('sample_adj', 'sample') %>% 
  unnest_sample

test_that("no emissions got lost", {
  expect_equal(samples_orig[, sum(value), by = run],
               samples_adj[, sum(value), by = run])
})

############################################################################## # 
##### save results #############################################################
############################################################################## # 
inventory_adj[!sapply(sample_adj, is.empty.list)]$category_code %>% unique
inventory_adj[sapply(proxies, is.data.table)]$proxies[[1]]
inventory_adj[grepl('1.A.3.b', category_code) & sapply(proxies, is.data.table)]$country_code %>% 
  unique

# change sample to adjusted sample whereever necessary (all 1.A.3 categories)
inventory_adj[!sapply(sample_adj, is.empty.list), 
              sample := sample_adj]
inventory_adj[, sample_adj := NULL]


save_results(inventory_adj)
#saveRDS(samples_dt2, './temp_results/4b_SEA_country_samples.RData')





# THE END ---------------------------------------------------------------------
