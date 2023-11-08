#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2023-09-08 16:41:18.818469
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(gtools)
library(ggthemes)
library(data.table)
library(ggplot2)
library(magrittr)
library(ggrepel)
library(purrr)
library(countrycode)
library(tidytext)
library(here)
library(callr)
library(cowplot)
library(kableExtra)
library(egg)
library(stringr)
library(arrow)

############################################################################## # 
##### load functions #################################################################
############################################################################## # 
source(file.path('src', 'functions_plot.R'))
source(file.path('src', 'functions.R'))


############################################################################## # 
##### settings #################################################################
############################################################################## # 

# 0. General settings ==========================================================
options("datatable.print.class" = TRUE)


# 1. read config and set paths ==========================================
config <- setup_config_and_log()
path2results <- config$path2output
path2plot <- file.path('figures', config$version)
if(!dir.exists(path2plot)) dir.create(path2plot)

# 2. Plots settings ============================================================

my_scale_fill <-scale_fill_colorblind()
my_cols <- (colorblind_pal()(8))

theme_simschul <- function(){ 
  #font <- "Georgia"   #assign font family up front
  
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      strip.background  = element_rect(fill = 'grey90', colour = NA)

    )
}
theme_set(theme_simschul())




############################################################################## # 
##### functions #################################################################
############################################################################## # 
geom_violin_box_jitter <- list(
  stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2, fill = 'white'), 
  geom_jitter(aes(size = mean_rel, alpha = mean_rel), shape=16,
              position=position_jitter(0.1, seed = 1)), 
  stat_summary(fun.data =  percentile, geom = 'errorbar', width = .2, 
               size = 1, col = my_cols[3]), 
  geom_violin(trim = TRUE, alpha = 0.2, fill = my_cols[7], col = NA), 
  stat_summary(fun.data =  median.quartile, geom = 'crossbar', width = .2, size = 0.7, alpha=0.4, fill = my_cols[3], col = my_cols[3]), 
  
  ylab('CV = sd / mean') , 
  xlab(''),  
  labs(size = 'Share of total', alpha = 'Share of total'), 
  theme(legend.position = 'bottom')
)

############################################################################## # 
##### load data #############################################################
############################################################################## # 
meta <- mRio::parse_EB3_metadata(config$path2exiobaseIOT)


# 1. GEA by industry ===========================================================

# loadign the data
gea_by_ind <- read_feather(file.path(path2results, 'prepare_EXIOBASE_samples_by_industry.feather'))

threshold <- 1E-6

# create labels
gea_by_ind[mean_rel > threshold, rank_cv := frankv((CI97.5 - CI2.5) / mean, 
                                                   order = -1L), 
           by = gas]
gea_by_ind[rank_cv < 10, label := paste0(EB_region, ': ', industry_code)]

# merge with exiobase data
exio_gea <- read_feather(file.path(path2results, 'prepare_EXIOBASE_gea.feather'))
setnames(exio_gea, 'value', 'value_exio')

gea_by_ind <- merge(gea_by_ind, exio_gea, by = c('gas', 'EB_region', 'industry_code'))

# mark if EXIO falls within or outside our 95%CI
gea_by_ind[value_exio >= CI2.5 & value_exio <= CI97.5, 
           type := 'within']
gea_by_ind[value_exio < CI2.5, 
           type := 'below']
gea_by_ind[value_exio > CI97.5, 
           type := 'above']
gea_by_ind[, type := factor(type, levels = c('above', 'within', 'below'))]



# 2. GEA by region =============================================================

# loadign the data
gea_by_reg <- read_feather(file.path(path2results, 'prepare_EXIOBASE_samples_by_region.feather'))

# create labels
gea_by_reg[, rank_ci := frankv((CI97.5 - CI2.5) / mean, order = -1L), by = gas]
gea_by_reg[rank_ci <= 5, label := EB_region]

#gea_by_reg[, rank_cv := frankv(cv, order = -1L), by = gas]
gea_by_reg[, rank_mean := frankv(mean, order = -1L), by = gas]
#gea_by_reg[rank_cv < 5 | rank_mean < 5, label := EB_region]

# merge with exiobase data
exio_by_reg <- read_feather(file.path(path2results, 'prepare_EXIOBASE_gea_by_reg.feather'))
setnames(exio_by_reg, 'value', 'value_exio')

gea_by_reg <- merge(gea_by_reg, exio_by_reg, by = c('gas', 'EB_region'))

# merge with OECD AESs
oecd_by_reg <- readRDS(file.path(path2results, 'prepare_oecd_AEA.RData'))
gea_by_reg <- merge(gea_by_reg, oecd_by_reg[activity == 'IND-TOTAL', -'activity'], by.x = c('EB_region', 'gas'), by.y = c('country', 'gas'), all.x = TRUE)

# merge with inventory data
unfccc_samples <- readRDS(file.path(path2results, 'sample_UNFCCC.RData'))
unfccc_by_reg <- unfccc_samples[, list(sample= sum_samples(sample), emissions_CRF = sum(emissions_CRF)), by = .(party, gas)]
unfccc_by_reg <- calculate_summary_statistics(unfccc_by_reg)
unfccc_by_reg[, EB_region := countrycode(party, 'iso3c', 'iso2c')]

edgar_samples <- readRDS(file.path(path2results, 'sample_EDGAR.RData'))
edgar_by_reg <- edgar_samples[, list(sample= sum_samples(sample), emissions_edgar = sum(emissions)), by = .(country_code, gas)]
edgar_by_reg <- calculate_summary_statistics(edgar_by_reg)
edgar_by_reg[, EB_region := countrycode(country_code, 'iso3c', 'iso2c')]

gea_by_reg <- merge(gea_by_reg, unfccc_by_reg[, .(gas, EB_region, emissions_CRF)], by = c('gas', 'EB_region'), all.x = TRUE)
gea_by_reg <-  merge(gea_by_reg, edgar_by_reg[, .(gas, EB_region, emissions_edgar)], by = c('gas', 'EB_region'), all.x = TRUE)

# mark if other AEA falls within or outside our 95%CI
gea_by_reg[value_exio >= CI2.5 & value_exio <= CI97.5, 
           type_exio := 'within']
gea_by_reg[value_exio < CI2.5, 
           type_exio := 'below']
gea_by_reg[value_exio > CI97.5, 
           type_exio := 'above']
gea_by_reg[, type_exio := factor(type_exio, levels = c('above', 'within', 'below'))]

gea_by_reg[emissions_OECD >= CI2.5 & emissions_OECD <= CI97.5, 
           type_oecd := 'within']
gea_by_reg[emissions_OECD < CI2.5, 
           type_oecd := 'below']
gea_by_reg[emissions_OECD > CI97.5, 
           type_oecd := 'above']
gea_by_reg[, type_oecd := factor(type_oecd, levels = c('above', 'within', 'below'))]

# 3. GEA by source category =====================================================
# loadign the data
gea_by_crf <- read_feather(file.path(path2results, 'prepare_EXIOBASE_samples_by_industry_and_CRF.feather'))
gea_by_reg_crf <- read_feather(file.path(path2results, 'prepare_EXIOBASE_samples_by_region_and_CRF.feather'))

gea_by_reg_crf[, sd_by_reg_crf := sd / sum(sd), 
               by = .(gas, EB_region)]

gea_by_reg_crf[, var := sd^2]

gea_by_reg[, var := sd^2]


gea_by_reg_crf <- merge(gea_by_reg_crf, gea_by_reg[, .(gas, EB_region, var)], 
                        by = c('gas', 'EB_region'), suffixes = c('', '_total'))

gea_by_reg_crf[, sobol_index := var / var_total]
gea_by_reg_crf$sobol_index %>% summary


# 4. Footprint by region =======================================================
fp_by_reg <- read_feather(file.path(path2results, 'prepare_footprints_by_region.feather'))

exio_fp_by_reg <- read_feather(file.path(path2results, 'prepare_EXIOBASE_fp_national.feather'))
setnames(exio_fp_by_reg, 'value', 'value_exio')

fp_by_reg <- merge(fp_by_reg, exio_fp_by_reg, by = c('gas', 'EB_region'))

# 5. GEA + FP by region (combine) ==============================================

full_by_reg <- rbindlist(list('gea' = gea_by_reg,
                              'fp' = fp_by_reg), 
                         idcol = 'type', use.names = TRUE, fill = TRUE)



# 6. Footprint multiplier ======================================================
fp_multiplier <- read_feather(file.path(path2results, 'prepare_footprints_by_sector.feather'))
setnames(fp_multiplier, 'j', 'id')


exio_Y <- readRDS(file.path(path2results, 'prepare_EXIOBASE_Y.RData'))
exio_Y <- data.table(id = 1:nrow(exio_Y), 
                     final_demand = rowSums(exio_Y))

#fp_multiplier[, id := id - 2]

# sector_names <- merge(meta$indices_A$colnames[, id := 1:.N], meta$industries[, .(Name, CodeNr)], 
#                       by.x = 'sector', by.y = 'Name', sort = FALSE)
# setnames(sector_names, 
#          c('region', 'sector', 'CodeNr'), 
#          c('EB_region', 'industry_name', 'industry_code'))
# 
# 
# fp_multiplier <- merge(fp_multiplier, sector_names,
#                        by = 'id', sort = FALSE)

fp_multiplier <- merge(fp_multiplier, exio_Y, by = 'id', sort = FALSE)

fp_multiplier[, total_emissions_mean := final_demand * mean]
fp_multiplier[, total_emissions_share := total_emissions_mean / sum(total_emissions_mean), 
              by = .(gas)]

fp_multiplier[total_emissions_share > 0.001, label := id]

# exclude all sectors with zero final demand: 
fp_multiplier <- fp_multiplier[final_demand > 0]

# 7. GEA + FP by industry =====================================================
gea_by_ind3 <- gea_by_ind[, .(gas, EB_region, industry_code, mean, 
                              cv, mean_rel)]
fp_multiplier3 <- fp_multiplier[, .(gas, EB_region, industry_code, 
                                    mean = total_emissions_mean, 
                                    cv, mean_rel = total_emissions_share)]


full_by_ind <- rbindlist(list(
  'GEA' = gea_by_ind3, 
  'FP' = fp_multiplier3
), idcol = 'type')

full_by_ind <- full_by_ind[is.finite(cv) & cv >= 0]


# 8. UNFCCC and EDGAR inventory ===================================================

# unfccc <- readRDS(file.path(path2results, 'merge_CRF_NIR_data.RData'))
# edgar <- readRDS(file.path(path2results, 'parse_EDGAR_uncertainty.RData'))
# 
# unfccc$cv_NIR %>% summary
# unfccc[cv_NIR < 0.001]
# 
# library(ggpmisc)
# 
# 
# ggplot(unfccc[cv_NIR>0], aes(x= emissions_CRF, y = cv_NIR, col = party)) + 
#   geom_point(alpha = 0.3, shape = 16) + 
#   geom_smooth(aes(group = party), method = 'glm', se = FALSE) + 
# #  geom_smooth(method = 'nls', formula = y ~ a * x^b, start = list(a=1,b=2),se=FALSE) + 
#  # geom_smooth(method="glm", aes(color="Exp Model"), formula= (y ~ exp(x)), 
# #              se=TRUE, linetype = 1) +
#   scale_x_log10() + 
#   scale_y_log10() + 
#   scale_color_viridis_d() + 
#   theme(legend.position = 'none') + 
#   facet_wrap(~gas, scales = 'free') 
# 
# ggplot(edgar[cv>0], aes(x= emissions_solazzo, y = cv, col = country_code)) + 
#   geom_point(alpha = 0.3, shape = 16) + 
#   geom_smooth(method = 'glm', se = FALSE) + 
#   #  geom_smooth(method = 'nls', formula = y ~ a * x^b, start = list(a=1,b=2),se=FALSE) + 
#   # geom_smooth(method="glm", aes(color="Exp Model"), formula= (y ~ exp(x)), 
#   #              se=TRUE, linetype = 1) +
#   scale_x_log10() + 
#   scale_y_log10() + 
#   scale_color_viridis_d() + 
#   theme(legend.position = 'none') + 
#   facet_wrap(~gas, scales = 'free') 
# 
# 
# 
# # create table with regression formula and r2
# unfccc_subset <- unfccc[cv_NIR > 0]
# fits_unfccc <- unfccc_subset[, list(fit = list(lm(log(cv_NIR) ~ log(emissions_CRF)))), 
#             by = .(gas, party)]
# fits_unfccc[, a := sapply(fit, function(x) exp(coef(x)[1]))]
# fits_unfccc[, b := sapply(fit, function(x) (coef(x)[2]))]
# fits_unfccc[, r2 := sapply(fit, function(x) summary(x)$r.squared)]
# fits_unfccc[, equation := sprintf("$y = %.2fx^{%.2f}$", a, b)]
# fits_unfccc[, "$R^2$" := sprintf("%.3f", r2)]
# 
# 
# edgar_subset <- edgar[cv > 0]
# fits_edgar <- edgar_subset[, list(fit = list(lm(log(cv) ~ log(emissions_solazzo)))), 
#                     by = .(gas, country_code)]
# fits_edgar[, a := sapply(fit, function(x) exp(coef(x)[1]))]
# fits_edgar[, b := sapply(fit, function(x) (coef(x)[2]))]
# fits_edgar[, r2 := sapply(fit, function(x) summary(x)$r.squared)]
# fits_edgar[, equation := sprintf("$y = %.2fx^{%.2f}$", a, b)]
# fits_edgar[, "$R^2$" := sprintf("%.3f", r2)]
# 
# fits <- rbindlist(list(unfccc = fits_unfccc, edgar = fits_edgar), 
#           idcol = 'database')
# 
# ggplot(fits, aes(x = gas, y = r2, fill = database, col = database)) + 
#   geom_boxplot(alpha = 0.5) + 
#   scale_fill_colorblind7() + 
#   scale_color_colorblind7() + 
#   theme(legend.position = 'bottom')
# 
# ggsave2(filename = "r2_boxplot.pdf", height = 4, width = 7)
# 
# 
# for (iparty in unique(unfccc$party)) {
#   for (igas in unique(unfccc$gas)) {
#     # Subset the data for current gas
#     current_data <- unfccc[cv_NIR > 0 & gas == igas & party == iparty]
#     
#     # Fit the model
#     fit <- lm(log(cv_NIR) ~ log(emissions_CRF), data=current_data)
#     
#     # Extract coefficients
#     a <- exp(coef(fit)[1])
#     b <- coef(fit)[2]
#     
#     # Extract R-squared value
#     r_squared <- summary(fit)$r.squared
#     
#     # Create the equation label
#     #label <- TeX(sprintf("$y = %.2fx^{%.2f}$", a, b), output = 'character')
#     equation <- sprintf("$y = %.2fx^{%.2f}$", a, b)
#     r2_label <- sprintf("$R^2 = %.3f$", r_squared)
#   }
# }
# 
# 
# 
# library(ggplot2)
# library(gridExtra) # to arrange multiple plots
# library(ggtext)
# library(latex2exp)
# data_subset <- unfccc[unfccc$cv_NIR > 0 & party == 'ITA' ]
# 
# # Unique gases
# gases <- unique(data_subset$gas)
# 
# plot_list <- list()
# 
# for(igas in gases) {
#   
#   # Subset the data for current gas
#   current_data <- subset(data_subset, gas == igas)
#   
#   # Fit the model
#   fit <- lm(log(cv_NIR) ~ log(emissions_CRF), data=current_data)
#   
#   # Extract coefficients
#   a <- exp(coef(fit)[1])
#   b <- coef(fit)[2]
#   
#   # Extract R-squared value
#   r_squared <- summary(fit)$r.squared
#   
#   # Create the equation label
#   #label <- TeX(sprintf("$y = %.2fx^{%.2f}$", a, b), output = 'character')
#   equation <- sprintf("$y = %.2fx^{%.2f}$", a, b)
#   r2_label <- sprintf("$R^2 = %.3f$", r_squared)
#   label <- TeX(paste(equation, ",\n", r2_label), output = 'character')
#   
#   # Create plot for current gas
#   p <- ggplot(current_data, aes(x=emissions_CRF, y=cv_NIR)) + 
#     geom_point(alpha = 0.3, shape = 16, col = 'grey30') + 
#     geom_smooth(method = 'lm', formula = y ~ x,
#                 se = FALSE, col = my_cols[7]) +
#     scale_x_log10() + 
#     scale_y_log10() + 
#     labs(title = igas) +
#     ylab("CV") + 
#     xlab('Emissions') + 
#   # annotate(geom='text', x=3, y=3, label=TeX("$\\hat{Y} = B_0 + B_1X_1", 
#   #output='character'), parse=TRUE) 
#      annotate("text", x = Inf, y = Inf, label = label, 
#               hjust = 1.1, vjust = 1.1, parse = TRUE)
#   
#   if (igas != 'CO2') {
#     p <- p + theme(axis.title.x=element_blank())  
#   }
#   
#   if (igas != 'CH4') {
#     p <- p + theme(axis.title.y=element_blank())  
#   }
#   
#   # Append plot to the list
#   plot_list[[igas]] <- p
# }
# 
# # Combine plots
# ggpubr::ggarrange(plotlist = plot_list, nrow = 1,align = 'h')

############################################################################## # 
##### Plots #############################################################
############################################################################## # 

# 1. by region =================================================================

# _a) GEA + footprint ==========================================================

#| fig-cap: "."
#| fig:height: 11 
#| fig:width: 8


# __i. Prepare data ============================================================

setorder(full_by_reg, type, gas, mean)
full_by_reg[, right := cumsum(mean)/ sum(mean), by = .(type, gas)]
full_by_reg[, left := right - (mean / sum(mean)), by = .(type, gas)]

full_by_reg[, type2 := factor(type, levels = c('gea', 'fp'), 
                              labels = 'Production-based', 'Consumption-based')]

# __ii. Plot ============================================================

ggplot(full_by_reg, aes(xmin = left, xmax = right, ymax = (CI97.5 / mean) - 1, ymin = (CI2.5 / mean) - 1, y = (median / mean) - 1)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_rect(col = 'grey50', size = 0.2, fill = my_cols[2], alpha = 0.7) + 
  scale_shape_manual(name = "Legend", values = c('Median' = 3, 'CV' = 4, 'EXIOBASE' = 17)) +
  scale_color_manual(name = "Legend", values = c('Median' = 1, 'CV' = my_cols[7], 'EXIOBASE' = my_cols[3])) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  geom_text_repel(aes(label = EB_region, y = (CI97.5 / mean) - 1, x = left + (right - left) / 2), size = 2.3, position = position_nudge_repel(y = 0.1), col = my_cols[4], max.overlaps = 15) + 
  facet_grid(rows = vars(gas), 
             cols = vars(factor(type, levels = c('gea', 'fp'), 
                                labels = c('GHG emission accounts', 'GHG footprints'))), 
             scales = 'free_y', space = 'fixed') + 
  ylab('% deviation from sample mean') + 
  xlab('Share of total emissions') + 
  theme(legend.position = 'bottom', 
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.background  = element_rect(fill = 'grey90', colour = NA))

# __iii. Save ============================================================

ggsave2(filename = "full_by_reg.pdf", height = 6, width = 7)


# _b) GEA against other databases ==============================================

(p <- ggplot(gea_by_reg, aes(x = EB_region, ymax = (CI97.5 / mean) - 1, ymin = (CI2.5 / mean) - 1, y = (median / mean) - 1)) +
   geom_hline(yintercept = 0, linetype = 'dotted') +
   #geom_errorbar(width = 0.05, alpha = 0.7) + 
   geom_linerange(col = 'grey60', alpha = 0.7, size = 3) + 
   #  geom_point(aes(shape = 'Median', col = 'Median')) + 
   geom_point(aes(y = (value_exio / mean) - 1, col = 'EXIOBASE', shape = 'EXIOBASE'), size = 2) +
   geom_point(aes(y = (emissions_CRF / mean)-1, shape = 'UNFCCC', col = 'UNFCCC'))+
   geom_point(aes(y = (emissions_edgar / mean)-1, shape = 'EDGAR', col = 'EDGAR'))+
   geom_point(aes(y = (emissions_OECD / mean)-1, shape = 'OECD', col = 'OECD'))+
   scale_shape_manual(name = "Legend", values = c('UNFCCC' = 3, 'EDGAR' = 4, 'EXIOBASE' = 17, 'OECD' = 14, 'Median' = 12)) +
   scale_color_manual(name = "Legend", values = c('UNFCCC' = 1, 'EDGAR' = my_cols[7], 'EXIOBASE' = my_cols[3], 'OECD' = my_cols[2], 'Median' = my_cols[4])) +
   #scale_x_log10() +
   scale_y_continuous(labels = scales::percent)+
   scale_x_reordered() + 
   facet_wrap(~gas, scales = 'free_y', ncol = 1, strip.position = 'right') + 
   ylab('deviation from sample mean [%]') + 
   theme(legend.position = 'bottom', legend.title = element_blank(), 
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
   xlab('Country / Region'))

# __iii. Save ============================================================

ggsave2(filename = "gea_by_reg.pdf", height = 6, width = 6)

ggsave_data(gea_by_reg[, .(gas, EB_region, mean, 
                           CI2.5 = (CI2.5/mean)-1, 
                           CI97.5 = (CI97.5/mean)-1,
                           cv, 
                           EXIOBASE = (value_exio/mean)-1, 
                           UNFCCC = (emissions_CRF/mean)-1, 
                           OECD = (emissions_OECD/mean)-1, 
                           EDGAR = (emissions_edgar/mean)-1)], 
            filename = 'gea_by_reg.csv')


# 2. by sectors (multiplier) ===================================================


# _a) GEA ====================================================================

#| fig-cap: "The relative standard error (CV) of indidivual GEA coefficients (orange line, x-axis) by the cumulative share of total emissions (green boxes, y-axis). The coefficients are sorted along the y-axis by their CV from high (bottom) to low (top). To ease interpretation, the dotted line mark a CV of 0.1 and the dashed line a CV of 0.5. "
#| fig:height: 11 
#| fig:width: 8

# __i. prepare data ===========================================================

gea_by_ind2 <- gea_by_ind[cv > 0 & !(mean == 0 & sd == 0)]
setorder(gea_by_ind2, gas, -cv)
gea_by_ind2[, right := cumsum(mean) / sum(mean), by = gas]
gea_by_ind2[, left := right - (mean / sum(mean)), by = .(gas)]



# # boxplot data
# ylim <- 10
# boxplot_data <- gea_by_ind2[, 
#                             list(boxplot.stats(cv)$stats), by = gas] %>% 
#   .[, variable := rep(c("ymin", "lower", "middle", "upper", "ymax"), 3)] %>% 
#   dcast(gas ~ variable, value.var = 'V1') %>% 
#   cbind(gea_by_ind2[, list(xmin = (-0.1), 
#                            xmax = (-0.05), 
#                            y2.5 = quantile(cv, 0.025, na.rm=TRUE), 
#                            y97.5 = quantile(cv, 0.975, na.rm=TRUE)), 
#                     by = gas][,2:5])
# yrange <- 1/30
# #boxplot_data[, xlower := 10^ (log10(xmin) - yrange * (log10(xmax) - log10(xmin)))]
# #boxplot_data[, xupper := xmin]
# 
# 
# gea_by_ind2[gas == 'CO2']$cv %>% log10 %>% hist
# 
# quantile(probs = c(0.025, 0.5, 0.975))

# __ii. plot ================================================================

tol <- 1E-7

ggplot(gea_by_ind2[mean_rel > tol], 
       aes(y = cv, x = left + (right - left) / 2)) +
  geom_rect(aes(xmin = left, xmax = right, fill = mean_rel, col = mean_rel), ymin = min(gea_by_ind[mean_rel > tol & cv >=0]$cv %>% log10), ymax = max(gea_by_ind[mean_rel > tol & cv >=0]$cv %>% log10),  
            #col = my_cols[4], size = 0.2, fill = my_cols[4], 
            col = NA, 
            alpha = 0.7) + 
  
  #geom_text_repel(data = gea_by_ind[, list(x = max(right)/2, y = max(gea_by_ind[, cv], na.rm = TRUE)), by = gas], 
  #aes(label = gas, x = x, y = y), col = 'black') + 
  # geom_linerange(data = boxplot_data, 
  #                aes(ymax = ymax, ymin = ymin, x = xmin), inherit.aes = FALSE) +
  # geom_crossbar(data = boxplot_data, 
  #               aes(ymax = upper, ymin = lower, y = middle, x = xmin), 
  #               fill = 'white', inherit.aes = FALSE, width = 0.1)  +
  #geom_violin(aes(y = cv, x = 0), inherit.aes = FALSE) + 
  geom_boxplot(aes(y = cv, x = -0.1), width = 0.1, inherit.aes = FALSE, 
               outlier.alpha = 0.3, outlier.colour = 'grey30', 
               outlier.shape = 16, outlier.size = 0.5) + 
  geom_hline(yintercept = 0.1, linetype = 'dotted') +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  geom_line(aes(col = mean_rel), col = my_cols[7], linewidth = 0.5) + 
  facet_wrap(~gas, scales = 'fixed', ncol = 3, strip.position = 'top') + 
  ylab('CV = SD / mean') + 
  xlab('cumulative share of total emissions [%]') +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01), trans = 'log10' )+
  #coord_flip() +
  scale_fill_viridis_c(direction=1L, trans = "log10") + 
  scale_color_viridis_c() + 
  #scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  labs(fill = "Mean share of total emissions")+
  theme(legend.position = 'bottom',
        #strip.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.background  = element_rect(fill = 'grey90', colour = NA), 
        strip.placement = 'inside', strip.clip = 'off')

`# __iii. Save ============================================================

ggsave2(filename = "gea_by_ind.pdf", height = 4, width = 7)




# _b) Multiplier ===============================================================
# __i. prepare data ===========================================================

fp_multiplier2 <- fp_multiplier[cv > 0 & !(mean == 0 & sd == 0)]
setorder(fp_multiplier2, gas, -cv)
fp_multiplier2[, right := cumsum(total_emissions_mean) / sum(total_emissions_mean), 
               by = gas]
fp_multiplier2[, left := right - (total_emissions_mean / sum(total_emissions_mean)), 
               by = .(gas)]



# __ii. plot ================================================================

tol <- 1E-9

ggplot(fp_multiplier2[mean_rel > tol], 
       aes(y = cv, x = left + (right - left) / 2)) +
  geom_rect(aes(xmin = left, xmax = right, 
                fill = total_emissions_share, col = total_emissions_share), 
            ymin = min(fp_multiplier2[mean_rel > tol & cv >=0]$cv %>% log10), 
            ymax = max(fp_multiplier2[mean_rel > tol & cv >=0]$cv %>% log10),  
            #col = my_cols[4], size = 0.2, fill = my_cols[4], 
            col = NA, 
            alpha = 0.7) + 
  
  #geom_text_repel(data = gea_by_ind[, list(x = max(right)/2, y = max(gea_by_ind[, cv], na.rm = TRUE)), by = gas], 
  #aes(label = gas, x = x, y = y), col = 'black') + 
  # geom_linerange(data = boxplot_data, 
  #                aes(ymax = ymax, ymin = ymin, x = xmin), inherit.aes = FALSE) +
  # geom_crossbar(data = boxplot_data, 
  #               aes(ymax = upper, ymin = lower, y = middle, x = xmin), 
  #               fill = 'white', inherit.aes = FALSE, width = 0.1)  +
  #geom_violin(aes(y = cv, x = 0), inherit.aes = FALSE) + 
  geom_boxplot(aes(y = cv, x = -0.1), width = 0.1, inherit.aes = FALSE, 
               outlier.alpha = 0.3, outlier.colour = 'grey30', 
               outlier.shape = 16, outlier.size = 0.5) + 
  geom_hline(yintercept = 0.1, linetype = 'dotted') +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  geom_line(aes(col = mean_rel), col = my_cols[7], linewidth = 0.5) + 
  facet_wrap(~gas, scales = 'fixed', ncol = 3, strip.position = 'top') + 
  ylab('CV = SD / mean') + 
  xlab('cumulative share of total emissions [%]') +
  #scale_y_log10() + 
  scale_y_continuous(labels = scales::label_number(), trans = 'log10' )+
  #coord_flip() +
  scale_fill_viridis_c(direction=1L, trans = "log10") + 
  scale_color_viridis_c() + 
  #scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  labs(fill = "Mean share of total emissions")+
  theme(legend.position = 'bottom',
        #strip.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.background  = element_rect(fill = 'grey90', colour = NA), 
        strip.placement = 'inside', strip.clip = 'off')

`# __iii. Save ============================================================

ggsave2(filename = "fp_by_ind.pdf", height = 4, width = 7)



# _c) GEA + multiplier =========================================================



# __i. prepare data ===========================================================
full_by_ind[, sum(mean), by = .(gas, type)]


tol <- 0
setorder(full_by_ind, gas, -cv)
full_by_ind[, right := cumsum(mean) / sum(mean), by = .(gas, type)]
full_by_ind[, left := right - (mean / sum(mean)), by = .(gas, type)]


full_by_ind$mean_rel %>% summary
full_by_ind[!is.finite(mean_rel)]

# __ii. plot ================================================================

full_by_ind[mean_rel < 0]

ggplot(full_by_ind[mean_rel >= tol], 
       aes(y = cv, x = left + (right - left) / 2)) +
  geom_rect(aes(xmin = left, xmax = right, fill = mean_rel, col = mean_rel), 
            ymin = min(full_by_ind[mean_rel >= tol]$cv %>% log10), 
            ymax = max(full_by_ind[mean_rel >= tol]$cv %>% log10),  
            #col = my_cols[4], size = 0.2, fill = my_cols[4], 
            col = NA, 
            alpha = 0.4) + 
  geom_boxplot(aes(y = cv, x = -0.1), width = 0.1, inherit.aes = FALSE, 
               outlier.alpha = 0.3, outlier.colour = 'grey30', 
               outlier.shape = 16, outlier.size = 0.5) + 
  geom_hline(aes(yintercept = 0.1, linetype = 'CV = 0.1')) +
  geom_hline(aes(yintercept = 0.5, linetype = 'CV = 0.5')) +
  geom_line(aes(col = mean_rel), col = my_cols[7], linewidth = 0.5) + 
  #facet_wrap(~gas+type, scales = 'fixed', ncol = 3, strip.position = 'top') + 
  facet_grid(rows = vars(gas), 
             cols = vars(factor(type, levels = c('GEA', 'FP'), 
                                labels = c('GHG emission accounts', 'GHG footprints')))) + 
  ylab('CV = SD / mean') + 
  xlab('cumulative share of total emissions [%]') +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01), trans = 'log10' )+
  #coord_flip() +
  #  scale_fill_viridis_c(direction=1L, trans = "log10") + 
  scale_fill_viridis_c(direction=1L, alpha = 0.4) + 
  scale_color_viridis_c(direction=1L, alpha = 0.4) + 
  #scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  scale_linetype_manual(name = "Legend", 
                        values = c('CV = 0.1' = 2, 
                                   'CV = 0.5' = 3))+
  labs(fill = "Mean share of total emissions")+
  theme(legend.position = 'bottom', 
        legend.background = element_rect(fill = 'grey97'),
        #strip.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.background  = element_rect(fill = 'grey90', colour = NA), 
        strip.placement = 'inside', strip.clip = 'off') +
  guides(linetype=guide_legend(nrow=2, byrow=TRUE, title.position = 'top'), 
         fill = guide_colourbar(title.position = "top", barwidth = 10)) 

`# __iii. Save ============================================================

ggsave2(filename = "full_by_ind.pdf", height = 5, width = 7)
ggsave2(filename = "full_by_ind.png", height = 5, width = 7, device = 'png')



# _d) ABS: GEA against EXIOBASE ====================================================

# __i. Tables =================================================================

temp <- gea_by_ind#[gas == 'CO2'] 
temp[, sum_by_ind := sum(mean), by = industry_code]
temp <- merge(temp, meta$industries[, .(CodeNr, Name)], by.x = 'industry_code', by.y= 'CodeNr', all.x = TRUE)
#temp <- temp[Name != "Re-processing of secondary preciuos metals into new preciuos metals"]

temp[, absdif := mean - value_exio]
temp[, reldif := absdif / value_exio]

temp2 <- temp[, list(Median = median(reldif),
                     "$Q_{0.25}$" = quantile(reldif, 0.25, na.rm = TRUE),
                     "$Q_{0.75}$" = quantile(reldif, 0.75, na.rm = TRUE)),
              by = .(Name, industry_code, gas)]

setnames(temp2, c('Name', 'industry_code'), c("Industry name", 'Industry code'))

temp2[nchar(`Industry name`) > 80, `Industry name` := paste0(strtrim(`Industry name`, 80), '...')]

temp2[gas == 'CO2' & `$Q_{0.25}$` > 0, -'gas'] %>% 
  setorder(-Median) %>% 
  kableExtra::kbl(digits = 2,escape = FALSE, format = 'latex', booktabs = FALSE, 
                  toprule = '\\tophline', midrule = '\\middlehline', 
                  bottomrule = '\\bottomhline', linesep = '', vline = '',  
                  caption = "Industry sectors for which our sample means are 
                  considerably (i.e. for more than 75\\% of all regions) ABOVE 
                  the official EXIOBASE V3.8.2 estimate. Numeric values depict 
                  the median, 25\\%-, and 75\\%-Quantiles, respectively, 
                  of the sector-wise relative differences between our sample mean 
                  and the official EXIOBASE V3.8.2 estimate. A median of 19, 
                  for example, means that the median relative difference for 
                  that specific industry sector among all 49 regions is factor 
                  19 compared to the official EXIOBASE estimates. CO2 only.") %>% 
  write(file = file.path(path2plot, 'table_overest.tex'))
#View
#knitr::kable(digits = 2, caption = "Industry sectors for which our sample means are considerably (i.e. for more than 75\\% of all regions) ABOVE the official EXIOBASE V3.8.2 estimate. Numeric values depict the median, 25\\%-, and 75\\%-Quantiles, respectively, of the sector-wise relative differences between our sample mean and the official EXIOBASE V3.8.2 estimate. A median of 19, for example, means that the median relative difference for that specific industry sector among all 49 regions is factor 19 compared to the official EXIOBASE estimates. CO2 only.")


temp2[gas == 'CO2' & `$Q_{0.75}$` < 0, -'gas'] %>% 
  setorder(Median) %>% 
  #View
  kableExtra::kable(digits = 2,escape = FALSE, format = 'latex', booktabs = FALSE, 
                    toprule = '\\tophline', midrule = '\\middlehline', bottomrule = '\\bottomhline', 
                    vline='', linesep = '',  
                    caption = "Industry sectors for which our sample means are considerably 
  (i.e. for more than 75\\% of all regions) BELOW the official EXIOBASE V3.8.2 
  estimate. Numeric values depict the median, 25\\%-, and 75\\%-Quantiles, 
  respectively, of the sector-wise relative differences between our sample mean 
  and the official EXIOBASE V3.8.2 estimate. A median of -0.9, for example, means
  that the median relative difference for that specific industry sector among
  all 49 regions is factor -0.9 compared to the official EXIOBASE estimates. 
  CO2 only.") %>% 
  write(file = file.path(path2plot, 'table_underest.tex'))



# __ii. Plots =================================================================

#| fig-cap: "Devitaion from sample mean for individual coefficients. Sectors with a relative contribution of less than 0.00001% of total global emissions are not plotted since they show partly deviation of up to 3000%.."
#| fig:height: 11 
#| fig:width: 8
setorder(gea_by_ind, gas, median)
gea_by_ind[, right := cumsum(mean) / sum(mean), by = gas]
gea_by_ind[, left := right - (mean / sum(mean)), by = .(gas)]
# this one!


data_prop <- gea_by_ind[mean > 0 & value_exio > 0, list(mean_emissions = sum(mean), 
                                                        max = max(CI97.5, value_exio)), 
                        by = .(gas, type)] %>% 
  .[, share_emissions := mean_emissions / sum(mean_emissions), by = gas] %>% 
  .[, max := max(max), by = gas] %>%
  .[, max := max + (max/20)] %>% 
  .[, cumsum := cumsum(share_emissions), by = gas] %>% 
  .[] 



library(ggforce)
p <- list()
for (igas in c('CH4', 'CO2', 'N2O')) {
  ymax <- gea_by_ind[gas == igas & mean > 0 & 
                       value_exio > 0 & right < 0.27, 
                     .(CI97.5, value_exio)] %>% unlist %>% max
  ymax_abs <-  gea_by_ind[gas == igas & mean > 0 & 
                            value_exio > 0, 
                          .(CI97.5, value_exio)] %>% unlist %>% max
  print(ymax_abs)
  p[[igas]] <-  ggplot(gea_by_ind[gas == igas & mean > 0 & value_exio > 0],
                       aes(y = (median / mean) - 1, 
                           x = left + (right - left) / 2)) +
    geom_rect(aes(xmin = left, xmax = right, ymax = (CI97.5), ymin = (CI2.5)), 
              col = 'grey40', size = 0.2, fill = 'grey60', alpha = 0.7) + 
    #geom_text_repel(data = gea_by_ind[, list(x = max(right)/2, y = max(gea_by_ind[, (CI97.5 / mean) - 1], na.rm = TRUE) - 1), by = gas], aes(label = gas, x = x, y = y), col = 'black') + 
    #geom_point(aes(y = (value_exio ), col = 'EXIOBASE', shape = 'EXIOBASE'), 
    #           size = 1.2, alpha = 0.8) +
    geom_point(aes(y = (value_exio), col = type, shape = type),
               size = 1.2, alpha = 0.8) +
    geom_segment(aes(y = median, x = left, xend = right, yend = median), 
                 col = 'grey30') +
    geom_col(data = data_prop[gas == igas], 
             aes(x = share_emissions,y = max, fill = type),
             orientation = 'y', width = ymax_abs / 20, 
             alpha = 0.4) + 
    
    #scale_shape_manual(name = "Legend", values = c('Median' = 3, 'CV' = 4, 'EXIOBASE' = 17)) +
    #scale_color_manual(name = "Legend", values = c('Median' = 1, 'CV' = my_cols[7], 
    #                                               'EXIOBASE' = my_cols[4])) +
    scale_color_colorblind7() +  
    scale_fill_colorblind7() +  
    facet_zoom(xlim = c(0, 0.25), ylim = c(0, ymax), zoom.size = 0.4) + 
    #facet_wrap(~gas, scales = 'free_y', ncol = 1, strip.position = 'right') + 
    ylab('emissions [Gg]') + 
    xlab('cumulative share of total emissions [%]') + 
    #scale_y_continuous(labels = scales::percent)+
    #scale_y_log10() + 
    scale_x_continuous(labels = scales::percent)+
    ggtitle(igas) +
    theme(#legend.position = 'none',
      #legend.title = element_blank(), 
      plot.margin = margin(t = 1, b = 1, l = 2, r = 2)
    )
  
  if (igas != 'CO2') {
    p[[igas]] <- p[[igas]] + 
      theme(axis.title.y = element_blank())
  }
  if (igas != 'N2O') {
    p[[igas]] <- p[[igas]] + 
      theme(axis.title.x = element_blank() )
  }
  
  
}

#plot_grid(plotlist = p, ncol = 1, align = 'v')
(pfull <- ggpubr::ggarrange(plotlist = p, ncol = 1, common.legend = TRUE,
                            align = 'v',
                            legend = 'bottom'))


`# __iii. Save ============================================================

ggsave2(plot = pfull, filename = "gea_by_ind_abs.pdf", height = 5, width = 7)



# _e) REL: GEA agai9nst EXIOBASE, version 2=========================================

setorder(gea_by_ind, gas, mean)
gea_by_ind[, right := cumsum(mean) / sum(mean), by = gas]
gea_by_ind[, left := right - (mean / sum(mean)), by = .(gas)]

tol <- 1E-9

data_prop <- gea_by_ind[mean_rel > tol & mean > 0 & value_exio > 0, 
                        list(mean_emissions = sum(mean), 
                             max = max((CI97.5/mean)-1, (value_exio/mean)-1)), 
                        by = .(gas, type)] %>% 
  .[, share_emissions := mean_emissions / sum(mean_emissions), by = gas] %>% 
  .[, max := max(max), by = gas] %>%
  .[, max := max + (max/20)] %>% 
  .[, cumsum := cumsum(share_emissions), by = gas] %>% 
  .[] 

p <- list()
for (igas in c('CH4', 'CO2', 'N2O')) {
  xlower <- 0.25
  
  
  ymax <- gea_by_ind[mean_rel > tol & gas == igas & mean > 0 & 
                       value_exio > 0 & right > xlower, 
                     .((CI97.5/mean)-1)] %>% unlist %>% max
  ymax_abs <- gea_by_ind[mean_rel > tol & gas == igas & mean > 0 & 
                           value_exio > 0, 
                         .((CI97.5/mean)-1, (value_exio/mean)-1)] %>% unlist %>% max
  
  
  p[[igas]] <-  ggplot(gea_by_ind[mean_rel > tol & gas == igas & mean > 0 & value_exio > 0], 
                       aes(y = (median / mean) - 1, x = left + (right - left) / 2)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    geom_rect(aes(xmin = left, xmax = right, ymax = (CI97.5 / mean) - 1, 
                  ymin = (CI2.5 / mean) - 1), col = 'grey30', 
              size = 0.2, fill = 'grey60', alpha = 0.7) + 
    geom_point(aes(y = (value_exio / mean) - 1, col = type, shape = type, 
                   alpha = mean_rel),
               size = 1.2, alpha= 0.7) +
    # geom_col(data = data_prop[gas == igas],
    #          aes(x = share_emissions,y = max, fill = type),
    #          orientation = 'y', width = ymax_abs / 20,
    #          alpha = 0.6) +
    # 
    scale_color_colorblind7() +
    scale_fill_colorblind7() + 
    ylab('deviation from sample mean [%]') + 
    xlab('cumulative share of total emissions [%]') + 
    labs(col = 'EXIOBASE [...] our 95% CI', fill = 'EXIOBASE [...] our 95% CI', shape = 'EXIOBASE [...] our 95% CI') + 
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(labels = scales::percent)+
    facet_zoom(xlim = c(xlower, 1), ylim = c(-1, ymax), zoom.size = 2.5, 
               horizontal = TRUE) + 
    ggtitle(igas) +
    guides(alpha = 'none') + 
    theme(#legend.position = 'none',
      #legend.title = element_blank(),
      
      plot.margin = margin(t = 1, b = 1, l = 3, r = 3)
      
    )
  p[[igas]]
  if (igas != 'CO2') {
    p[[igas]] <- p[[igas]] +
      theme(axis.title.y = element_blank())
  }
  if (igas != 'N2O') {
    p[[igas]] <- p[[igas]] +
      theme(axis.title.x = element_blank() )
  }
  
}
(pfull <- ggpubr::ggarrange(plotlist = p, ncol = 1, align = 'v', 
                            common.legend = TRUE, legend = 'bottom'))



`# __iii. Save ============================================================

ggsave2(plot = pfull, filename = "gea_by_ind_rel.pdf", height = 5, width = 7)


# _f) GEA agai9nst EXIOBASE, version 2=========================================

gea_by_ind[, dev_exio := (value_exio/mean)-1]

setorder(gea_by_ind, gas, type, -dev_exio)
gea_by_ind[, right := cumsum(mean) / sum(mean), by = gas]
gea_by_ind[, left := right - (mean / sum(mean)), by = .(gas)]

gea_by_ind[type == 'over' & dev_exio < 0]
gea_by_ind[mean > CI97.5]

p <- list()
for (igas in c('CH4', 'CO2', 'N2O')) {
  xlower <- 0.1
  
  
  ymax <- gea_by_ind[mean_rel > 1E-9 & gas == igas & mean > 0 & 
                       value_exio > 0 & right > xlower, 
                     .((CI97.5/mean)-1, (value_exio/mean)-1)] %>% unlist %>% max
  p[[igas]] <-  ggplot(gea_by_ind[mean_rel > 1E-9 
                                  & gas == igas 
                                  & mean > 0 
                                  & value_exio > 0], 
                       aes(y = (median / mean) - 1, x = left + (right - left) / 2)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    geom_rect(aes(xmin = left, xmax = right, ymax = (CI97.5 / mean) - 1, 
                  ymin = (CI2.5 / mean) - 1), col = 'grey30', 
              size = 0.2, fill = 'grey60', alpha = 0.7) + 
    #geom_text_repel(data = gea_by_ind[, list(x = max(right)/2, y = max(gea_by_ind[, (CI97.5 / mean) - 1], na.rm = TRUE) - 1), by = gas], aes(label = gas, x = x, y = y), col = 'black') + 
    
    geom_point(aes(y = (value_exio / mean) - 1, col = type, shape = type),
               size = 1.2, alpha = 0.8) +
    # scale_shape_manual(name = "Legend", values = c('Median' = 3, 'CV' = 4, 'EXIOBASE' = 17)) +
    #scale_color_manual(name = "Legend", values = c('Median' = 1, 'CV' = my_cols[7], 'EXIOBASE' = my_cols[4])) +
    #facet_wrap(~gas, scales = 'fixed', ncol = 1, strip.position = 'bottom') + 
    scale_color_colorblind7() + 
    ylab('deviation from sample mean [%]') + 
    xlab('cumulative share of total emissions [%]') + 
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(labels = scales::percent)+
    facet_zoom(xlim = c(xlower, 1), ylim = c(-1, ymax), zoom.size = 2.5, 
               horizontal = TRUE) + 
    #facet_wrap(~gas, scales = 'free_y', ncol = 1, strip.position = 'right') + 
    #scale_y_continuous(labels = scales::percent)+
    #scale_y_log10() + 
    
    ggtitle(igas) +
    theme(#legend.position = 'none',
      #legend.title = element_blank(),
      
      plot.margin = margin(t = 1, b = 1, l = 3, r = 3)
      
    )
  p[[igas]]
  if (igas != 'CO2') {
    p[[igas]] <- p[[igas]] +
      theme(axis.title.y = element_blank())
  }
  if (igas != 'N2O') {
    p[[igas]] <- p[[igas]] +
      theme(axis.title.x = element_blank() )
  }
  
  
  # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
  
}
#p[[1]]
#plot_grid(plotlist = p, ncol = 1, align = 'v')
(pfull <- ggarrange(plots = p, ncol = 1))


# __iii. Save ============================================================

ggsave2(plot = pfull, filename = "gea_by_ind_rel.pdf", height = 5, width = 7)


############################################################################## # 
##### TABLES #############################################################
############################################################################## # 

# _1. Table of CVs ==================================================

table_cv_ind <- full_by_ind[cv > 0, list(V1 = quantile(cv, probs = c(0.025, 0.5, 0.975)), 
                                         N = length(cv)),
                            by = .(gas, type)] %>% 
  .[, V1 := round(V1, 2)] %>% 
  .[, type := tolower(type)] %>% 
  .[, measure := rep(c('q025', 'median', 'q975'), 6)] %>% 
  .[] %>% 
  dcast(gas + type + N ~ measure, value.var = 'V1') %>% 
  .[] %>% 
  .[, value := paste0('$', median, '^{+', 
                      q975-median, '}_{-', median -q025, '}$')] %>% 
  dcast(gas ~ type, value.var = 'value')


table_cv_reg <- full_by_reg[cv > 0, list(quantile(cv, probs = c(0.025, 0.5, 0.975)) %>% 
                                           round(2)),
                            by = .(gas, type)] %>% 
  .[, measure := rep(c('q025', 'median', 'q975'), 6)] %>% 
  .[] %>% 
  dcast(gas + type ~ measure, value.var = 'V1') %>% 
  .[] %>% 
  .[, value := paste0('$', median, '^{+', 
                      q975-median, '}_{-', median -q025, '}$')] %>% 
  dcast(gas ~ type, value.var = 'value')



table_cv <- rbindlist(list('country/region' = table_cv_reg,
                           'economic sector' = table_cv_ind), idcol = 'level') %>% 
  .[] %>% 
  setcolorder(c('level', 'gas', 'gea', 'fp')) %>% 
  setnames(c('fp', 'gea'), c('GHG footprints', 'GHG emission accounts'))

kableExtra::kbl(table_cv, digits = 2,escape = FALSE, format = 'latex', 
                booktabs = FALSE, 
                toprule = '\\tophline', midrule = '\\middlehline', 
                bottomrule = '\\bottomhline', linesep = '', vline = '',  
                caption = "Distribution of the coefficients of variation (CV) of 
                the country- and sector-level GHG emission accounts and GHG footprints. 
                Numbers are denoted in the form of 
                $median^{+(Q_{0.975}-median)}_{-(median - Q_{0.025})}$", 
                label = 'cvs') %>% 
  write(file = file.path(path2plot, 'table_cv.tex'))




# _2. Table of inter-database comparisons =================================
table_valid_ind <- gea_by_ind[, list(total = sum(mean), 
                                     number = length(mean)), by = .(gas, type)] %>% 
  .[, 'share of sectors (EXIOBASE)':=number/sum(number), by = .(gas)] %>% 
  .[, 'share of total emissions (EXIOBASE)':=total/sum(total), by = .(gas)] %>% 
  .[, total := NULL] %>% 
  .[, number := NULL] %>% 
  setnames('type', 'EXIOBASE ... our 95\\% CI') %>% 
  .[] %>% 
  print

temp <- gea_by_reg[, .(gas, type_exio, type_oecd, mean)] 

table_oecd <- temp[, list(total = sum(mean), 
                          number = length(mean)), by = .(gas, type_oecd)] %>% 
  na.omit %>% 
  .[, 'share of countries':=number/sum(number), by = .(gas)] %>% 
  .[, 'share of total emissions':=total/sum(total), by = .(gas)] %>% 
  .[, total := NULL] %>% 
  .[, number := NULL] %>% 
  setnames('type_oecd', 'EXIOBASE ... our 95\\% CI') %>% 
  .[] %>% 
  print



table_exio <- gea_by_reg[, list(total = sum(mean), 
                                number = length(mean)), by = .(gas, type_exio)] %>%
  .[, 'share of countries':=number/sum(number), by = .(gas)] %>% 
  .[, 'share of total emissions':=total/sum(total), by = .(gas)] %>% 
  .[, total := NULL] %>% 
  .[, number := NULL] %>% 
  setnames('type_exio', 'EXIOBASE ... our 95\\% CI') %>% 
  .[] %>% 
  print


table_valid_reg <- merge(table_exio,table_oecd, 
                         by = c('gas', 'EXIOBASE ... our 95\\% CI'), 
                         all = TRUE, suffixes = c(' (EXIOBASE)', ' (OECD)')) 

table_valid <- rbindlist(list('country/region' = table_valid_reg,
                              'economic sector' = table_valid_ind), 
                         idcol = 'level', 
                         fill = TRUE)



kableExtra::kbl(table_valid, digits = 2,escape = FALSE, format = 'latex', 
                booktabs = FALSE, 
                toprule = '\\tophline', midrule = '\\middlehline', 
                bottomrule = '\\bottomhline', linesep = '', vline = '',  
                caption = "Distribution of the coefficients of variation (CV) of 
                the country- and sector-level GHG emission accounts and GHG footprints. 
                Numbers are denoted in the form of 
                $median^{+(Q_{0.975}-median)}_{-(median - Q_{0.025})}$", 
                label = 'valid') %>% 
  write(file = file.path(path2plot, 'table_valid.tex'))

# plot 
(p1 <- table_valid %>% 
    melt(id.vars = c('level', 'gas', 'EXIOBASE ... our 95\\% CI'), 
         variable.factor = FALSE) %>% 
    .[grepl('EXIO', variable), db := 'EXIOBASE'] %>%
    .[grepl('OECD', variable), db := 'OECD'] %>% 
    .[, variable := str_replace(variable, ' \\(OECD\\)', '')] %>% 
    .[, variable := str_replace(variable, ' \\(EXIOBASE\\)', '')] %>%
    .[level == 'country/region'] %>% 
    na.omit() %>% 
    .[] %>% 
    ggplot(aes(x = variable, y = value, fill = `EXIOBASE ... our 95\\% CI`, 
               label = scales::percent(round(value, 2), accuracy = 1))) + 
    geom_col() + 
    geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
    scale_fill_colorblind7() + 
    scale_y_continuous(labels = scales::percent)+
    ylab('% share') +
    xlab('')+ 
    labs(fill = 'position in relation\nto our 95% CI') + 
    facet_grid(cols = vars(gas), rows = vars(db)) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)))



(p2 <- table_valid %>% 
    melt(id.vars = c('level', 'gas', 'EXIOBASE ... our 95\\% CI'), 
         variable.factor = FALSE) %>% 
    .[grepl('EXIO', variable), db := 'EXIOBASE'] %>%
    .[grepl('OECD', variable), db := 'OECD'] %>% 
    .[, variable := str_replace(variable, ' \\(OECD\\)', '')] %>% 
    .[, variable := str_replace(variable, ' \\(EXIOBASE\\)', '')] %>%
    .[level == 'economic sector' & db == 'EXIOBASE'] %>% 
    .[] %>% 
    na.omit() %>% 
    ggplot(aes(x = variable, y = value, fill = `EXIOBASE ... our 95\\% CI`, 
               label = scales::percent(round(value, 2), accuracy = 1))) + 
    geom_col() + 
    geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
    scale_fill_colorblind7() + 
    scale_y_continuous(labels = scales::percent)+
    ylab('% share') +
    xlab('')+ 
    labs(fill = 'position in relation\nto our 95% CI') + 
    facet_grid(cols = vars(gas), rows = vars(db)) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)))

(p <- ggpubr::ggarrange(p1, p2, nrow = 1, common.legend = TRUE, legend = 'bottom', 
                        align = 'none', labels = 'AUTO'))
`# __iii. Save ============================================================

ggsave2(plot = p, filename = "gea_full_comparison.pdf", height = 5, width = 7)


# _3) Unceratinty of GEAs larger than for FPs??===============================

full_by_reg[, .(type, gas, EB_region,cv, mean, CI2.5, CI97.5)] %>% 
  .[] %>% 
  dcast(gas + EB_region ~ type, value.var = 'cv') %>% 
  .[fp < gea]

full_by_ind[, .(type, gas, EB_region, industry_code,
                cv, mean)] %>% 
  .[] %>% 
  dcast(gas + EB_region + industry_code ~ type, value.var = 'cv') %>% 
  .[FP > GEA]
.[]


############################################################################## # 
##### RETRIEVING data for the manuscript #############################################################
############################################################################## # 

# abstract ========
lapply(config$gases, function(x) gea_by_reg[gas == x, quantile(cv, 
                                                                c(0, 0.025, 0.25,
                                                                  0.5,0.75, 
                                                                  0.975, 1)) %>% 
                                               round(2)]) %>% 
  setNames(config$gases)

gea_by_reg[gas == 'CO2' & cv > 1]


lapply(config$gases, function(x) gea_by_ind[gas == x, quantile(cv, 
                                                                c(0, 0.025, 0.25,
                                                                  0.5,0.75, 
                                                                  0.975, 1), 
                                                               na.rm= TRUE) %>% 
                                               round(2)]) %>% 
  setNames(config$gases)

############################################################################## # 
##### save results #############################################################
############################################################################## # 


# THE END ---------------------------------------------------------------------
