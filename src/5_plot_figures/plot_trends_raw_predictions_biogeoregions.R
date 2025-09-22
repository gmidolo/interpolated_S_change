################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 05.07.2025
################################################################################

# Description: Plot partial dependence of S change over time per region (Figure 3)

################################################################################

#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(sf)
})

# Define habitat and biogeoregion names
hab <- c('Forest', 'Grassland', 'Scrub', 'Wetland')
bgr <- c('Alpine', 'Arctic-Boreal', 'Atlantic', 'Continental', 'Mediterranean', 'Pannonian', 'Steppic')

#### 2. Plot minimaps of biogeographic regions ####

# Load biogeographic regions
biogeoregions <- read_rds('./data/spatial/biogeoregions.rds') 

# Get biogeoregion polygons to plot
biogeoregions_pretty <- biogeoregions %>%
  st_simplify(dTolerance = 7000) # simplify to plot
biogeomaps = list()
for (i in bgr) {
  biogeomaps[[i]] <- ggplot()+
    geom_sf(data=st_union(biogeoregions_pretty$geometry), fill='white', col='grey30') +
    geom_sf(data=biogeoregions_pretty %>% filter(biogeo==i), fill='black', col=NA)+
    theme_void()+
    theme(axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5, size=13, face=2)) +
    ggtitle(i)
}

#### 3. Plot partial dependence trends ####

# Load predictions per year
dat <- read_csv('./data/preds/pdp/habitat.biogeo_pdp_trends_alltrees.csv', show_col_types = F)

# Define periods
periods <- c('From 1960 to 1980', 'From 1980 to 2000', 'From 2000 to 2020')
period.dat <- data.frame(
  periods=periods,
  min=c(1960, 1980, 2000),
  max=c(1980, 2000, 2020)
)

# Get slopes per period
period_slope = list()
for (i in periods) {  
  max_i = period.dat %>% filter(periods==i) %>% pull(max)
  min_i = period.dat %>% filter(periods==i) %>% pull(min)
  period_slope[[i]] <- dat %>%
    filter(year >= min_i & year <= max_i) %>%
    group_by(habitat, biogeo) %>%
    do(broom::tidy(lm(mean ~ year , data = .))) %>% # use median
    filter(term == 'year')
}
period_slope <- period_slope %>% bind_rows(.id='period')
period_slope$slope_decades <- period_slope$estimate*10
period_slope$slope_decades %>% hist()
period_slope$slope_decades %>% range()

# Define color range and associated palette
vals = round(seq(-3.5, 3.5, 0.01), 2)
pals = hcl.colors(length(vals), 'Spectral')

# # Plot legend palette
# pdf('./fig/S_trends/biogeo_legenda.pdf', height = 5, width = 3.5)
# legend_image <- as.raster(matrix(rev(colorspace::adjust_transparency(pals, 0.95)), ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Species gained/loss\nper decade')
# text(x=1.5, y = seq(0,1,l=5), labels = quantile(as.numeric(vals), seq(0,1,.25)))
# rasterImage(legend_image, 0, 0, 1,1)
# dev.off()

# Calculate changes to the baseline (1960)
dat_baseline <- dat %>% filter(year==1960) 
names(dat_baseline)[5:length(dat_baseline)] <- paste0(names(dat_baseline)[5:length(dat_baseline)],'_1960')
dat_cntrd <- dat %>%
  left_join(
    dat_baseline %>% select(-year), by=c('habitat','biogeo','no.plots')
  ) %>%
  group_by(habitat, biogeo, no.plots) %>%
  mutate(
    m = mean-mean_1960,
    lpi = m + (quantile0.05 - median), # lower prediction interval - quantile
    upi = m + (quantile0.95 - median), # upper prediction interval - quantile
    lci = m + (lower_ci_95 - mean), # lower confidence interval of the mean
    uci = m + (upper_ci_95 - mean) # upper confidence interval of the mean
  ) %>%
  select(habitat, biogeo, year, no.plots, mean_1960, m, lci, uci, lpi, upi)
glimpse(dat_cntrd)

# Truncate extreme values to species richness +/- 12
range(c(dat_cntrd$upi,dat_cntrd$lpi))
truncate_at <- 12
dat_cntrd$upi <- ifelse(dat_cntrd$upi >  truncate_at,  truncate_at, dat_cntrd$upi)
dat_cntrd$lpi <- ifelse(dat_cntrd$lpi < -truncate_at, -truncate_at, dat_cntrd$lpi)
range(c(dat_cntrd$lpi, dat_cntrd$upi))

# Plot map (quick version; to check)
dat_cntrd %>%
  ggplot(aes(year,m))+
  geom_ribbon(aes(ymin = lpi, ymax = upi), fill = "grey70")+
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "grey30")+
  geom_line()+
  facet_wrap(habitat~biogeo, scales='free_y', ncol=7)+
  ylim(c(-truncate_at, truncate_at))

# Plot map (pretty version)
p_means.1960 <- list()
for(b in bgr){ # for each biogeoregion
 for(h in hab){ # for each habitat
  
  # define ylims
  y_limits_centrd <- c(-truncate_at, truncate_at)
  
  # define y labels and breaks
  y_baseline <- round((dat_cntrd %>% filter(habitat==h & biogeo==b) %>% pull(mean_1960) %>% unique()),1)
  y_baseline <- ifelse(grepl("\\.", y_baseline), y_baseline, paste0(y_baseline, ".0"))
  y_labels <- c('-10', '-5', 
              y_baseline, 
              '+5', '+10')
  y_breaks <- seq(-10, 10, 5)

  # no. plots for each habitat and region
  no.plots_lab <- dat_cntrd %>%
    filter(habitat == h & biogeo == b) %>%
    pull(no.plots) %>%
    unique() %>%
    format(big.mark=",", scientific=F)

  # prepare color background (lm slope estimate)
  t1 <- period_slope %>% filter(period=='From 1960 to 1980') %>% filter(habitat==h & biogeo==b) %>% pull(slope_decades)
  t1 <- pals[which(vals %in% round(t1, 2))]
  t2 <- period_slope %>% filter(period=='From 1980 to 2000') %>% filter(habitat==h & biogeo==b) %>% pull(slope_decades)
  t2 <- pals[which(vals %in% round(t2, 2))]
  t3 <- period_slope %>% filter(period=='From 2000 to 2020') %>% filter(habitat==h & biogeo==b) %>% pull(slope_decades)
  t3 <- pals[which(vals %in% round(t3, 2))]
  
  ## Plot relative changes
  phb <- dat_cntrd %>%
    filter(habitat == h & biogeo == b) %>%
    ggplot(aes(year, m)) +
    
    # plot background colors
    annotate('rect', xmin=1960, xmax=1980, ymin=-Inf, ymax=Inf, alpha=.8, fill=t1) +
    annotate('rect', xmin=1980, xmax=2000, ymin=-Inf, ymax=Inf, alpha=.8, fill=t2) +
    annotate('rect', xmin=2000, xmax=2020, ymin=-Inf, ymax=Inf, alpha=.8, fill=t3) +
    
    # plot zero change line
    geom_segment(x = 1960, y = 0, xend = 2020, yend = 0, lty=3, color='grey30', linewidth=.2) +

    # plot preidction interval
    geom_ribbon(aes(ymin=lpi, ymax=upi), fill='grey80', alpha=.7)+

    # # plot path
    # geom_path(linewidth = 1, color='grey30') +

     # plot CI to the mean
    geom_ribbon(aes(ymin=lci, ymax=uci), fill='grey22', alpha=1)+

    # plot linear regressions per periods
    # geom_smooth(data = dat_cntrd %>% filter(year>=1960 & year<=1980 & habitat==h & biogeo==b), method = 'lm', color='#000000', se=F, linewidth=.3, alpha=1) +
    # geom_smooth(data = dat_cntrd %>% filter(year>=1980 & year<=2000 & habitat==h & biogeo==b), method = 'lm', color='#000000', se=F, linewidth=.3, alpha=1) +
    # geom_smooth(data = dat_cntrd %>% filter(year>=2000 & year<=2020 & habitat==h & biogeo==b), method = 'lm', color='#000000', se=F, linewidth=.3, alpha=1) +

    theme_bw()+
    theme(axis.title = element_blank(),
          panel.grid = element_blank())+

    scale_y_continuous(
      limits = y_limits_centrd,
      labels = y_labels,
      breaks = y_breaks
    ) +
    
    # add plot count
    annotate(
      'text',
      x = 1990, y = Inf, label = paste0('n = ', no.plots_lab), vjust   = 1.75, 
      size= 2.75,
     ) +

    theme(
      title = element_blank(),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank())

   p_means.1960[[b]][[h]] <- phb
           
 }

}

# Combine plots
cp <- plot_grid(
  
  # Inset map of biogeographic regions
  biogeomaps$Alpine,
  biogeomaps$`Arctic-Boreal`,
  biogeomaps$Atlantic,
  biogeomaps$Continental,
  biogeomaps$Mediterranean,
  biogeomaps$Pannonian,
  biogeomaps$Steppic,
  
  # Trends for forests
  p_means.1960[['Alpine']]$Forest,
  p_means.1960[['Arctic-Boreal']]$Forest,
  p_means.1960[['Atlantic']]$Forest,
  p_means.1960[['Continental']]$Forest,
  p_means.1960[['Mediterranean']]$Forest,
  p_means.1960[['Pannonian']]$Forest,
  p_means.1960[['Steppic']]$Forest,
  
  # Trends for grasslands
  p_means.1960[['Alpine']]$Grassland,
  p_means.1960[['Arctic-Boreal']]$Grassland,
  p_means.1960[['Atlantic']]$Grassland,
  p_means.1960[['Continental']]$Grassland,
  p_means.1960[['Mediterranean']]$Grassland,
  p_means.1960[['Pannonian']]$Grassland,
  p_means.1960[['Steppic']]$Grassland,
  
  # Trends for scrub
  p_means.1960[['Alpine']]$Scrub,
  p_means.1960[['Arctic-Boreal']]$Scrub,
  p_means.1960[['Atlantic']]$Scrub,
  p_means.1960[['Continental']]$Scrub,
  p_means.1960[['Mediterranean']]$Scrub,
  p_means.1960[['Pannonian']]$Scrub,
  p_means.1960[['Steppic']]$Scrub,
  
  # Trends for wetlands
  p_means.1960[['Alpine']]$Wetland,
  p_means.1960[['Arctic-Boreal']]$Wetland,
  p_means.1960[['Atlantic']]$Wetland,
  p_means.1960[['Continental']]$Wetland,
  p_means.1960[['Mediterranean']]$Wetland,
  p_means.1960[['Pannonian']]$Wetland,
  p_means.1960[['Steppic']]$Wetland,

  ncol = 7)

# export
# ggsave('./fig/S_trends/trends_biogeoregions.pdf', cp, width = 11, height = 7)
ggsave('./fig/S_trends/trends_biogeoregions.jpg', cp, width = 11, height = 7)
