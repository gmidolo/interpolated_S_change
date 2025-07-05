################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 05.07.2025
################################################################################

# Description: Plot partial dependence of S change over time (Figure 2b)

################################################################################


#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})

# Load predictions per year
dat <- read_csv('./data/preds/pdp/habitat_pdp_trends_alltrees.csv', show_col_types = F)

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
    group_by(habitat) %>%
    do(broom::tidy(lm(mean ~ year , data = .))) %>% # use median
    filter(term == 'year')
  
}
period_slope <- period_slope %>% bind_rows(.id='period')
period_slope$slope_decades <- period_slope$estimate*10
period_slope$slope_decades %>% hist()
period_slope$slope_decades %>% range()

# Define color range and associated palette
vals = round(seq(-1, 1, 0.01), 2)
pals = hcl.colors(length(vals), 'Spectral')

# # Plot legend
# pdf('./fig/S_trends/legenda.pdf', height = 5, width = 3.5)
# legend_image <- as.raster(matrix(rev(colorspace::adjust_transparency(pals, 0.95)), ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Species gained/loss\nper decade')
# text(x=1.5, y = seq(0,1,l=5), labels = quantile(as.numeric(vals), seq(0,1,.25)))
# rasterImage(legend_image, 0, 0, 1,1)
# dev.off()

# Calculate changes to the baseline (1960)
dat_baseline <- dat %>% filter(year==1960) 
names(dat_baseline)[3:length(dat_baseline)] <- paste0(names(dat_baseline)[3:length(dat_baseline)],'_1960')
dat_cntrd <- dat %>%
  left_join(
    dat_baseline %>% select(-year), by='habitat'
  ) %>%
  group_by(habitat) %>%
  mutate(
    m = mean-mean_1960,
    lpi = m + (quantile0.05 - median), # lower prediction interval - quantile
    upi = m + (quantile0.95 - median), # upper prediction interval - quantile
    lci = m + (lower_ci_95 - mean), # lower confidence interval of the mean
    uci = m + (upper_ci_95 - mean) # upper confidence interval of the mean
  ) %>%
  select(habitat, year, mean_1960, m, lci, uci, lpi, upi)

# Plot map (quick version; to check)
dat_cntrd %>%
  ggplot(aes(year,m))+
  geom_ribbon(aes(ymin = lpi, ymax = upi), fill = 'grey70')+
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'grey30')+
  geom_line()+
  facet_wrap(~habitat, scales='free_y')+
  ylim(c(-5.2, 5.5))

# Plot map (pretty version)
p_means.1960 <- list()
hab = c('Forest', 'Grassland', 'Scrub', 'Wetland')
for (h in hab) {
  
  # define ylims
  y_limits_centrd <- c(-5.2, 5.5)
  # define y labels and breaks
  y_labels <- c('-4', '-2', 
              round((dat_cntrd %>% filter(habitat==h) %>% pull(mean_1960) %>% unique()),1), 
              '+2', '+4')
  y_breaks <- seq(-4, 4, 2)

  # prepare color background (lm slope estimate)
  t1 <- period_slope %>% filter(period=='From 1960 to 1980') %>% filter(habitat==h) %>% pull(slope_decades)
  t1 <- pals[which(vals %in% round(t1, 2))]
  t2 <- period_slope %>% filter(period=='From 1980 to 2000') %>% filter(habitat==h) %>% pull(slope_decades)
  t2 <- pals[which(vals %in% round(t2, 2))]
  t3 <- period_slope %>% filter(period=='From 2000 to 2020') %>% filter(habitat==h) %>% pull(slope_decades)
  t3 <- pals[which(vals %in% round(t3, 2))]
  
  # plot
  p_means.1960[[h]] <- dat_cntrd %>%
    filter(habitat == h) %>%
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
    geom_ribbon(aes(ymin=lci, ymax=uci), fill='grey40', alpha=.7)+

    # # plot linear regressions per periods
    # geom_smooth(data = dat_cntrd %>% filter(year>=1960 & year<=1980 & habitat==h), method = 'lm', color='#000000', se=F, linewidth=.3, alpha=1) +
    # geom_smooth(data = dat_cntrd %>% filter(year>=1980 & year<=2000 & habitat==h), method = 'lm', color='#000000', se=F, linewidth=.3, alpha=1) +
    # geom_smooth(data = dat_cntrd %>% filter(year>=2000 & year<=2020 & habitat==h), method = 'lm', color='#000000', se=F, linewidth=.3, alpha=1) +

    ggtitle(h)+
    theme_bw()+
    theme(axis.title = element_blank(),
          panel.grid = element_blank(),
          title = element_text(size = 10))+
    
    # set up y axis 
    scale_y_continuous(
      limits = y_limits_centrd,
      labels = y_labels,
      breaks = y_breaks
    )
                       
}

# Combine plots
cp <- plot_grid(p_means.1960$Forest,
                p_means.1960$Grassland,
                p_means.1960$Scrub,
                p_means.1960$Wetland)

ggsave('./fig/S_trends/trends_habitats.jpg', cp, width = 3.85, height = 3.465, dpi = 500)
# ggsave('./fig/trends_habitats/minitrends.changeto1960.pdf', cp, width = 3.85, height = 3.465)