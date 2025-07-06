################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 06.07.2025
################################################################################

# Description: Summary percentage change (1960 to 2020) (Figure 2a)

################################################################################


#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
})

# Load plot-level predictions
dat <- read_csv('./data/preds/preds_stdpltsz.rf.csv', 
                show_col_types = F,
                # load only needed columns
                col_select = c('plot_id', 'x', 'y', 'year', 'habitat', 'S_perc.change_1960.2020'))
head(dat)

# Filter only dat from 1960 to 2020
dat <- dat %>%
  filter(year >= 1960 & year <= 2020) 

# Summary stats about overall changes
prop.table(table(dat$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more
prop.table(table(dat$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more

# Summary stats for forests
dat_hi=dat %>% filter(habitat == 'Forest')
prop.table(table(dat_hi$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more in forests
prop.table(table(dat_hi$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more in forests
mean(dat_hi$S_perc.change_1960.2020) # fraction of plots losing -20% or more

# Summary stats for grasslands
dat_hi=dat %>% filter(habitat == 'Grassland')
prop.table(table(dat_hi$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more in grasslands
prop.table(table(dat_hi$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more in grasslands
mean(dat_hi$S_perc.change_1960.2020) # fraction of plots losing -20% or more

# Summary stats for scrub
dat_hi=dat %>% filter(habitat == 'Scrub')
prop.table(table(dat_hi$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more in scrubs
prop.table(table(dat_hi$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more in scrubs
mean(dat_hi$S_perc.change_1960.2020) # fraction of plots losing -20% or more

# Summary stats for wetlands
dat_hi=dat %>% filter(habitat == 'Wetland')
prop.table(table(dat_hi$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more in wetlands
prop.table(table(dat_hi$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more in wetlands
mean(dat_hi$S_perc.change_1960.2020) # fraction of plots losing -20% or more

#### Plot histogram (Figure 2a) ####

# Summary stats
dat$S_perc.change_1960.2020 %>% range
dat$S_perc.change_1960.2020 %>% mean
dat$S_perc.change_1960.2020 %>% median

# Plot and export
hist_total <- dat %>% 
  ggplot(aes(x=S_perc.change_1960.2020))+
  geom_histogram(bins = 22, fill='grey', color='grey20') +
  scale_y_continuous(labels = scales::unit_format(unit = 'k', scale = 1e-3))+
  theme_bw()+
  geom_vline(aes(xintercept=mean(S_perc.change_1960.2020)), color="red", linetype=1, linewidth=1, alpha=.7) +
  geom_vline(aes(xintercept=0), color="black", linetype=2, linewidth=.5) +
  scale_x_continuous(breaks = c(-150,-100,-50,-10,10,50,100,150), limits = c(-100,190)) +
  theme(panel.grid.minor = element_blank())+
  labs(x=expression('Species richness change (%)'*'100*'*'(('*S[2020]-S[1960]*')'/S[1960]*')'), y='No. plots')+
  theme(axis.text = element_text(size=11),
        axis.text.x = element_text(angle = 45, hjust=1))

ggsave('./fig/S_trends/hist_perc_change_1960-2020.jpg', hist_total, width = 4, height = 4)
# ggsave('./fig/S_trends/hist_pchange.pdf', hist_total, width = 4, height = 4)

#### Plot latitudinal gradients patterns (S change vs. latitude) (Supporing information) ####
# 
# # Transform data to WGS84
# dat_lat_corr <- dat %>%
#   st_as_sf(coords=c('x','y'), crs=25832, remove=F) %>%
#   st_transform(crs='WGS84') %>%
#   as_Spatial() %>%
#   as.data.frame() %>%
#   rename(lon=coords.x1, lat=coords.x2)
# 
# # Plot and export
# corr_with_lat_plot <- ggplot(dat_lat_corr, aes(y=S_perc.change_1960.2020, x = lat)) +
#   geom_hex(bins=45) +
#   facet_wrap(~habitat, ncol=2, scales='free') +
#   geom_hline(yintercept = 0, lty=2, col='grey80')+
#   geom_smooth(color='brown2')+
#   labs(x='Latitude', y='Species richness change (1960-2020) (%)', fill='No. plots')+
#   theme_bw()
# ggsave('./fig/S_trends/corr_with_lat_plot.jpg', corr_with_lat_plot, width = 7, height = 4.5, dpi=600)