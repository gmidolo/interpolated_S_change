################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 06.07.2025
################################################################################

# Description: Map S change across Europe (Figure 4)

################################################################################

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(terra)
})

# Load predictions at the plot level
dat <- './data/preds/preds_stdpltsz.rf.csv' %>%
 read_csv(show_col_types = F) %>%
 filter(year >= 1960 & year <= 2020)

# Load EU shapefile         
EU <- './data/spatial/EU_shape_map.rds' %>%
  read_rds() %>%
  # Further simplify EU shapefile
  st_buffer(1000) %>% 
  st_simplify(dTolerance = 4000)

# Define base raster where to calculate average S change
res_km <- 50 # resolution of the raster in km
r <- rast(res = res_km*1000, extent=ext(EU), crs=crs(EU)) 

# Variables to work with, assign time range
var2collect <- cbind(
  var = names(dat)[str_detect(names(dat), '.change')], 
  t(as.data.frame(regmatches(
    names(dat)[str_detect(names(dat), '.change')], 
    gregexpr("[[:digit:]]+", names(dat)[str_detect(names(dat), '.change')])
  )))
) %>%
  as_tibble(.name_repair = 'minimal') %>%  # or use "unique"
  setNames(c('var','min_yr', 'max_yr')) %>%
  mutate(min_yr = as.numeric(min_yr), max_yr = as.numeric(max_yr)) %>%
  filter(str_detect(var, 'S_perc.change'))
print(head(var2collect))

# Define function to calculate the mean in cases where >= 5 plots are available
mean_at_least_five_plots <- \(x, th = 5) {ifelse(length(x) >= th, mean(x), NA)}

# Get rasterized (averaged) metrics of change
d2rast <- list() # list storing rasters
d2plot <- list() # list storing data to plot
for(i in var2collect$var) {
  for(h in c('Forest','Grassland','Scrub','Wetland')) {
   d_i_h <- dat %>% filter(habitat==h)
   d2rast[[h]][[i]] <- rasterize(d_i_h %>% select(x,y) %>% as.matrix(), r, values = d_i_h[i], fun = mean_at_least_five_plots)
   d2plot[[h]][[i]] <- d2rast[[h]][[i]] %>%
                        as.data.frame(xy=T, na.rm=F) %>%
                        rownames_to_column('id') %>%
                        rename(mean = values) %>%
                        drop_na()
 }
}
d2plot <- d2plot %>% 
           map(\(x){bind_rows(x, .id = 'metric')}) %>%
           bind_rows(.id='habitat') 

# Define metric to plot (= species richness percentage change)
metric2plot <- 'S_perc.change_'

# Filter data for plotting
d2plot_refined <- d2plot %>% filter(str_detect(metric,metric2plot))

# Define pretty facet labels
nice_labels <- data.frame(metric = unique(d2plot_refined$metric))
nice_labels$period <- paste0('From ', nice_labels$metric %>% str_remove(metric2plot)) %>% str_replace('\\.', ' to ')
nice_labels$period <- factor(nice_labels$period, nice_labels$period)

# Add facet labels and cut the mean values of the metric to plot
br = c(-Inf,-50, -25, -10, -5, 5, 10, 25, 50, Inf)
lb = c('< -50%','-25% - -50%','-10% - -25%','-5% - -10% ', '-5% - 5%' ,'5% - 10%', '10% - 25%','25% - 50%', '> 50%')

d2plot_refined <- d2plot_refined %>% left_join(nice_labels, 'metric') 
d2plot_refined <- d2plot_refined %>%
                  mutate(mean_cat = cut(
                  mean, 
                  breaks = br, 
                  labels = lb))

# Define color palette
cols <-  hcl.colors(length(levels(d2plot_refined[1,'mean_cat'])), 'Spectral')

# Define theme for plotting
my_theme <- theme(
  strip.text = element_text(colour ='black', face=2, size=10),
  axis.title=element_blank(),
  legend.title = element_text(face=2, size=10),
  strip.background = element_blank(), 
  strip.placement = 'outside'
)

# Plot
map <- ggplot() +
 geom_sf(data=EU, fill='white', color=NA) +
 geom_raster(data=d2plot_refined, aes(x,y,fill=mean_cat)) +
 facet_grid(habitat~period) +
 scale_fill_manual(values = cols) +
 geom_sf(data=EU, fill=NA, color=alpha('black',0.5), linewidth=.1) +
 labs(fill='Percentage change') +
 geom_rect(data = subset(d2plot_refined, period %in% c('From 1960 to 2020')), 
                          linewidth = 1.25, fill = NA, colour = 'grey20', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
 my_theme +
 theme(legend.position = 'bottom') +
 guides(fill = guide_legend(reverse=TRUE, direction = 'horizontal', nrow = 1))
map

# Export
ggsave('./fig/S_trends/geo_map.jpg', map, width = 9, height = 8)
# ggsave('./fig/S_trends/geo_map.pdf', map, width = 9, height = 8)


# Prepare histograms (distribution of plots per species richness change class) to add to the above map
hists <- list()
coltab <- bind_rows(mean_cat = lb, cols = cols)

for (h in c('Forest', 'Grassland', 'Scrub', 'Wetland')) {
  for (i in var2collect$var) {
    d_h_i <- dat %>%
      filter(habitat == h)
    
    names(d_h_i)[names(d_h_i) %in% i] <- 'var2plot'
    
    d_h_i <- d_h_i  %>%
      #filter(year >= var2collect[which(var2collect$var == i),]$min_yr) %>%
      #filter(year <= var2collect[which(var2collect$var == i),]$max_yr) %>%
      mutate(mean_cat = cut(var2plot, breaks = br, labels = lb))
    
    colpa <- data.frame(cols = cols, mean_cat = lb) %>% semi_join(d_h_i, 'mean_cat')
    
    dbase <- d_h_i %>% group_by(mean_cat) %>% summarise(n = n())
    
    # histograms
    hists[[h]][[i]] <-
      dbase %>%
      ggplot(aes(x = mean_cat, y = n, fill = mean_cat)) +
      geom_col(col = 'grey20',
               position = 'dodge',
               width = 1) +
      scale_fill_manual(values = colpa$cols) +
      theme_classic() +
      ggtitle(paste0('n = ', format(
        nrow(d_h_i), big.mark = ",", scientific = FALSE
      ))) +
      theme_void() +
      theme(
        title = element_text(size = 16, face = 2),
        axis.title = element_blank(),
        axis.text.y = element_text(colour = 'black', size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
      ) +
      theme(axis.text.y = element_blank())
    
  }
}

# Export histograms
for (h in names(hists)) {

  ggsave(
    paste0('./fig/S_trends/hist_', h, '.svg'),
    cowplot::plot_grid(hists[[h]][[1]], hists[[h]][[2]], hists[[h]][[3]], hists[[h]][[4]], labels = c('1960.1980', '1980.2000', '1980.2000', '2000.2020')),
    width = 5 * 0.9,
    height = 4 * 0.9
  )
  
}

