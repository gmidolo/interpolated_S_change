#--- MAP OF DIVERSITY CHANGE USING RANDOM FOREST ---#

# Plot objects used for SI (Metrics of species richness change maps)

#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(sf)
    library(terra)
  }
)

# Load predictions at the plot level
dat <- './data/preds/preds_stdpltsz.rf.csv' %>%
 read_csv(show_col_types = F) 

#plot(dat_ORIG$S_abs.change_1960.2020,dat$S_abs.change_1960.2020)
#plot(dat_ORIG$lm.slope_estimate,dat$lm.slope_estimate)

# Define theme for plotting
my_theme <- theme(
 strip.text = element_text(colour ='black', face=2, size=10),
 axis.title=element_blank(),
 legend.title = element_text(face=2, size=10),
 strip.background = element_blank(), 
 strip.placement = 'outside'
)

# Define function to calculate the mean in cases where >= 5 plots are available
mean_at_least_five_plots <- \(x, th = 5) {ifelse(length(x) >= th, mean(x), NA)}

# Define EU regions to show
regions.name <- c('Albania', 'Austria', 'Belarus','Baleares', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria',
                 'Corsica', 'Crete', 'Croatia', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany',
                 'Greece', 'Hungary', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein',
                 'Lithuania', 'Luxembourg', 'Malta', 'Moldova', 'Montenegro', 'Netherlands', 'North Macedonia',
                 'Norway', 'Poland', 'Portugal', 'Romania', 'Sardinia', 'Serbia', 'Sicily',
                 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Ukraine', 'United Kingdom')

# load EU shapefile         
EU <- './data/spatial/euro+med_map/euro+med.map.shp' %>%
  read_sf %>%
  semi_join(data.frame(name = regions.name), by = 'name') %>% #include only the needed regions
  st_transform(crs = 25832) %>% # reproject
  st_simplify(dTolerance = 1000)

# Further simplify EU shapefile
EU <- EU %>% 
  st_buffer(1000) %>% 
  st_simplify(dTolerance = 4000)

#### 2. Maps of temporal changes (contrast-based metircs) in different periods per habitat ####

# Define base raster
res_km <- 50 # resolution of the raster
r <- rast(res = res_km*1000, extent=ext(EU), crs=crs(EU)) 

# Variables to work with, assign time range
var2collect <- cbind(
               var = names(dat)[str_detect(names(dat), '.change')], 
               t(as.data.frame(regmatches(names(dat)[str_detect(names(dat), '.change')], gregexpr("[[:digit:]]+", names(dat)[str_detect(names(dat), '.change')]))))
) %>%
 as_tibble() %>%
 setNames(c('var','min_yr', 'max_yr')) %>%
 mutate(min_yr=as.numeric(min_yr), max_yr=as.numeric(max_yr))

# Get rasterized (averaged) metrics of change
d2rast <- list()
d2plot <- list()
for(i in var2collect$var) {
  for(h in c('Forest','Grassland','Scrub','Wetland')) {
   d_i_h <- dat %>% 
             filter(habitat==h) %>% 
             filter(year >= var2collect[which(var2collect$var == i),]$min_yr) %>%
             filter(year <= var2collect[which(var2collect$var == i),]$max_yr)
              
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


### 2.1 Maps of Percentage of change (%) #####
metric2plot <- 'S_perc.change_'

# Filter data for plotting
d2plot_refined <- d2plot %>% filter(str_detect(metric,metric2plot))

# Define pretty facet labels
nice_labels <- data.frame(metric = unique(d2plot_refined$metric))
nice_labels$period <- paste0('From ', nice_labels$metric %>% str_remove(metric2plot)) %>% str_replace('\\.', ' to ')
nice_labels$period <- factor(nice_labels$period, nice_labels$period)

# Add facet labels and cut the mean values of the metric to plot
d2plot_refined <- d2plot_refined %>% left_join(nice_labels, 'metric') 
d2plot_refined <- d2plot_refined %>%
                  mutate(mean_cat = cut(
                  mean, 
                  breaks = c(-Inf,-50, -25, -10, -5, 5, 10, 25, 50, Inf), 
                  labels = c('< -50%','-50% – -25%','-25% – -10%','-10% – -5%', '-5% – 5%' ,'5% – 10%', '10% – 25%','25% – 50%', '> 50%') ))

# Define color palette
cols <-  hcl.colors(length(levels(d2plot_refined[1,'mean_cat'])), 'Spectral')

# Plot
map <- ggplot() +
 geom_sf(data=EU, fill='white', color=NA) +
 geom_raster(data=d2plot_refined, aes(x,y,fill=mean_cat)) +
 facet_grid(habitat~period) +
 scale_fill_manual(values = cols) +
 geom_sf(data=EU, fill=NA, color=alpha('black',0.5), linewidth=.1) +
 labs(fill='Percentage change') +
 geom_rect(data = subset(d2plot_refined, period %in% c('From 1960 to 2020')), 
                          linewidth = 1.25, fill = NA, colour = 'grey20', xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf) +
 my_theme +
 guides(fill = guide_legend(reverse=TRUE))
map

# Export
ggsave(paste0('./fig/geomaps_stdpltsz_all/RF.',metric2plot,'_stdpltsz_map.jpg'), map, width = 9, height = 8, dpi=600)
ggsave(paste0('./fig/geomaps_stdpltsz_all/RF.',metric2plot,'_stdpltsz_map.pdf'), map, width = 9, height = 8)


### 2.2 Maps of log-response ratios (lnRR) #####
metric2plot <- 'S_lnRR.change_'

# Filter data for plotting
d2plot_refined <- d2plot %>% filter(str_detect(metric,metric2plot))

# Define pretty facet labels
nice_labels <- data.frame(metric = unique(d2plot_refined$metric))
nice_labels$period <- paste0('From ', nice_labels$metric %>% str_remove(metric2plot)) %>% str_replace('\\.', ' to ')
nice_labels$period <- factor(nice_labels$period, nice_labels$period)

# Add facet labels and cut the mean values of the metric to plot
d2plot_refined <- d2plot_refined %>% left_join(nice_labels, 'metric') 
d2plot_refined <- d2plot_refined %>%
                  mutate(mean_cat = cut(
                  mean, 
                  breaks = c(-10,-0.5, -0.1, -0.05, 0.05, 0.1, 0.5, 10),
                  labels = c('< -0.5','-0.5 – -0.1','-0.1 – -0.05', '-0.05 – 0.05','0.05 – 0.1', '0.1 – 0.5' ,'> 0.5')))

# Define color palette
cols <-  hcl.colors(length(levels(d2plot_refined[1,'mean_cat'])), 'Spectral')

# Plot
map <- ggplot() +
 geom_sf(data=EU, fill='white', color=NA) +
 geom_raster(data=d2plot_refined, aes(x,y,fill=mean_cat)) +
 facet_grid(habitat~period) +
 scale_fill_manual(values = cols) +
 geom_sf(data=EU, fill=NA, color=alpha('black',0.5), linewidth=.1) +
 labs(fill='lnRR') +
 geom_rect(data = subset(d2plot_refined, period %in% c('From 1960 to 2020')), 
                          linewidth = 1.25, fill = NA, colour = 'grey20', xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf) +
 my_theme +
 guides(fill = guide_legend(reverse=TRUE))
map

# Export
ggsave(paste0('./fig/geomaps_stdpltsz_all/RF.',metric2plot,'_stdpltsz_map.jpg'), map, width = 9, height = 8, dpi=600)
ggsave(paste0('./fig/geomaps_stdpltsz_all/RF.',metric2plot,'_stdpltsz_map.pdf'), map, width = 9, height = 8)


### 2.3 Maps of abs_change #####
metric2plot <- 'S_abs.change_'

# Filter data for plotting
d2plot_refined <- d2plot %>% filter(str_detect(metric,metric2plot))

# Define pretty facet labels
nice_labels <- data.frame(metric = unique(d2plot_refined$metric))
nice_labels$period <- paste0('From ', nice_labels$metric %>% str_remove(metric2plot)) %>% str_replace('\\.', ' to ')
nice_labels$period <- factor(nice_labels$period, nice_labels$period)

# Add facet labels and cut the mean values of the metric to plot
d2plot_refined <- d2plot_refined %>% left_join(nice_labels, 'metric') 
d2plot_refined <- d2plot_refined %>%
                  mutate(mean_cat = cut( 
                  mean,
                  breaks = c(-Inf, -10, -5, -1, 1, 5, 10,Inf),
                  labels = c('< -10','-10 – -5', '-5 – -1','-1 – 1', '1 – 5' ,'5 – 10', '> 10')
                  ))

# Define color palette
cols <-  hcl.colors(length(levels(d2plot_refined[1,'mean_cat'])), 'Spectral')

# Plot
map <- ggplot() +
geom_sf(data=EU, fill='white', color=NA) +
 geom_raster(data=d2plot_refined, aes(x,y,fill=mean_cat)) +
 facet_grid(habitat~period) +
 scale_fill_manual(values = cols) +
 geom_sf(data=EU, fill=NA, color=alpha('black',0.5), linewidth=.1) +
 labs(fill='N. species') +
 geom_rect(data = subset(d2plot_refined, period %in% c('From 1960 to 2020')), 
                          linewidth = 1.25, fill = NA, colour = 'grey20', xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf) +
 my_theme +
 guides(fill = guide_legend(reverse=TRUE))
map

# Export
ggsave(paste0('./fig/geomaps_stdpltsz_all/RF.',metric2plot,'_stdpltsz_map.jpg'), map, width = 9, height = 8, dpi=600)
ggsave(paste0('./fig/geomaps_stdpltsz_all/RF.',metric2plot,'_stdpltsz_map.pdf'), map, width = 9, height = 8)



#### 3. Maps of linear trends per habitat ####

metric2plot <- 'lm.slope_estimate'

# Define base raster
res_km <- 50 # resolution of the raster
r <- rast(res = res_km*1000, extent=ext(EU), crs=crs(EU)) 

# Variables to work with, assign time range
var2collect <-
  cbind(
    var = c('lm.slope_estimate1960.1980','lm.slope_estimate1980.2000','lm.slope_estimate2000.2020','lm.slope_estimate'),
    var2collect[1:4,2:3]
  )

d2rast <- list()
d2plot <- list()
for(i in var2collect$var) {
  for(h in c('Forest','Grassland','Scrub','Wetland')) {
   d_i_h <- dat %>% 
             filter(habitat==h) %>% 
             filter(year >= var2collect[which(var2collect$var == i),]$min_yr) %>%
             filter(year <= var2collect[which(var2collect$var == i),]$max_yr)
   
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

# Filter data for plotting
d2plot_refined <- d2plot %>% filter(str_detect(metric,metric2plot))
d2plot_refined$metric <- ifelse(d2plot_refined$metric == 'lm.slope_estimate', 'lm.slope_estimate1960.2020', d2plot_refined$metric)

# Define pretty facet labels
nice_labels <- data.frame(metric = unique(d2plot_refined$metric))
nice_labels$period <- paste0('From ', nice_labels$metric %>% str_remove(metric2plot)) %>% str_replace('\\.', ' to ')
nice_labels$period <- factor(nice_labels$period, nice_labels$period)

# Add facet labels and cut the mean values of the metric to plot
d2plot_refined <- d2plot_refined %>% left_join(nice_labels, 'metric') 
d2plot_refined <- d2plot_refined %>%
                  mutate(mean_cat = cut(
                  mean, 
                  breaks = c(-Inf,-0.5, -0.2, -0.1, -0.05, -0.025, 0.025, 0.05, 0.1, 0.2, 0.5, Inf), 
                  labels = c('< -5','-5 – -2','-2 – -1','-1 – -0.5','-0.5 – -0.25','-0.25 – 0.25','0.25 – 0.5', '0.5 – 1','1 – 2', '2 – 5', '> 5') ))

# Define color palette
cols <-  hcl.colors(length(levels(d2plot_refined[1,'mean_cat'])), 'Spectral')

# Plot
map <- ggplot() +
 geom_sf(data=EU, fill='white', color=NA) +
 geom_raster(data=d2plot_refined, aes(x,y,fill=mean_cat)) +
 facet_grid(habitat~period) +
 scale_fill_manual(values = cols) +
 geom_sf(data=EU, fill=NA, color=alpha('black',0.5), linewidth=.1) +
 labs(fill='Linear slope\nestimate (species\nper decade)') +
 geom_rect(data = subset(d2plot_refined, period %in% c('From 1960 to 2020')), 
                          linewidth = 1.25, fill = NA, colour = 'grey20', xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf) +
 my_theme +
 guides(fill = guide_legend(reverse=TRUE))
map

# Export
ggsave(paste0('./fig/geomaps_stdpltsz_all/RF.',metric2plot,'_stdpltsz_map.jpg'), map, width = 9, height = 8, dpi=600)
ggsave(paste0('./fig/geomaps_stdpltsz_all/RF.',metric2plot,'_stdpltsz_map.pdf'), map, width = 9, height = 8)

quit(save='no')