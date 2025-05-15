#--- MAP OF DIVERSITY CHANGE USING RANDOM FOREST ---#

# Plot objects used for Figure 3 (Percentage change maps)

#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(sf)
    library(terra)
  }
)

# Load predictions
dat <- read_csv('./data/preds/preds_stdpltsz.rf.csv', show_col_types = F)
str(dat)

# Load EU map
regions.name <- c('Albania', 'Austria', 'Belarus','Baleares', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria',
                 'Corsica', 'Crete', 'Croatia', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany',
                 'Greece', 'Hungary', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein',
                 'Lithuania', 'Luxembourg', 'Malta', 'Moldova', 'Montenegro', 'Netherlands', 'North Macedonia',
                 'Norway', 'Poland', 'Portugal', 'Romania', 'Sardinia', 'Serbia', 'Sicily',
                 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Ukraine', 'United Kingdom')
                 
EU <- './data/spatial/euro+med_map/euro+med.map.shp' %>%
  read_sf %>%
  semi_join(data.frame(name = regions.name), by = 'name') %>% #include only the needed regions
  st_transform(crs = 25832) %>% #reproject
  st_simplify(dTolerance = 1000) 

my_theme <- theme(
  #panel.background = element_rect(fill = 'white',color='grey70'),
  #panel.grid = element_line(color = 'grey90'),
  #strip.background = element_rect(fill='white', color='black'),
  strip.text = element_text(colour ='black'),
  axis.title=element_blank()
)

#### 1. Inspect general trends #####
br = c(-Inf,-50, -25, -10, -5, 5, 10, 25, 50, Inf)
#lb = c('< -50%','-50% - -25%','-25% - -10%','-10% - -5%', '-5% - 5%' ,'5% - 10%', '10% - 25%','25% - 50%', '> 50%')
lb = c('< -50%','-25% - -50%','-10% - -25%','-5% - -10% ', '-5% - 5%' ,'5% - 10%', '10% - 25%','25% - 50%', '> 50%')

# how many plots have >20% losses or 20% gains?
dat %>%
  filter(year >= 1960) %>%
  filter(year <= 2020) %>%
  mutate(cat = cut(
    S_perc.change_1960.2020, 
    breaks = c(-Inf, -20, 20, Inf), 
    labels = c('loss','stationary','gain'))) %>%
  pull(cat) %>%
  table() %>%
  prop.table()

#### 2. Rasterize average change per habitat ####

# Form base raster
res_km <- 50
r <- rast(res = res_km*1000, extent=ext(EU), crs=crs(EU)) 

# Variables to work with, assign time range
var2collect <- data.frame(
               var = 'S_perc.change_1960.2020', 
               min_yr = 1960,
               max_yr = 2020
)
dat$S_perc.change_1960.2020 %>% hist()
# Rasterize metrics of change
mean_at_least_five_plots <- \(x, th = 5) {
  ifelse(length(x) >= th, mean(x), NA)
} # average if there are at least 5 plots
conta_at_least_five_plots <- \(x, th = 5) {
  ifelse(length(x) >= th, length(x), NA)
} # average if there are at least 5 plots

dat_i_h <- list()
conta_tot = list()
conta_rast = list()
d2rast <- list()
d2plot <- list()
for(i in var2collect$var) {
  for(h in c('Forest','Grassland','Scrub','Wetland')) {
   d_i_h <- dat %>% 
             filter(habitat==h) %>% 
             filter(year >= var2collect[which(var2collect$var == i),]$min_yr) %>%
             filter(year <= var2collect[which(var2collect$var == i),]$max_yr)
   dat_i_h[[h]] <- d_i_h
   
   conta_tot[[h]] <- data.frame(habitat=h, n=nrow(d_i_h))          
   d2rast[[h]][[i]] <- rasterize(d_i_h %>% select(x,y) %>% as.matrix(), r, values = d_i_h[i], fun = mean_at_least_five_plots)
   conta_rast[[h]] <- rasterize(d_i_h %>% select(x,y) %>% as.matrix(), r, values = 1:nrow(d_i_h), fun = conta_at_least_five_plots)
   
   d2plot[[h]][[i]] <- d2rast[[h]][[i]] %>%
                        as.data.frame(xy=T, na.rm=F) %>%
                        rownames_to_column('id') %>%
                        rename(mean = values) %>%
                        drop_na()
 }
}
conta_tot %>% bind_rows
conta_rast$Forest[][,1] %>% sum(na.rm = T)
conta_rast$Grassland[][,1] %>% sum(na.rm = T)
conta_rast$Scrub[][,1] %>% sum(na.rm = T)
conta_rast$Wetland[][,1] %>% sum(na.rm = T)
sum(
  conta_rast$Forest[][,1] %>% sum(na.rm = T),
  conta_rast$Grassland[][,1] %>% sum(na.rm = T),
  conta_rast$Scrub[][,1] %>% sum(na.rm = T),
  conta_rast$Wetland[][,1] %>% sum(na.rm = T)
)
# Plot n plots
plot(rast(conta_rast))


d2plot <- d2plot %>% 
           map(\(x){bind_rows(x, .id = 'metric')}) %>%
           bind_rows(.id='habitat') 
#conta = bind_rows(conta)


#### 3. Plotting ####

br = c(-Inf,-50, -25, -10, -5, 5, 10, 25, 50, Inf)
#lb = c('< -50%','-50% - -25%','-25% - -10%','-10% - -5%', '-5% - 5%' ,'5% - 10%', '10% - 25%','25% - 50%', '> 50%')
lb = c('< -50%','-25% - -50%','-10% - -25%','-5% - -10% ', '-5% - 5%' ,'5% - 10%', '10% - 25%','25% - 50%', '> 50%')

metric2plot <- 'S_perc.change_1960.2020'
d2plot_refined <- d2plot %>% filter(str_detect(metric,metric2plot))
d2plot_refined <- d2plot_refined %>%
                  mutate(mean_cat = cut(
                  mean, 
                  breaks = br, 
                  labels = lb))
table(d2plot_refined$mean_cat)
cols <-  hcl.colors(length(levels(d2plot_refined[1,'mean_cat'])), 'Spectral')

# Prepare histograms (distribution of plots per species richness change class)
hists=list()
for (h in c('Forest','Grassland','Scrub','Wetland')) {
  
  d_h <- dat %>% 
    filter(habitat==h) %>%
    filter(year >= var2collect[which(var2collect$var == i),]$min_yr) %>%
    filter(year <= var2collect[which(var2collect$var == i),]$max_yr) %>% 
    mutate(mean_cat = cut(
      S_perc.change_1960.2020, 
      breaks = br, 
      labels = lb)) 
  
  colpa <- data.frame(cols=cols, mean_cat=lb) %>% semi_join(d_h, 'mean_cat')
  
  dbase = d_h %>% group_by(mean_cat) %>% summarise(n=n()) 
  
  message(h)
  print(paste0('Mean: ', round(mean(d_h$S_perc.change_1960.2020),3)))
  print(prop.table(table(d_h$S_perc.change_1960.2020 >= 20))[2])
  print(prop.table(table(d_h$S_perc.change_1960.2020 <= -20))[2])
  message(paste('-25% - -10% change:',round(prop.table(table(d_h$mean_cat))[3],2)))
  message(paste('10% - 25% change:',round(prop.table(table(d_h$mean_cat))[7],2)))
  
  hists[[h]] <-
    dbase %>%
    ggplot(aes(x = mean_cat, y = n, fill = mean_cat)) +
    geom_col(col='grey20', position = 'dodge', width=1) +
    scale_fill_manual(values=colpa$cols) +
    theme_classic() +
    ggtitle(
      paste0('n = ',format(nrow(d_h), big.mark=",", scientific=FALSE))
    )+
    theme(
      title = element_text(size=16, face = 2),
      #plot.background = element_rect(color = 'black', fill='white', linewidth = 3)
      axis.title = element_blank(),
      axis.text.y = element_text(colour='black', size=14),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.position = 'none',
      plot.title = element_text(hjust = 0.5),
    )+
    scale_y_continuous(labels = scales::unit_format(unit = "k", scale = 1e-3))
   

  
}
hists # distributions for each histogram

for (h in names(hists)) {
  ggsave(paste0('./fig/geomaps_stdpltsz/hist_',h,'.svg'), hists[[h]], width = 3*0.9, height = 2.5*0.9)
}

newEU <- EU %>% st_buffer(1000) %>% st_simplify(dTolerance = 4000)

# Map per habitat
map <- ggplot() +
 geom_sf(data=newEU, fill='white', color=NA) +
 geom_raster(data=d2plot_refined, aes(x,y,fill=mean_cat)) +
 facet_wrap(~habitat) +
 scale_fill_manual(values = cols) +
 #geom_sf(data=newEU, fill=NA, color=alpha('black', .5), linewidth=.4) +
 geom_sf(data=newEU, fill=NA, color='grey20', linewidth=.2) +
 labs(fill='Species richness\nchange (2020 vs. 1960)')+
 theme(strip.text = element_text(colour ='black', face = 2, size=10),
       axis.title=element_blank(),
       legend.title = element_text(face='bold', size=10),
       strip.background = element_blank(), 
       strip.placement = 'outside',
       strip.text.x = element_text(hjust = 0, vjust = 1, margin=margin(l=0.25))) +
  guides(fill = guide_legend(reverse=TRUE))
map

ggsave(paste0('./fig/geomaps_stdpltsz/basemap.svg'), map, width = 6*1.25, height = 5*1.25)
