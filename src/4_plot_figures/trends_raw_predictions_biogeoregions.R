#--- TRENDS MAP IN BIOGEOGRAPHICAL REGIONS USING RANDOM FOREST ---#

# Plot objects used for Figure 3 
# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(sf)
  }
)

# Define habitat and region names
hab <- c('Forest','Grassland','Scrub','Wetland')
bgr <- c('Alpine','Arctic-Boreal','Atlantic','Continental','Mediterranean','Pannonian','Steppic')

#### 1. Plot minimaps of biogeographic regions ####

# Source function to format ReSurveyEU
source('./src/validation/functions_valid.R')

# Set up 
standardize_plot.size <- TRUE
pred_years <- seq(1960, 2020, 1)
output_file <- './data/preds/pdp/habitat.biogeo_pdp_trends_alltrees.csv'

# Load EU map
regions.name <- c('Albania', 'Austria', 'Baleares', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria',
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

# Load regions
st=Sys.time()
studyarea <- './data/spatial/biogeoregEU/BiogeoRegions2016.shp' %>%
  st_read() %>%
  anti_join(
    data.frame(short_name = c('anatolian','macaronesia','outside'))
  ) %>%
  st_transform(25832) %>%
  st_simplify(dTolerance = 1000) %>%
  st_buffer(5*1000) %>%
  select(code, geometry) %>% 
  st_cast('MULTIPOLYGON') 
studyarea <- studyarea %>% st_cast('POLYGON') %>% st_as_sf() %>% mutate(id=1:nrow(.))

# crop to study area
studyarea <- st_intersection(studyarea, st_union(EU %>% st_buffer(5*1000)))

# Define biogeographic regions
studyarea$biogeo <- ''
#Mediterranean:
studyarea$biogeo <- ifelse(studyarea$code == 'Mediterranean', 'Mediterranean', studyarea$biogeo)
#Alpine:
studyarea$biogeo <- ifelse(studyarea$code == 'Alpine', 'Alpine', studyarea$biogeo)
#Arctic-Boreal:
studyarea$biogeo <- ifelse(studyarea$code == 'Arctic', 'Arctic-Boreal', studyarea$biogeo)
studyarea$biogeo <- ifelse(studyarea$code == 'Boreal', 'Arctic-Boreal', studyarea$biogeo)
studyarea$biogeo <- ifelse(studyarea$code == 'Alpine' & studyarea$id == 10, 'Arctic-Boreal', studyarea$biogeo)
#Temperate-Dry:
studyarea$biogeo <- ifelse(studyarea$code == 'Pannonian', 'Pannonian', studyarea$biogeo)
studyarea$biogeo <- ifelse(studyarea$code == 'Steppic', 'Steppic', studyarea$biogeo)
#Temperate-Humid:
studyarea$biogeo <- ifelse(studyarea$code == 'Continental', 'Continental', studyarea$biogeo)
studyarea$biogeo <- ifelse(studyarea$code == 'BlackSea', 'Continental', studyarea$biogeo)
studyarea$biogeo <- ifelse(studyarea$code == 'Pannonian' & studyarea$id == 160, 'Continental', studyarea$biogeo)
studyarea$biogeo <- ifelse(studyarea$code == 'Alpine' & studyarea$id == 11, 'Continental', studyarea$biogeo)
studyarea$biogeo <- ifelse(studyarea$code == 'Alpine' & studyarea$id == 12, 'Continental', studyarea$biogeo)
studyarea$biogeo <- ifelse(studyarea$code == 'Atlantic', 'Atlantic', studyarea$biogeo)

# union
studyarea <- studyarea %>% group_by(biogeo) %>% summarize(geometry=st_union(geometry)) %>% ungroup()

# Get biogeoregion polygons to plot
studyarea_pretty <- studyarea %>%
  st_intersection(st_union(EU %>% st_buffer(4*1000))
) %>%
  st_simplify(dTolerance = 7000)
biogeomaps = list()
for (i in bgr) {
  biogeomaps[[i]] <- ggplot()+
    geom_sf(data=st_union(studyarea_pretty$geometry), fill='white', col='grey30') +
    geom_sf(data=studyarea_pretty %>% filter(biogeo==i), fill='black', col=NA)+
    theme_void()+
    theme(axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5, size=13, face=2)) +
    ggtitle(i)
}

#### 2.Plot trends ####

# Load predictions per year
dat <- read_csv('./data/preds/pdp/habitat.biogeo_pdp_trends_alltrees.csv', show_col_types = F)
glimpse(dat)

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
vals = round(seq(-3.5, 3.5, 0.01), 2)
pals = hcl.colors(length(vals), 'Spectral')

# Plot legend palette
pdf('./fig/biogeo_trendmaps_stdpltsz/biogeo_legenda.pdf', height = 5, width = 3.5)
legend_image <- as.raster(matrix(rev(colorspace::adjust_transparency(pals, 0.95)), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Species gained/loss\nper decade')
text(x=1.5, y = seq(0,1,l=5), labels = quantile(as.numeric(vals), seq(0,1,.25)))
rasterImage(legend_image, 0, 0, 1,1)
dev.off()

# Calculate changes to the baseline
dat_baseline <- dat %>% filter(year==1960) 
names(dat_baseline)[5:length(dat_baseline)] <- paste0(names(dat_baseline)[5:length(dat_baseline)],'_1960')
dat_cntrd <- dat %>%
  left_join(
    dat_baseline %>% select(-year), by=c('habitat','biogeo','no.plots')
  ) %>%
  group_by(habitat, biogeo, no.plots) %>%
  mutate(
    m = mean-mean_1960,
    lpi = m + (quantile0.05 - median), # lower prediction interval quantile
    upi = m + (quantile0.95 - median), # upper prediction interval quantile
    lci = m + (lower_ci_95 - mean), # lower confidence interval of the mean
    uci = m + (upper_ci_95 - mean) # upper confidence interval of the mean
  ) %>%
  select(habitat, biogeo, year, no.plots, mean_1960, m, lci, uci, lpi, upi)
glimpse(dat_cntrd)

# truncate extreme values
range(c(dat_cntrd$upi,dat_cntrd$lpi))
truncate_at <- 12
dat_cntrd$upi <- ifelse(dat_cntrd$upi >  truncate_at,  truncate_at, dat_cntrd$upi)
dat_cntrd$lpi <- ifelse(dat_cntrd$lpi < -truncate_at, -truncate_at, dat_cntrd$lpi)
range(c(dat_cntrd$lpi, dat_cntrd$upi))

# quick map:
dat_cntrd %>%
  ggplot(aes(year,m))+
  geom_ribbon(aes(ymin = lpi, ymax = upi), fill = "grey70")+
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "grey30")+
  geom_line()+
  facet_wrap(habitat~biogeo, scales='free_y', ncol=7)+
  ylim(c(-truncate_at, truncate_at))

# Plot map
p_means.1960=list()
for(b in bgr){
 for(h in hab){
  
  # define ylims
  y_limits_centrd <- c(-truncate_at, truncate_at)
  # define y labels and breaks
  y_baseline <- round((dat_cntrd %>% filter(habitat==h & biogeo==b) %>% pull(mean_1960) %>% unique()),1)
  y_baseline <- ifelse(grepl("\\.", y_baseline), y_baseline, paste0(y_baseline, ".0"))
  y_labels <- c('-10', '-5', 
              y_baseline, 
              '+5', '+10')
  y_breaks <- seq(-10, 10, 5)

  no.plots_lab <- dat_cntrd %>%
    filter(habitat==h & biogeo==b) %>% 
    pull(no.plots) %>%
    unique() %>%
    format(big.mark=",", scientific=F)

  t1 <- period_slope %>% filter(period=='From 1960 to 1980') %>% filter(habitat==h & biogeo==b) %>% pull(slope_decades)
  t1 <- pals[which(vals %in% round(t1, 2))]
  t2 <- period_slope %>% filter(period=='From 1980 to 2000') %>% filter(habitat==h & biogeo==b) %>% pull(slope_decades)
  t2 <- pals[which(vals %in% round(t2, 2))]
  t3 <- period_slope %>% filter(period=='From 2000 to 2020') %>% filter(habitat==h & biogeo==b) %>% pull(slope_decades)
  t3 <- pals[which(vals %in% round(t3, 2))]
  
  ## Plot relative changes
  phb <- dat_cntrd %>%
    filter(habitat==h & biogeo==b) %>%
    ggplot(aes(year, m)) +

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


    #facet_wrap(~habitat, scales='free_y') +
    labs(y='mean species richness', x='Year') +
    theme_bw()+
    theme(axis.title = element_blank(),
          panel.grid = element_blank())+

    scale_y_continuous(
      limits = y_limits_centrd,
      labels = y_labels,
      breaks = y_breaks
    ) +

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

# Finalize: put all things together
cp <- cowplot::plot_grid(
  
  biogeomaps$Alpine,
  biogeomaps$`Arctic-Boreal`,
  biogeomaps$Atlantic,
  biogeomaps$Continental,
  biogeomaps$Mediterranean,
  biogeomaps$Pannonian,
  biogeomaps$Steppic,
  
  p_means.1960[['Alpine']]$Forest,
  p_means.1960[['Arctic-Boreal']]$Forest,
  p_means.1960[['Atlantic']]$Forest,
  p_means.1960[['Continental']]$Forest,
  p_means.1960[['Mediterranean']]$Forest,
  p_means.1960[['Pannonian']]$Forest,
  p_means.1960[['Steppic']]$Forest,
  
  p_means.1960[['Alpine']]$Grassland,
  p_means.1960[['Arctic-Boreal']]$Grassland,
  p_means.1960[['Atlantic']]$Grassland,
  p_means.1960[['Continental']]$Grassland,
  p_means.1960[['Mediterranean']]$Grassland,
  p_means.1960[['Pannonian']]$Grassland,
  p_means.1960[['Steppic']]$Grassland,
  
  p_means.1960[['Alpine']]$Scrub,
  p_means.1960[['Arctic-Boreal']]$Scrub,
  p_means.1960[['Atlantic']]$Scrub,
  p_means.1960[['Continental']]$Scrub,
  p_means.1960[['Mediterranean']]$Scrub,
  p_means.1960[['Pannonian']]$Scrub,
  p_means.1960[['Steppic']]$Scrub,
  
  p_means.1960[['Alpine']]$Wetland,
  p_means.1960[['Arctic-Boreal']]$Wetland,
  p_means.1960[['Atlantic']]$Wetland,
  p_means.1960[['Continental']]$Wetland,
  p_means.1960[['Mediterranean']]$Wetland,
  p_means.1960[['Pannonian']]$Wetland,
  p_means.1960[['Steppic']]$Wetland,

  ncol = 7)

cp

# export
ggsave('./fig/biogeo_trendmaps_stdpltsz/aggrplt_raw.pdf', cp, width = 11, height = 7)
