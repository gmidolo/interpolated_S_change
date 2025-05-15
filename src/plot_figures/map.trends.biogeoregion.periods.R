#--- TRENDS MAP IN BIOGEOGRAPHICAL REGIONS USING RANDOM FOREST ---#

# Plot objects used for Figure 4 (Trend maps)

#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(sf)
    library(terra)
  }
)


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
plot(studyarea)
studyarea <- studyarea %>% group_by(biogeo) %>% summarize(geometry=st_union(geometry)) %>% ungroup()

# Load predictions
dat <- read_csv('./data/preds/preds_stdpltsz.rf.csv', show_col_types = F) 
str(dat)

# Filter only dat from 1960 to 2020
dat <- dat %>%
  filter(year >= 1960 & year <= 2020)

# Intersect biogeographical regions
int <- as.list(st_intersects(studyarea, dat %>% st_as_sf(coords=c('x','y'), crs=25832)))
dati=list()
for(i in 1:length(int)){
  dati[[i]] = dat[int[[i]],]
}
names(dati) <- studyarea$biogeo
dat <- dati %>% bind_rows(.id='biogeo')


### 2. Plot trends across biogeographic regions ####
hab <- c('Forest','Grassland','Scrub','Wetland')
bgr <- c('Alpine','Arctic-Boreal','Atlantic','Continental','Mediterranean','Pannonian','Steppic')

# Prepare data for regression
st=Sys.time()
hab_dat <- dat %>%
  select(plot_id, habitat, biogeo, contains('S_pred_')) %>%
  gather('year', 'S_pred', contains('S_pred_')) %>%
  mutate(year = as.numeric(gsub("\\D", "", year))) 
Sys.time()-st

# Add period 
hab_dat_periods <- list(
  hab_dat %>% filter(year>=1960 & year <=1980),
  hab_dat %>% filter(year>=1980 & year <=2000),
  hab_dat %>% filter(year>=2000 & year <=2020)
) %>%
  setNames(c(
    'From 1960 to 1980',
    'From 1980 to 2000',
    'From 2000 to 2020'
  )) %>%
  bind_rows(.id='period')

# Calculate slopes
st=Sys.time()
pred_lm.slope <- hab_dat_periods %>%
  group_by(habitat, biogeo, period) %>%
  do(broom::tidy(lm(S_pred ~ year, data = .))) %>%
  filter(term == 'year')
Sys.time()-st
hist(pred_lm.slope$estimate)
pred_lm.slope$loss_gains_decade <- round(pred_lm.slope$estimate*10, 2) # Change the slope by decade change rate
hist(pred_lm.slope$loss_gains_decade)
pred_lm.slope

# Define theme
my_theme <- theme(
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank(),
  axis.title = element_blank(),
  axis.title.x = element_blank(),
  #axis.text.x = element_blank(),
  #axis.ticks.x = element_blank(),
  axis.line = element_blank(),
  axis.ticks.length=unit(.1, "cm")
  # axis.ticks = element_line(linewidth = 0.3)
)

rangeseq <- round(seq(-max(abs(pred_lm.slope$loss_gains_decade)+0.1), max(abs(pred_lm.slope$loss_gains_decade)+0.1),0.01),2)
colpalval <- as.character(rangeseq)
#colpalval <- as.character(round(seq(-1, 1,0.01),2))
colpalcol <- hcl.colors(length(colpalval), 'Spectral')
colpal <- data.frame(colpalval, colpalcol)
# scales::show_col(colpal$colpalcol, ncol=15, labels = F)

colpal <- colpal %>%
  mutate(mean_cat = as.numeric(colpalval)) %>%
  mutate(mean_cat = cut(
    mean_cat, 
    breaks = c(-Inf,-2, -1, -0.5, -0.25, 0.25, 0.5, 1, 2, Inf), 
    labels = c('< -2','-2 – -1','-1 – -0.5','-0.5 – -0.25','-0.25 – 0.25','0.25 – 0.5', '0.5 – 1','1 – 2', '> 2') ))
colpal <- colpal %>%
  left_join(
    data.frame(mean_cat=unique(colpal$mean_cat), catcol=hcl.colors(length(unique(colpal$mean_cat)), 'Spectral')),
    'mean_cat'
  )

# Plot legend palette
nrmlz <- function(x){(x-min(x))/(max(x)-min(x))}
svg('./fig/biogeo.periods_trendmaps_stdpltsz/legenda.svg', height = 5, width = 3.5)
legend_image <- as.raster(matrix(rev(colorspace::adjust_transparency(colpal$colpalcol, 0.95)), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Species gained/loss\nper decade')
ylabsdata = data.frame(
  rangeseq,
  rangeseq.norm = nrmlz(rangeseq)
)
text(x=1.5, 
     y = ylabsdata[ylabsdata$rangeseq %in% c(-3,-1.5,0,1.5,3),]$rangeseq.norm, 
     labels = c(-3,-1.5,0,1.5,3)
     )
rasterImage(legend_image, 0, 0, 1,1)
dev.off()

## Plot trends palette
# titles = data.frame(labels = c('F', 'G', 'S', 'W'), habitat=hab)

hab.means.1960 <- hab_dat_periods %>% filter(year==1960) %>% group_by(habitat, biogeo) %>% summarise(S_pred_mean_hab=mean(S_pred))

yseq <- seq(-6,6,2)

minitrends <- list()
for (i in bgr) {
  cat(i, ' |> ')
 for (k in hab) {
   message(paste0(k, ' |> '))
   obse_range <- dat %>% filter(biogeo==i) %>% filter(habitat==k) %>% select(habitat, biogeo, year) %>% unique() %>% arrange(year)
   
   rawdik <- hab_dat_periods %>% 
     filter(biogeo==i) %>% filter(habitat==k) #%>%
     #filter(year >= min(obse_range$year) & year <= max(obse_range$year))
     #semi_join(obse_range, by = join_by(habitat, biogeo, year))
   
   dik <- rawdik %>%
     group_by(year) %>%
     summarise(S_sd=sd(S_pred), n=n(), S_pred = mean(S_pred) 
               ) %>% 
     ungroup() #%>%
     # mutate(S_lci = S_pred - (1.96 * (S_sd/sqrt(n))),
     #        U_uci = S_pred + (1.96 * (S_sd/sqrt(n))))
   
   dik_periods <- list(
     dik %>% filter(year>=1960 & year <=1980),
     dik %>% filter(year>=1980 & year <=2000),
     dik %>% filter(year>=2000 & year <=2020)
   ) %>%
     setNames(c(
       'From 1960 to 1980',
       'From 1980 to 2000',
       'From 2000 to 2020'
     )) %>%
     bind_rows(.id='period')
   
   dat4reg <- hab_dat_periods %>% 
     filter(habitat == k & biogeo == i) #%>%
     # semi_join(obse_range, by = join_by(habitat, biogeo, year))
   
   nplots <- dat %>% filter(biogeo==i) %>% filter(habitat==k) %>% summarise(n=n()) %>% pull(n) %>%
     prettyNum(big.mark=",",scientific=FALSE)
   
   sik <- pred_lm.slope %>% 
     filter(biogeo==i) %>% filter(habitat==k) %>% arrange(period) %>%
     #semi_join(obse_range, by = join_by(habitat, biogeo)) %>%
     pull(loss_gains_decade)
   
   color_res <- data.frame(colpalval=as.character(sik)) %>% left_join(colpal, 'colpalval') %>% 
     pull(colpalcol)# %>% #continuous
   #pull(catcol) #categorical
   
   ycentre <- hab.means.1960 %>% filter(habitat==k & biogeo==i) %>% pull(S_pred_mean_hab) %>% round(1)
   ybreaks <- yseq + ycentre
   ylabels <- as.character(yseq)
   ylabels[which(ylabels == 0)] <- format(as.numeric(ycentre), nsmall = 1)
   ylabels[((length(yseq)/2+0.5)+1):length(yseq)] <- paste0('+',ylabels[((length(yseq)/2+0.5)+1):length(yseq)])
   
   minitrends[[i]][[k]] <- ggplot(dik_periods,aes(x=year, y=S_pred)) + 
     annotate('rect', xmin=1960, xmax=1980, ymin=-Inf, ymax=Inf, alpha=.7, fill=color_res[1]) +
     annotate('rect', xmin=1980, xmax=2000, ymin=-Inf, ymax=Inf, alpha=.7, fill=color_res[2]) +
     annotate('rect', xmin=2000, xmax=2020, ymin=-Inf, ymax=Inf, alpha=.7, fill=color_res[3]) +
     # geom_vline(xintercept = 1960,lty=3, linewidth=.15, col ='black')+
     # geom_vline(xintercept = 1980,lty=3, linewidth=.15, col ='black')+
     # geom_vline(xintercept = 2000,lty=3, linewidth=.15, col ='black')+
     # geom_vline(xintercept = 2020,lty=3, linewidth=.15, col ='black')+
     # plot zero change line
     geom_segment(x = 1960, y = ycentre, xend = 2020, yend = ycentre, lty=3, color='grey30', linewidth=.3)+
     geom_path(linewidth = 1, col ='grey35')+
     geom_smooth(data=dik_periods, aes(group = period), method = 'lm', se=F, col='black', lty=1 , linewidth=.5)+ 
     theme_classic()+
     xlim(c(1960,2020))+
     scale_color_gradient2(limits=range(pred_lm.slope$loss_gains_decade), mid=0)+
     my_theme+
     annotate(
       'text',
       x = 1990, y = Inf, label = paste0('n = ', nplots), vjust   = 1, 
       size= 2.75,
     )+
     #ggtitle(titles[titles$habitat == k,]$labels) +
     # scale_y_continuous(breaks = 
     #                      c(min(dik$S_pred), (min(dik$S_pred) + max(dik$S_pred))/2, max(dik$S_pred)) %>%
     #                      round(1)
     #                      )
     scale_y_continuous(breaks = ybreaks, labels = ylabels, limits = c(min(ybreaks)-1, max(ybreaks)+1))
     
 }
}

# # coloring results (color assigned)
# color_res <- color_res %>% map(function(x){as.data.frame(unlist(x)) %>% rownames_to_column('x') %>% setNames(c('habitat', 'col'))}) %>%
#   bind_rows(.id='biogeo')

# Plot Biogeoregion shapes
studyarea_pretty <- studyarea %>%
  st_intersection(st_union(EU %>% st_buffer(4*1000))
) %>%
  st_simplify(dTolerance = 7000)
plot(studyarea_pretty$geometry)

mappette = list()
for (i in bgr) {
  mappette[[i]] <- ggplot()+
    geom_sf(data=st_union(studyarea_pretty$geometry), fill='white', col='black') +
    geom_sf(data=studyarea_pretty %>% filter(biogeo==i), fill='black', col=NA)+
    theme_void()+
    theme(axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5, size=13, face=2)) +
    ggtitle(i)
}

# Finalize: put all things together
aggplt <- cowplot::plot_grid(
  
  mappette$Alpine,
  mappette$`Arctic-Boreal`,
  mappette$Atlantic,
  mappette$Continental,
  mappette$Mediterranean,
  mappette$Pannonian,
  mappette$Steppic,
  
  minitrends[['Alpine']]$Forest,
  minitrends[['Arctic-Boreal']]$Forest,
  minitrends[['Atlantic']]$Forest,
  minitrends[['Continental']]$Forest,
  minitrends[['Mediterranean']]$Forest,
  minitrends[['Pannonian']]$Forest,
  minitrends[['Steppic']]$Forest,
  
  minitrends[['Alpine']]$Grassland,
  minitrends[['Arctic-Boreal']]$Grassland,
  minitrends[['Atlantic']]$Grassland,
  minitrends[['Continental']]$Grassland,
  minitrends[['Mediterranean']]$Grassland,
  minitrends[['Pannonian']]$Grassland,
  minitrends[['Steppic']]$Grassland,
  
  minitrends[['Alpine']]$Scrub,
  minitrends[['Arctic-Boreal']]$Scrub,
  minitrends[['Atlantic']]$Scrub,
  minitrends[['Continental']]$Scrub,
  minitrends[['Mediterranean']]$Scrub,
  minitrends[['Pannonian']]$Scrub,
  minitrends[['Steppic']]$Scrub,
  
  minitrends[['Alpine']]$Wetland,
  minitrends[['Arctic-Boreal']]$Wetland,
  minitrends[['Atlantic']]$Wetland,
  minitrends[['Continental']]$Wetland,
  minitrends[['Mediterranean']]$Wetland,
  minitrends[['Pannonian']]$Wetland,
  minitrends[['Steppic']]$Wetland,

  
  
  #labels = c('F', 'G', 'S', 'W'),
  #label_x = .8,
  ncol = 7)

ggsave('./fig/biogeo.periods_trendmaps_stdpltsz/aggrplt_raw.svg', aggplt, width = 11, height = 7)
ggsave('./fig/biogeo.periods_trendmaps_stdpltsz/aggrplt_raw.pdf', aggplt, width = 11, height = 7)
