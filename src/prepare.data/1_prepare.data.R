# 1. load pkgs####
library(tidyverse);library(sf);library(terra)

username <- str_split(getwd(), '/')[[1]][3]
setwd(paste0('C:/Users/', username, '/OneDrive - CZU v Praze/czu/intrplEU/'))


# 2. Load header data####

renames <- c('plot_id', 'dataset', 'lon', 'lat', 'country', 'plot_size', 'ESy', 'full_recording_date', 'location_uncertainty', 'for_EVA_y_n', 'ReSurvey_y_n', 'ReSurvey_type', 'ReSur_site', 'ReSur_plot', 'ReSur_obs', 'manipulate_y_n', 'ReSur_time','ReSur_dupl','manipulation_type')

## read EVA + ReSurvey 222 core data
EVA_ReSu <- 
  './data/eva/222_PlantDiversity20241017_notJUICE/222_PlantDiversity20241017_notJUICE_header.csv' %>%
  read_delim(col_select = c('PlotObservationID','Dataset', 'Longitude', 'Latitude', 'Country', 'Relevé area (m²)', 'Expert System','Date of recording', 'Location uncertainty (m)', 'For EVA (Y/N)', 'ReSurvey plot (Y/N)',
                            'RS_PROJTYP', 'ReSurvey site', 'ReSurvey plot', 'ReSurvey observation', 'Manipulate (y/n)','RS_TIME','RS_DUPL','Type of manipulation')
             ) 
## add GVRD-Germany data
EVA_ReSu <- bind_rows(EVA_ReSu,
                    './data/eva/222_PlantDIversity20241025_GVRD/222_GVRD_20241025_notJUICE_header.csv' %>%
                      read_delim(col_select = colnames(EVA_ReSu), show_col_types = F)
                      )

## add UKFloodplainMeadows data
EVA_ReSu <- bind_rows(EVA_ReSu,
                      './data/eva/222_UKFloodplainMeadows20241031/222_UKFloodplainMeadows20241031_notJUICE_header.csv' %>%
                        read_delim(col_select = colnames(EVA_ReSu), show_col_types = F) %>%
                        mutate(`ReSurvey site` = as.character(`ReSurvey site`))
                      )

## rename colnames
EVA_ReSu <- setNames(EVA_ReSu, renames)

## subset Denmark Naturdata metadata 
EVA_ReSu_DN <- EVA_ReSu %>%
  filter(str_detect(dataset,'Denmark Natur'))

## read Denmark Naturdata data with habitat
rawDenNat <- './data/eva/Naturdata_Eunis-RSy.csv' %>%
  read_delim(delim=';', show_col_types = F,
             col_select = c('PlotObservationID','HABITAT','Eunis-ESy', 'EUNIS-ESy level 1')
             ) 
rawDenNat <- setNames(rawDenNat, c('plot_id','DenNatdat_habitat','ESy_max', 'ESy1'))
rawDenNat$ESy <- ifelse(is.na(rawDenNat$ESy_max), rawDenNat$ESy1, rawDenNat$ESy_max)

## clean EVA_ReSu from old Denmark data and bind the new version to it
EVA_ReSu <- EVA_ReSu %>%
  anti_join(EVA_ReSu_DN, 'plot_id')
EVA_ReSu_DN <- EVA_ReSu_DN %>% 
  select(-ESy) %>% 
  left_join(rawDenNat %>% select(plot_id, ESy), 'plot_id')
EVA_ReSu <- EVA_ReSu %>%
  bind_rows(EVA_ReSu_DN)

## arrange by plot id
EVA_ReSu <- EVA_ReSu %>%
  arrange(plot_id)


# 3. Apply standard EVA filters (habitat, plot size, and year) #### 

## load habitat conversion table
habitat_conversion <- 
  paste0('./data/eva/habitat_conversion_table.csv') %>%
  read_csv(show_col_types = F) %>%
  arrange(habitat, ESy) %>%
  semi_join(data.frame(habitat=c('forest','grassland','scrub','wetland'))) %>%
  mutate(habitat = str_to_title(habitat)) %>%
  select(ESy, habitat)
head(habitat_conversion)

## filter habitat needed and categorize them into 'forest','grassland','scrub',or 'wetland'
EVA_ReSu <- EVA_ReSu %>% 
  semi_join(habitat_conversion, 'ESy') %>%
  left_join(habitat_conversion, 'ESy') 

EVA_ReSu %>% # explore distribution of the data (plot sizes)
  ggplot(aes(x=plot_size)) +
  geom_histogram() +
  scale_x_continuous(trans = 'log10', breaks = c(1, 10 ,100, 1000))+
  facet_wrap(~habitat, scales = 'free') +
  theme_bw()

## Define filtering table
plot_size_table <- data.frame(
  habitat = c('Forest', 'Grassland', 'Scrub', 'Wetland'),
  min_size = c(100, 1, 1, 1),
  max_size = c(1000, 100, 100, 100)
)

## Apply dedicated filter
keep_size <- list()
for (i in plot_size_table$habitat) {
  keep_size[[i]] <- EVA_ReSu %>%
    filter(habitat == i) %>%
    mutate(keep = plot_size >= plot_size_table[which(plot_size_table$habitat %in% i), 'min_size'] &
                  plot_size <= plot_size_table[which(plot_size_table$habitat %in% i), 'max_size'])
  
  message(i)
  print(round(prop.table(table(keep_size[[i]]$keep))*100, 2))
  
  keep_size[[i]] <- keep_size[[i]] %>% filter(keep) %>% select(plot_id) 
}
EVA_ReSu <- EVA_ReSu %>%
  semi_join(bind_rows(keep_size))

## Visually check again the distribution of plots
EVA_ReSu %>% # re-explore distribution of the data (plot sizes)
  ggplot(aes(x=plot_size)) +
  geom_histogram(bins = 10) +
  scale_x_continuous(trans = 'log10', breaks = c(1, 10 ,100, 1000))+
  facet_wrap(~habitat, scales = 'free') +
  theme_bw() + labs(x='Plot size (squared m)', y='Plot count')

## Extract year
EVA_ReSu$year <- lubridate::year(as.Date(EVA_ReSu$full_recording_date, format = "%d.%m.%Y"))
prop.table(table(!is.na(EVA_ReSu$year)))# proportion of plots that have a date
# barplot(table(lubridate::wday(as.Date(EVA_ReSu$full_recording_date, format = "%d.%m.%Y"), label=T))) # Easter egg: which day of the week is the most sampled?
# barplot(table(lubridate::month(as.Date(EVA_ReSu$full_recording_date, format = "%d.%m.%Y"), label=T))) # Most sampled months, here you can see that we have several outliers in January

# Retain only plots with sampling year
EVA_ReSu <- EVA_ReSu %>% 
  filter(!is.na(year))

# Retain only plots with sampling year >= 1945
EVA_ReSu <- EVA_ReSu %>% 
  filter(year >= 1945)

EVA_ReSu %>%
  ggplot(aes(x=year)) +
  geom_histogram() + theme_bw()


# 4. Clip out geographical outliers ####

## Define projection
prj = 25832 

## Define EU study area
EU <- read_sf(list.files(paste0('C:/Users/',username,'/OneDrive - CZU v Praze/brno/brno_postdoc/maps/euro+med_map/Final.Map'), pattern = 'shp',full.names = T)) %>%
  semi_join(data.frame(name =
                         c('Albania', 'Austria', 'Baleares', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria',
                           'Corsica', 'Crete', 'Croatia', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany',
                           'Greece', 'Hungary', 'Ireland', 'Italy', 'Kaliningrad Region', 'Kosovo', 'Latvia', 'Liechtenstein',
                           'Lithuania', 'Luxembourg', 'Malta', 'Moldova', 'Montenegro', 'Netherlands', 'North Macedonia',
                           'Norway', 'Poland', 'Portugal', 'Romania', 'Sardinia', 'Serbia', 'Sicily',
                           'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Ukraine', 'United Kingdom'
                         )), 'name') %>%
  st_transform(crs = prj) %>%
  st_simplify(dTolerance = 1000)

## Define various boundaries
EU_51 <-  EU %>% st_buffer(51*1000) # 51 km buffer
EU_51 <- st_as_sf(st_union(EU_51$geometry)) # apply union
EU_1 <-  EU %>% st_buffer(1*1000) # 1 km buffer
EU_1 <- st_as_sf(st_union(EU_1$geometry)) # apply union

## Spatialize EVA_ReSu
EVA_ReSu_spat <- EVA_ReSu %>%
  st_as_sf(coords=c('lon','lat'), crs='WGS84', remove = F) %>%
  st_transform(crs = prj)

st = Sys.time()
out_of_bounds_51km <- st_disjoint(EVA_ReSu_spat, EU_51, sparse = F)
out_of_bounds_51km <- EVA_ReSu_spat[out_of_bounds_51km[,1], ]
Sys.time()-st

nrow(out_of_bounds_51km)
plot(out_of_bounds_51km %>% filter(lat>0) %>% pull(geometry), col='red', pch=16)
plot(EU, add=T, col=NA)

EVA_ReSu_spat <- EVA_ReSu_spat %>% 
  anti_join(out_of_bounds_51km %>% st_drop_geometry(), 'plot_id')

st = Sys.time()
out_of_bounds_1km <- st_disjoint(EVA_ReSu_spat, EU_1, sparse = F)
out_of_bounds_1km <- EVA_ReSu_spat[out_of_bounds_1km[,1], ]
Sys.time()-st

out_Ukraine <- out_of_bounds_1km %>%
  filter(country == 'Ukraine') %>%
  filter(lat <= 44.6)

out_Turkey <- out_of_bounds_1km %>%
  filter(country == 'Turkey')

out_Bulgaria <- out_of_bounds_1km %>%
  filter(country == 'Bulgaria')

additional2remove <- data.frame(plot_id = c(1446784,1446799,1446782,581041,581040,1292359,1292360,1292358,585064, 581355, 581356, 579300, 579301,1446821, 1446814,1446808,1317370,1655784, 1651442, 1651210, 1651279, 1217421, 1124489, 1952089,1933067,1933040,1933008,489110,1921689,1937776,1951739,1937835,1937782,1921696,1922017,1922018,1286433,406643,405638, 1974678,2033112,1765829,1771790,1773577,534422,525083,523767,536828,1124344,1124885,1234578,1217445,1222124,1214332,1231553,1240552,1217899,1216096,1214740,1652505,1653657,1653928,1654752,1654964,1652067,1651920,1651563,1651562,1651588,1651561,1651560,1650905,1651636,1651622,1650900,1651623,1654287,1653114,1651758,1124708,1124858,596818,596737,596776,1339767,1125490,1317024,1317024,2025559,1341032,1360578,1118307,1125428,1124426,2335993,1127698,1130925,1141162,1153547,1174787,1169726,1150801,1141097,1151369,1151436,1141094,1151414,1158969,1174147,1166467,1153811,1158967,1166698,1153768,1166858,1151486,1153669,1153671,1133366,1162479,1153536,1151469,1151478,1174067,1151402,2335842,1298701,1300305,581429,581297,583791,580668,580667,2020452,797388,1124701,1450641,1125043,1124683,1444884,578300,578377,578389,580495,2027287,1125087,1125090)) %>% 
  left_join(EVA_ReSu,'plot_id') %>% 
  select(lon, lat) %>% unique() %>% 
  left_join(EVA_ReSu %>% select(plot_id, lon, lat), c('lon','lat')) %>%
  st_as_sf(coords=c('lon','lat'), crs='WGS84', remove = F)
#mapview::mapview(additional2remove)

EVA_ReSu_spat <- EVA_ReSu_spat %>% 
  anti_join(out_Ukraine %>% st_drop_geometry(), 'plot_id') %>% 
  anti_join(out_Turkey %>% st_drop_geometry(), 'plot_id') %>% 
  anti_join(out_Bulgaria %>% st_drop_geometry(), 'plot_id') %>%
  anti_join(additional2remove %>% st_drop_geometry(), 'plot_id')

# #check again 
st = Sys.time()
out_of_bounds_1km <- st_disjoint(EVA_ReSu_spat, EU_1, sparse = F)
out_of_bounds_1km <- EVA_ReSu_spat[out_of_bounds_1km[,1], ]
Sys.time()-st

## Inspect location uncertainty (we keep everything below 1 km)
EVA_ReSu$location_uncertainty %>% hist(main = 'Distribution of location uncertainty', xlab = 'Uncertainty (m)')
table(EVA_ReSu$location_uncertainty <= 1000 | is.na(EVA_ReSu$location_uncertainty)) # plots with location unc. less than 1 km; or NA


# 5. Extract elevation ####

## Load elevation raster data
ele <- paste0('C:/Users/',username,'/OneDrive - CZU v Praze/brno/brno_postdoc/climdata/topodata/elevation/Copernicus_GLO.90_DEM_Europe.tif') %>%
  rast()

## add elevation to plot (take a 2-3 minutes)
st=Sys.time()
EVA_ReSu_spat$elev <- extract(ele, st_transform(EVA_ReSu_spat, crs = crs(ele, proj=T)))$r1
Sys.time()-st


# 6. Assign to either EVA or ReSurveyEU ####

## Transform it back to EVA_ReSu
EVA_ReSu <- EVA_ReSu_spat %>%
  as_Spatial() %>%
  as.data.frame() %>%
  rename(x = coords.x1 , y =  coords.x2)

## Let's re-arrange a bit the order of the variables
EVA_ReSu <- EVA_ReSu %>%
  select(plot_id, dataset, country, lon, lat, x, y, habitat, year,everything())

## Now, isolate ReSurveyEU...
ResEU <- EVA_ReSu %>% 
  filter(ReSurvey_y_n=='Y') %>%
  filter(manipulate_y_n == 'N') %>%
  # name database
  mutate(database = 'ReSurveyEU', .before = plot_id)

## ... and isolate EVA...
EVA <- EVA_ReSu %>% 
  filter(ReSurvey_y_n != 'Y' | is.na(ReSurvey_y_n)) %>%
  filter(for_EVA_y_n == 'Y' | is.na(for_EVA_y_n)) %>%
  # name database
  mutate(database = 'EVA', .before = plot_id)

# percentages of plots loss from splitting EVA and ReSurveyEU
(1 - ((nrow(EVA) + nrow(ResEU))/ nrow(EVA_ReSu)))*100

## ... and put them back together.
EVA_ReSu <- bind_rows(EVA, ResEU)
head(EVA_ReSu)


# 7. Prepare species data ####
 
## Load core EVA species data
species_data <- read_delim(
  './data/eva/222_PlantDiversity20241017_notJUICE/222_PlantDiversity20241017_notJUICE_species.csv',
  col_select = c(1,3,6,9,10), show_col_types = F
) %>%
  bind_rows( ## add GVRD species data
    read_delim(
      './data/eva/222_PlantDIversity20241025_GVRD/222_GVRD_20241025_notJUICE_species.csv',
      col_select = c(1,3,6,9,10), show_col_types = F
    ) 
  ) %>%
  bind_rows( ## add UKFloodplainMeadows species data
    read_delim(
      './data/eva/222_UKFloodplainMeadows20241031/222_UKFloodplainMeadows20241031_notJUICE_species.csv',
      col_select = c(1,3,6,9,10), show_col_types = F
    ) 
  ) %>%
  setNames(c('plot_id','taxongroup', 'matched_concept', 'layer', 'cover')) %>% # rename columns
  semi_join(EVA_ReSu, 'plot_id') # filter out only the plots we selected this far

## check taxa unknown in EVA: are they vascular plants?
unk_taxa_freq <- species_data %>% 
  filter(taxongroup == 'Unknown') %>% 
  select(plot_id, matched_concept) %>% ungroup() %>% unique() %>%
  group_by(matched_concept) %>%
  summarise(n = n()) # check frequency of Unknown taxa in the dataset
unk_taxa <- species_data %>% 
  filter(taxongroup == 'Unknown') %>% 
  select(matched_concept) %>% 
  unique() 
unk_genera <- unk_taxa %>% 
  mutate(matched_concept= str_remove(str_remove(matched_concept,'Cf. '),'cf. ') %>% str_remove('cf ')) %>% 
  separate(matched_concept, into=c('genus'), sep=' ') %>%
  mutate(genus=str_remove_all(genus, '\\*'))
#remotes::install_github('helixcn/plantlist') # install plantlist R package
check_unk_genera <- plantlist::TPL(unk_genera$genus) # search the genus for higher plants under modern classification systems
check_unk_genera <- unique(check_unk_genera) %>% select(YOUR_SEARCH,GROUP) %>% setNames(c('genus', 'GROUP'))
check_unk_genera$GROUP <- ifelse(is.na(check_unk_genera$GROUP), 'NotFound', check_unk_genera$GROUP)
unk_genera <- unk_genera %>% left_join(check_unk_genera, 'genus')
unk_genera$keep_unk_genera  <- unk_genera$GROUP %in% (c('Angiosperms', 'Ferns and lycophytes', 'Gymnosperms'))
unk_taxa_2keep <- unk_taxa[unk_genera$keep_unk_genera, ]

## list of vascular taxa (will be used to filter the vascular plant upon which to calculate species richness)
vascular_taxa <- species_data %>%
  filter(taxongroup == 'Vascular plant') %>% 
  select(matched_concept) %>%
  unique() %>%
  bind_rows(unk_taxa_2keep) %>% # bind checked names
  arrange(matched_concept)


# 8. Calculate vascular species richness ####
Sric <-  species_data %>%
  semi_join(vascular_taxa, 'matched_concept') %>%
  select(plot_id, matched_concept) %>%
  ungroup() %>%
  unique() %>%
  group_by(plot_id) %>%
  summarise(S = n()) %>%
  ungroup()
hist(Sric$S)
range(Sric$S)

table(Sric$plot_id %in% EVA_ReSu$plot_id)
table(EVA_ReSu$plot_id %in% Sric$plot_id )

## Only select plots with actual calculable vascular species richness
# plots_with_NA_richness <- EVA_ReSu[!(EVA_ReSu$plot_id %in% Sric$plot_id), ]
# View(species_data %>% semi_join(plots_with_NA_richness)) # inspect species list (there are no vascular plants collected)
EVA_ReSu <- EVA_ReSu[EVA_ReSu$plot_id %in% Sric$plot_id, ]
table(EVA_ReSu$plot_id %in% Sric$plot_id )

# # # Suspiciously high S data #
# dat_high_S <- Sric %>%
#   filter(S >= 100)
#  
# meta_high_S <-  read_delim('./data/eva/222_PlantDiversity20241017_notJUICE/222_PlantDiversity20241017_notJUICE_header.csv') %>%
#   mutate(Altitude=as.numeric(Altitude), `Slope (°)`=as.numeric(`Slope (°)`)) %>%
#   bind_rows(read_delim('./data/eva/222_PlantDIversity20241025_GVRD/222_GVRD_20241025_notJUICE_header.csv')) %>%
#   semi_join(dat_high_S, by = join_by('PlotObservationID' == 'plot_id')) %>%
#   left_join(dat_high_S %>% select(plot_id, S), by = join_by('PlotObservationID' == 'plot_id')) %>%
#   select(PlotObservationID, S, everything()) %>%
#   rename(Vascular.Species.Richness=S) %>%
#   arrange(PlotObservationID)
# 
# spd_high_S <- species_data %>%
#   semi_join(vascular_taxa, 'matched_concept') %>%
#   semi_join(dat_high_S, 'plot_id') %>%
#   rename(PlotObservationID=plot_id)%>%
#   arrange(PlotObservationID)
# write_csv(meta_high_S, './data/S_outliers/header.csv')
# write_csv(spd_high_S , './data/S_outliers/species.csv')

## Load richness outliers, checked by I. Knollova and M. Chytry
richness_outliers <- readxl::read_excel('./data/high.richnes.outliers_IlonaKnollova.xlsx') %>%
  select(1) %>%
  setNames(c('plot_id'))


## Remove richness outliers
Sric <- Sric %>%
  anti_join(
    richness_outliers
  )

EVA_ReSu <- EVA_ReSu %>% 
  semi_join(Sric, 'plot_id') %>% # filter header data accordingly
  left_join(Sric)

dat_high_S_check <- Sric %>%
  filter(S >= 100)


#9. Finalize data & export ####

EVA_ReSu <- EVA_ReSu %>%
  filter(country != 'Turkey') # remove unwanted countries, ca 7 plots are left and labelled as Turkey

## Finalize EVA data
EVA <- EVA_ReSu %>%
  filter(database == 'EVA') %>%
  select(
    database, plot_id, dataset, ESy, S, habitat, lon, lat, x, y, elev, year, plot_size 
  ) %>%
  arrange(plot_id)

## Finalize ReSurveyEU data
ResEU <- EVA_ReSu %>%
  filter(database == 'ReSurveyEU') %>%
  select(
    database, plot_id, dataset, ESy, S, habitat, lon, lat, x, y, elev, year, plot_size,
    ReSurvey_type, ReSur_site, ReSur_plot, ReSur_obs, ReSur_time
  ) 
ResEU$ReSurvey_type %>% table()
ResEU$ReSur_type <- ifelse(str_detect(ResEU$ReSurvey_type, 'esampling'), 'Resampling', 'Permanent')
ResEU$ReSur_type %>% table() %>% prop.table()
ResEU <- ResEU %>%
  select(database, plot_id, contains('ReSur'), everything()) %>%
  arrange(plot_id) %>%
  select(-ReSurvey_type)

ggplot(bind_rows(EVA,ResEU), aes(S, fill = database)) + geom_density(alpha = 0.5) + theme_bw()
ggplot(bind_rows(EVA,ResEU), aes(S, fill = database)) + geom_histogram(alpha = 0.5, col='grey85') + theme_bw()

## Export EVA data
write_csv(EVA, './data/input/EVA.csv')

## Export ReSurveyEU data
write_csv(ResEU, './data/input/ReSurveyEU.csv')

# Pretty maps
library(tidyverse);library(sf);library(terra)
username <- str_split(getwd(), '/')[[1]][3]
setwd(paste0('C:/Users/', username, '/OneDrive - CZU v Praze/czu/intrplEU/'))

prj = 25832 
EU <- read_sf(list.files(paste0('C:/Users/',username,'/OneDrive - CZU v Praze/brno/brno_postdoc/maps/euro+med_map/Final.Map'), pattern = 'shp',full.names = T)) %>%
  semi_join(data.frame(name =
                         c('Albania', 'Austria', 'Baleares', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria',
                           'Corsica', 'Crete', 'Croatia', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany',
                           'Greece', 'Hungary', 'Ireland', 'Italy', 'Kaliningrad Region', 'Kosovo', 'Latvia', 'Liechtenstein',
                           'Lithuania', 'Luxembourg', 'Malta', 'Moldova', 'Montenegro', 'Netherlands', 'North Macedonia',
                           'Norway', 'Poland', 'Portugal', 'Romania', 'Sardinia', 'Serbia', 'Sicily',
                           'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Ukraine', 'United Kingdom'
                         )), 'name') %>%
  st_transform(crs = prj) %>%
  st_simplify(dTolerance = 1000)
EU <- EU %>% st_buffer(1*1000)
EU <- st_as_sf(st_union(EU$geometry)) 

d2p <- bind_rows(ResEU %>% select(database, x, y), EVA %>% select(database, x, y)) 

p <- ggplot() + 
  geom_sf(data = EU, fill='#e6e2df') +
  geom_hex(data=d2p, aes(x, y), bins = 70, alpha = 0.8) +
  scale_fill_viridis_c(trans='log10', option='C') +
  facet_wrap(~database) +
  theme_bw() +
  theme(axis.title = element_blank()) +
  labs(fill = 'N. of plots')
p

ggsave('./fig/lon_lat_plot.pdf', p, width = 8, height = 4.6)
ggsave('./fig/lon_lat_plot.jpg', p, width = 8, height = 4.6, dpi = 600)
