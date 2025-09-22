################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 07.07.2025
################################################################################

# Description: Calculate minimum distance between:
#              1) testing vs. training plots for each decade of the full model;
#              2) ReSurveyEurope vs. EVA plots.

################################################################################


#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(sf)
  library(terra)
})

# Source function to format Resurvey data
source('./src/utils.R')

# Prepare data for modeling and split train and test dataset
set.seed(123)
dat_split <- 
  bind_rows(
    read_csv('./data/input/EVA.csv.gz', show_col_types = F), # Load EVA data
    format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]  # Load ReSurveyEU (static, using one random point in the survey)
  ) %>%
  ## select variables for modeling
  select(plot_id, S, x, y, elev, year, plot_size, habitat) %>%
  ## split the data
  initial_split(prop = 4/5, strata = S) # We will use 80% of the data for training, 20% for testing

# Subset training set
dat_train <- training(dat_split) 

# Load model fit
m <- list.files('./data/models/', pattern = 'RF.last_fit', full.names = T) %>%
  read_rds()

# Collect predictions
pred_test <- augment(extract_workflow(m), testing(dat_split))
pred_test$resid <- (pred_test$S-pred_test$.pred) # model residuals

# Get raster template to populate
pred_years <- seq(1960, 2020, 1)
cell_size_km <- 1
cell_size_m <- cell_size_km*1000
ext_points <- dat_split$data %>% 
  st_as_sf(coords=c('x','y'), crs=25832) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(10000) %>%
  st_bbox()
raster_template <- rast(ext = ext_points, resolution = cell_size_m, crs = st_crs(25832)$wkt)


#### 2. Calculate distance: testing vs. training plots for each decade of the full model ####

decades <- tibble(
  decade_name = c('1961-1970', '1971-1980', '1981-1990', '1991-2000', '2001-2010', '2011-2020'),
  min_year = c(1961, 1971, 1981, 1991, 2001, 2011),
  max_year = c(1970, 1980, 1990, 2000, 2010, 2020)
)

for(i in decades$decade_name) {
 cat(paste0('Period: ',i))
 st = Sys.time()
 # get xy coordinates (and simplify coordinates by 1000 m)
 p_i <- dat_train %>%
   filter(year >= decades[which(decades$decade_name == i),]$min_year) %>%
   filter(year <= decades[which(decades$decade_name == i),]$max_year) %>%
   mutate(x = round(x/1000)*1000,
          y = round(y/1000)*1000) %>%
   select(x,y) %>%
   unique %>%
   as.matrix()
 # transform to spatial vector
 v_i <- vect(p_i, crs=st_crs(25832)$wkt)
 # calc distance
 r_i <- distance(raster_template, v_i)
 resid_i <- pred_test %>%
   filter(year >= decades[which(decades$decade_name == i),]$min_year) %>%
   filter(year <= decades[which(decades$decade_name == i),]$max_year)
 # extract spatial distance
 resid_i$space_dist <- extract(r_i, resid_i %>% select(x,y) %>% as.matrix())[,1]
 resid_i <- mutate(resid_i, decade_name=i, .before=1)
 # export both raster and tabular data of testing data with residuals + extracted spatial distance
 write_csv(resid_i, paste0('./data/spatial/training_data_distance_csv/spatial_distance_',i,'.csv.gz'))
 writeRaster(r_i, paste0('./data/spatial/training_data_distance_rasters/training_data_distance_',i,'.tif'), overwrite=T)
 print(Sys.time()-st)
}


#### 3. Calculate distance: ReSurveyEurope vs. EVA ####
st = Sys.time()
set.seed(123)
# Load ReSurveyEU (static, using one random point in the survey)
ReSuEU <- format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]
# Get dat split
dat_split <- 
  bind_rows(
    read_csv('./data/input/EVA.csv.gz', show_col_types = F), # Load EVA data
    ReSuEU  
  ) %>%
  ## select variables for modeling
  select(database, plot_id, S, x, y, elev, year, plot_size, habitat) %>%
  ## split the data
  initial_split(prop = 4/5, strata = S)
# Subset training data from EVA 
dat_train <- training(dat_split) %>%
  filter(database=='EVA')
# call just resurvey EU data
ReSuEU_xy <- ReSuEU %>%
  select(resurv_id, x_mean, y_mean) %>%
  unique()
# get xy coordinates (and simplify coordinates by 1000 m)
p_i <- dat_train %>%
  mutate(x = round(x/1000)*1000,
         y = round(y/1000)*1000) %>%
  select(x,y) %>%
  unique() %>%
  as.matrix()
# calc distance
v_i <- vect(p_i, crs=st_crs(25832)$wkt)
dp_i <- distance(raster_template, v_i)
# extract spatial distance
ReSuEU_xy$space_dist <- extract(dp_i, ReSuEU_xy %>% select(x_mean, y_mean) %>% as.matrix())[,1]

print(Sys.time()-st)
# export both raster and tabular data of testing data with residuals + extracted spatial distance
write_csv(ReSuEU_xy, './data/spatial/training_data_distance_csv/spatial_distance_ReSurveyEU_from_EVA.csv')
writeRaster(dp_i, './data/spatial/training_data_distance_rasters/training_data_distance_EVA.vs.ReSurveyEU.tif', overwrite=T)


# Quit
quit(save = 'no')