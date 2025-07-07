################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 07.07.2025
################################################################################

# Description: Compute Moran's I for 250-km grid cells

################################################################################


#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(sf)
  library(terra)
  library(ncf)
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

# Correlogram of residuals
pred_test_sf <- pred_test %>%
  st_as_sf(coords=c('x','y'), crs = 25832, remove=F)

# Generate grid
cellsize_km = 250
pred_test_grd <- st_make_grid(pred_test_sf, cellsize=c(cellsize_km*1000, cellsize_km*1000), square=F) %>%
  st_as_sf() %>%
  st_filter(pred_test_sf) %>%
  mutate(id=1:nrow(.))
plot(pred_test_grd)# hexagons have 144337.6 m side 

# Calculate moran I for each distance
no_resamp = 1000
st = Sys.time() # takes approx. 7 h; lower the number of resamples (`no_resamp`) to reduce computational costs
res_correlog <- list()
res_correlog_data <- list()
for (i in 1:nrow(pred_test_grd)){
  cat(i, ' - ')
  di <- pred_test_sf %>% 
   st_filter(pred_test_grd[i,])
  print(paste0('no. plots: ', nrow(di)))
  if (nrow(di) < 30) { # skip the calculation for the cell if there are less than 30 plots
    res_correlog[[i]] = NULL
  } else {
    if(nrow(di) > 1000){ # randomly sample 1000 plots if there are more than 1000 plots in a cell
      di <- di %>% sample_n(1000)
    }
    moran_correlog <- spline.correlog(x = di$x, y = di$y, z = di$resid, resamp = no_resamp, npoints = 500)
    # extract & store variables for plotting correlograms
    distance_meters <- round(as.numeric(moran_correlog$real$predicted$x))
    moranI <- as.numeric(moran_correlog$real$predicted$y)
    res_correlog[[i]]  <- moran_correlog
    res_correlog_data[[i]] <- data.frame(distance_meters, moranI)
  }
}
Sys.time() - st

# Bind results
res_correlog_df <- bind_rows(
  res_correlog_data, .id='id' 
)

# Export
write_csv(
  res_correlog_df, './data/spatial/moran_correlog_data/correlog_df.csv'
)

# Quit
quit(save='no')