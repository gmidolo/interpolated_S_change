################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 01.09.2025
################################################################################

# Description: Spatial block cross-validation for Random Forests

################################################################################

#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(tidymodels)
    library(doParallel)
  }
)

# Source function to format Resurvey data
source('./src/utils.R')

# Prepare data for modeling and split train and test dataset
set.seed(123)
dat <- 
  bind_rows(
    read_csv('./data/input/EVA.csv.gz', show_col_types = F), # Load EVA data
    format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]  # Load ReSurveyEU (static, using one random point in the survey)
  ) %>%
  ## select variables for modeling
  select(plot_id, S, x, y, elev, year, plot_size, habitat) 

#### 2. Spatial-block CV ####

# Load results of the tuning procedure
tune_res <- read_rds('./data/models/RF.tune_res.rds') 

# Select best set of hyperparameters (based on RMSE)
tune_best <- select_best(tune_res, metric = 'rmse')

# Define recipe
rec <- recipe(S ~ x + y + elev + plot_size + year + habitat, data = dat) %>%
  step_string2factor(habitat, levels = c('Forest', 'Grassland', 'Scrub', 'Wetland'))

# Define model based on tuning results
spec <- rand_forest(
  trees = 1000, 
  min_n = tune(),
  mtry = tune()
) %>%
  set_mode('regression') %>%
  set_engine('ranger', importance = 'impurity', seed = 1975) %>%
  finalize_model(tune_best)

# Define workflow
wflow <- workflow() %>%
  add_model(spec) %>%
  add_recipe(rec) %>%
  finalize_workflow(tune_best)
wflow


# Loop over different size blocks
grid_km_list <- c(1, 10, 100) # blocks of 1, 10, and 100 km
nfolds <- 10 # set 10-fold CV

for (i in grid_km_list) {
  # Define spatial blocks
  ## Value in meters to aggregate coordinates
  grid_size_km <- i # grid size in km
  fact <- grid_size_km * 1000 # grid size in meters
  
  ## Aggregate in space
  dat_folds <- dat %>%
    mutate(x_aggr = round(x / fact, 0) * fact,
           y_aggr = round(y / fact, 0) * fact) %>%
    group_by(x_aggr, y_aggr) %>%
    mutate(fold_id = cur_group_id(), .before = plot_id) %>%
    ungroup()
  # table(dat_folds$fold_id) # no. of observations available per block
  
  # reshuffle and force to factor (optional; better for viz)
  dat_folds$fold_id <-
    factor(dat_folds$fold_id, levels = sample(unique(dat_folds$fold_id)))
  
  # # plot folds (optional)
  # dat_folds %>%
  #   sample_n(99999) %>% #select just a few plots for visualization
  #   ggplot(aes(x, y, col=as.numeric(fold_id))) +
  #   geom_point() +
  #   scale_color_viridis_c() +
  #   labs(col='Fold ID')
  
  # dat_sf <- sf::st_as_sf(
  #   dat, coords = c("x", "y"), crs = 25832
  # ) %>%
  #   st_transform()
  
  # Get CV folds
  cv_spatial_folds <- group_vfold_cv(dat_folds,
                                     group = fold_id,
                                     v = nfolds)
  
  # # plot a fold
  # fold_no = 1 # e.g. plot fold no. 1
  # f2p <- bind_rows(
  #   analysis(cv_spatial_folds$splits[[fold_no]]) %>% mutate(fold_type='Train'),
  #   assessment(cv_spatial_folds$splits[[fold_no]]) %>% mutate(fold_type='Test')
  # )
  # f2p %>% #select just a few plots for visualization
  #   ggplot(aes(x, y, col=fold_type)) +
  #   geom_point() +
  #   labs(col='Fold type')
  
  # check that 1/10 (for 10-fold cv) blocks are used for testing
  # f2p %>%
  #   select(fold_type, fold_id) %>%
  #   unique %>%
  #   pull(fold_type) %>%
  #   table() %>%
  #   prop.table()
  
  # Perform CV
  st = Sys.time()
  cl <- makePSOCKcluster(10)
  registerDoParallel(cl)
  cv_res <- wflow %>%
    fit_resamples(
      preprocessor = rec,
      resamples = cv_spatial_folds,
      control = control_resamples(verbose = T),
      metrics = metric_set(rmse, rsq)
    )
  print(Sys.time() - st)
  
  # Export CV raw results
  cv_res %>%
    write_rds(
      paste0(
        './data/models/RF.cv.spatial_',
        grid_size_km,
        'km_res.rds'
      )
    )
  
  # Export CV metrics
  cv_res_metrics <- cv_res %>%
    collect_metrics(summarize = F)
  print(head(cv_res_metrics))
  
  cv_res_metrics %>%
    write_csv(
      paste0(
        './data/models/RF.cv.spatial_',
        grid_size_km,
        'km_metrics.csv'
      )
    )
  
  stopCluster(cl)
  stopImplicitCluster()
}

# quit
quit(save='no')