################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 04.07.2025
################################################################################

# Description: Random Forest temporal-block cross-validation 

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
  select(plot_id, S, x, y, elev, year, plot_size, habitat) %>%
  ## focus on 1960-2020 for the temporal CV
  filter(year >=1961 & year <= 2020) 

#### 2. Temporal CV ####

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

# Define temporal blocks
dat_folds <- dat %>%
  mutate(decade_cv = ceiling(year / 10) * 10) %>%
  mutate(decade_cv = factor(decade_cv, levels = sort(unique(decade_cv))))
table(dat_folds$decade_cv) # no. of observations available per block

# Get CV folds
set.seed(124)
cv_temporal_folds <- group_vfold_cv(dat_folds,
                                    group = decade_cv, 
                                    v = length(unique(dat_folds$decade_cv)))


cv_temporal_folds$decade_start <- cv_temporal_folds$splits %>%
  map(~ as.character(unique(assessment(.x)$decade_cv))) %>%
  unlist() # Define end year of each decade

# Perform CV
set.seed(125)
st = Sys.time()
cl <- makePSOCKcluster(10)
registerDoParallel(cl)
cv_res <- wflow %>%
  fit_resamples(
    preprocessor = rec,
    resamples = cv_temporal_folds,
    control = control_resamples(verbose = T),
    metrics = metric_set(rmse, rsq)
  )
print(Sys.time()-st)

# Export CV raw results
cv_res %>%
  write_rds('./data/models/RF.cv.temporal_res.rds')

# Export CV metrics
cv_res %>%
  collect_metrics(summarize = F) %>%
  left_join(cv_temporal_folds[,2:3], by = 'id') %>% # add decade information
  write_csv('./data/models/RF.cv.temporal_metrics.csv')

stopCluster(cl)
stopImplicitCluster()
quit(save='no')