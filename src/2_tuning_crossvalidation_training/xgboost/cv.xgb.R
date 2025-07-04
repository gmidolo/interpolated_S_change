################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 04.07.2025
################################################################################

# Description: Extreme Gradient Boosting random cross-validation 

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
dat_split <- 
  bind_rows(
    read_csv('./data/input/EVA.csv', show_col_types = F), # Load EVA data
    format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]  # Load ReSurveyEU (static, using one random point in the survey)
  ) %>%
  ## select variables for modeling
  select(plot_id, S, x, y, elev, year, plot_size, habitat) %>%
  ## split the data
  initial_split(prop = 4/5, strata = S) # We will use 80% of the data for training, 20% for testing

# Subset training set
dat_train <- training(dat_split) 


#### 2. Random CV ####

# Load results of the tuning procedure
tune_res <- read_rds('./data/models/XGB.tune_res.rds') 

# Select best set of hyperparameters (based on RMSE)
tune_best <- select_best(tune_res, metric = 'rmse')

# Define recipe
rec <- recipe(S ~ x + y + elev + plot_size + year + habitat, data = dat_train) %>%
  step_dummy(all_nominal_predictors(), one_hot = T) # transform habitat to dummy data
  
# Preview recipe data 
rec %>% prep() %>% juice() %>% glimpse() 

# Define model based on tuning results
spec <- boost_tree(
  trees = 1000, 
  loss_reduction = tune(), min_n = tune(), tree_depth = tune(), # model complexity
  sample_size = tune(), mtry = tune(), # randomness
  learn_rate = tune() # shrinkage
) %>%
  set_mode('regression') %>%
  set_engine('xgboost') %>%
  finalize_model(tune_best)

# Define workflow 
wflow <- workflow() %>%
  add_model(spec) %>%
  add_recipe(rec) 
wflow

# Get CV folds 
set.seed(124) 
cv_random_folds <- vfold_cv(dat_train, v = 10, repeats = 3, strata = S) # This time we repeat 3 times CV

# Perform CV
set.seed(125)
st = Sys.time()
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
cv_res <- wflow %>%
  fit_resamples(
    preprocessor = rec,
    resamples = cv_random_folds,
    metrics = metric_set(rmse, rsq)
  )
print(Sys.time()-st)

# Export CV raw results
cv_res %>%
  write_rds('./data/models/XGB.cv_res.rds')

# Export CV metrics
cv_res %>%
  collect_metrics(summarize = F) %>%
  write_csv('./data/models/XGB.cv_metrics.csv')



stopCluster(cl)
doParallel::stopImplicitCluster()
quit(save='no')