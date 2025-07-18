################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 04.07.2025
################################################################################

# Description: Extreme Gradient Boosting model tuning 

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
    read_csv('./data/input/EVA.csv.gz', show_col_types = F), # Load EVA data
    format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]  # Load ReSurveyEU (static, using one random point in the survey)
  ) %>%
  ## select variables for modeling
  select(plot_id, S, x, y, elev, year, plot_size, habitat) %>%
  ## split the data
  initial_split(prop = 4/5, strata = S) # We will use 80% of the data for training, 20% for testing

# Subset training set
dat_train <- training(dat_split) 


#### 2. Tuning ####

# Get CV folds
set.seed(234)
cv_random_folds <- vfold_cv(dat_train, v = 10, repeats = 1, strata = S)

# Define recipe
rec <- recipe(S ~ x + y + elev + plot_size + year + habitat, data = dat_train) %>%
  step_dummy(all_nominal_predictors(), one_hot = T) # transform habitat to dummy data

# Preview recipe data 
rec %>% prep() %>% juice() %>% glimpse() 

# Define model 
spec <- boost_tree(
  trees = 1000, 
  loss_reduction = tune(), min_n = tune(), tree_depth = tune(), # model complexity
  sample_size = tune(), mtry = tune(), # randomness
  learn_rate = tune() # shrinkage
) %>%
  set_mode('regression') %>%
  set_engine('xgboost')

# Define workflow
wflow <- workflow() %>%
  add_model(spec) %>%
  add_recipe(rec)
wflow

# Define tune grid
set.seed(345)
grd_tune <- grid_latin_hypercube(
  tree_depth(), min_n(), loss_reduction(), # model complexity
  sample_size = sample_prop(), mtry(range = c(2L, 9L)), # randomness
  learn_rate(), #shrinkage
  size = 50 # Set the total number of parameter value combinations returned
)
glimpse(grd_tune)

# Plot tuning grid
gather(grd_tune) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram(bins=20) + 
  facet_wrap(~key, scales = 'free') +
  theme_bw()

# Perform tuning
set.seed(456)
st = Sys.time()
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
tune_res <- tune_grid(
  object = wflow,
  resamples = cv_random_folds,
  grid = grd_tune,
  # control = control_grid(verbose = T),
  metrics = metric_set(rmse, rsq)
)
print(Sys.time()-st)

# Export tuning results
tune_res %>%
  write_rds('./data/models/XGB.tune_res.rds')

# Visualize tuning result
autoplot(tune_res) + theme_bw()

# Display the best performing sets of parameters
show_best(tune_res, metric = 'rmse')

# Select the best performing sets of parameters
tune_best <- select_best(tune_res, metric = 'rmse')
tune_best

#### 3. Fit models with best hyperparameters ####

# Finalize workflow, fit on training data and test on testing data
lfit <- wflow %>%
  finalize_workflow(tune_best) %>%
  last_fit(dat_split)

# Export last fit
lfit %>%
  write_rds('./data/models/XGB.last_fit_.rds')

# Assess predictive performance metrics
lfit %>%
  collect_metrics() 


stopCluster(cl)
stopImplicitCluster()
quit(save='no')