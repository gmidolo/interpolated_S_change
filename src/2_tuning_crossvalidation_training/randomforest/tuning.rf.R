#--- TUNING RANDOM FOREST ---#


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
source('./src/validation/functions_valid.R')

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


#### 2. Tuning ####

# Get CV folds
set.seed(234)
cv_random_folds <- vfold_cv(dat_train, v = 10, repeats = 1, strata = S)

# Define recipe
rec <- recipe(S ~ x + y + elev + plot_size + year + habitat, data = dat_train) %>%
  step_string2factor(habitat, levels = c('Forest', 'Grassland', 'Scrub', 'Wetland'))

# Preview recipe data 
rec %>% prep() %>% juice() %>% glimpse() 

# Define model 
spec <- rand_forest(
  trees = 1000, 
  min_n = tune(),
  mtry = tune()
) %>%
  set_mode('regression') %>%
  set_engine('ranger', importance = 'impurity', seed = 1975)

# Define workflow
wflow <- workflow() %>%
  add_model(spec) %>%
  add_recipe(rec)
wflow

# Define tune grid
grd_tune <- expand.grid(
             min_n = c(2,5,10,15,20),
             mtry = c(2:6)) %>%
            as_tibble()
glimpse(grd_tune)

# Perform tuning
set.seed(345)
st = Sys.time()
cl <- makePSOCKcluster(10)
registerDoParallel(cl)
tune_res <- tune_grid(
  object = wflow,
  resamples = cv_random_folds,
  grid = grd_tune,
  # control = control_grid(verbose = T), # This is ignored when running in parallel
  metrics = metric_set(rmse, rsq)
)
print(Sys.time()-st)

# Export tuning results
tune_res %>%
  write_rds('./data/models/RF.tune_res.rds')

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
  write_rds('./data/models/RF.last_fit.rds')

# Assess predictive performance metrics
lfit %>%
  collect_metrics() 

  

stopCluster(cl)
doParallel::stopImplicitCluster()
quit(save='no')