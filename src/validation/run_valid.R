#--- VALIDATION VIA RESURVEY DATA & EVA using RANDOM FOREST ---#

# Set seed, optional
set.seed(123) 

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(tidymodels)
  }
)

# Source function to format Resurvey data
source('./src/validation/functions_valid.R')

# Specify hyperparameters
hyperpar <- c(trees = 1000, min_n = 5, mtry = 3)

# Specify formula
pf <- '~ x + y + elev + plot_size + year + habitat'

# Run validation tests on EVA only
st = Sys.time()
EVA_only <- run_validation_RF(datatype = 'EVA', 
                  response_var_name = 'S',
                  predictor_formula = pf,
                  training_strategy_ReSurv = 'random',
                  format_lnRRchange_preds_method = 'end.vs.start',
                  trees_rf = hyperpar[['trees']], mtry_rf = hyperpar[['mtry']], min_n_rf = hyperpar[['min_n']])
print(Sys.time()-st)

# Run validation tests on ReSurveyEU only
st = Sys.time()
ReSurv_only <- run_validation_RF(datatype = 'ReSurv', 
                  response_var_name = 'S',
                  predictor_formula = pf,
                  training_strategy_ReSurv = 'random',
                  format_lnRRchange_preds_method = 'end.vs.start',
                  trees_rf = hyperpar[['trees']], mtry_rf = hyperpar[['mtry']], min_n_rf = hyperpar[['min_n']])
print(Sys.time()-st)

# Run validation tests on EVA+ReSurveyEU
st = Sys.time()
EVA_and_ReSurv <- run_validation_RF(datatype = 'EVA_ReSurv', 
                  response_var_name = 'S',
                  predictor_formula = pf,
                  training_strategy_ReSurv = 'random',
                  format_lnRRchange_preds_method = 'end.vs.start',
                  trees_rf = hyperpar[['trees']], mtry_rf = hyperpar[['mtry']], min_n_rf = hyperpar[['min_n']])
print(Sys.time()-st)

# Put results together
res <- list(
  EVA_only = EVA_only,
  ReSurv_only = ReSurv_only,
  EVA_and_ReSurv = EVA_and_ReSurv
)

# Export 
write_rds(res, './data/validation/multi_test_validation.rds')

quit(save = 'no')