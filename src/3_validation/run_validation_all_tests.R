########################################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 07.07.2025
########################################################################################################

# Description: Validation of species richness interpolation from `static` data (no temporal replication)
#              Models trained on 
#              A) EVA only,
#              B) static ReSurveyEurope only,
#              C) EVA + static ReSurveyEurope;
#              Performance (= RMSE, rsq) assessed on:
#              1) `internal_static` = Standard 20% test set
#              2) `external_static` = Independent ReSurveyEurope plots (not used in training/testing).
#              3) `external_change` = Species richness change (log-response ratio) in ReSurveyEurope.

########################################################################################################


# Set seed (optional)
set.seed(123) 

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
})

# Source function to run validation analyses (=`run_validation_RF()`)
source('./src/utils.R')

# Specify hyperparameters
hyperpar <- c(trees = 1000, min_n = 5, mtry = 3)

# Specify right-side formula
pf <- '~ x + y + elev + plot_size + year + habitat'

#### A) Run validation tests on EVA only ####
st = Sys.time()
EVA_only <- run_validation_RF(datatype = 'EVA', 
                  response_var_name = 'S',
                  predictor_formula = pf,
                  training_strategy_ReSurv = 'random',
                  format_lnRRchange_preds_method = 'end.vs.start',
                  trees_rf = hyperpar[['trees']], mtry_rf = hyperpar[['mtry']], min_n_rf = hyperpar[['min_n']])
print(Sys.time()-st)

#### B) Run validation tests on ReSurveyEU only ####
st = Sys.time()
ReSurv_only <- run_validation_RF(datatype = 'ReSurv', 
                  response_var_name = 'S',
                  predictor_formula = pf,
                  training_strategy_ReSurv = 'random',
                  format_lnRRchange_preds_method = 'end.vs.start',
                  trees_rf = hyperpar[['trees']], mtry_rf = hyperpar[['mtry']], min_n_rf = hyperpar[['min_n']])
print(Sys.time()-st)

#### C) Run validation tests on EVA+ReSurveyEU ####
st = Sys.time()
EVA_and_ReSurv <- run_validation_RF(datatype = 'EVA_ReSurv', 
                  response_var_name = 'S',
                  predictor_formula = pf,
                  training_strategy_ReSurv = 'random',
                  format_lnRRchange_preds_method = 'end.vs.start',
                  trees_rf = hyperpar[['trees']], mtry_rf = hyperpar[['mtry']], min_n_rf = hyperpar[['min_n']])
print(Sys.time()-st)

# Combine results & export
res <- list(
  EVA_only = EVA_only,
  ReSurv_only = ReSurv_only,
  EVA_and_ReSurv = EVA_and_ReSurv
)

# Export 
write_rds(res, './data/validation/multi_test_validation.rds')

# Quit
quit(save = 'no')