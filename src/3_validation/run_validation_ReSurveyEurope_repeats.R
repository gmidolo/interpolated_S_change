########################################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 07.07.2025
########################################################################################################

# Description: Validation of species richness interpolation from `static` data (no temporal replication)
#              Models trained on static ReSurveyEurope only with repeats (100 times).
#              Each repeat uses a new random selection of training/testing data from time series.
#              Performance (= RMSE, rsq) assessed on:
#              1) `internal_static` = Standard 20% test set
#              2) `external_static` = Independent ReSurveyEurope plots (not used in training/testing).
#              3) `external_change` = Species richness change (log-response ratio) in ReSurveyEurope.

########################################################################################################

# Set seed, optional
set.seed(123) 

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
})

# Source function to format Resurvey data
source('./src/utils')

# Specify hyperparameters
hyperpar <- c(trees = 1000, min_n = 5, mtry = 3)

# Set number of iterations (repeat random selection)
reps <- 100

# Run validation tests 
st = Sys.time()
res = list()
for(i in 1:reps){
res[[i]] <- run_validation_RF(datatype = 'ReSurv', 
                  response_var_name = 'S',
                  predictor_formula = '~ x + y + elev + plot_size + year + habitat',
                  training_strategy_ReSurv = 'random',
                  format_lnRRchange_preds_method = 'end.vs.start',
                  trees_rf = hyperpar[['trees']], mtry_rf = hyperpar[['mtry']], min_n_rf = hyperpar[['min_n']])
# cat(i, ' - ') # monitor progress
}
Sys.time()-st

# Collect evaluation results only (do not store each model object)
final_res <- list(
  eval = res %>% map(\(x){x$eval }) %>% bind_rows(.id = 'rep') 
)

# Inspect statistics of the results
final_res$eval %>%
  select(-rep) %>%
  group_by(.validation) %>%
  summarise_all(mean) # mean
final_res$eval %>%
  select(-rep) %>%
  group_by(.validation) %>%
  summarise_all(sd) # standard dev.

# Export the results 
write_rds(final_res, paste0('./data/validation/ReSu.only.rep_test_validation.rds'))

# Quit
quit(save = 'no')