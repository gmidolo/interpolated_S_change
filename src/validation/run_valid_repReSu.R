#--- VALIDATION VIA RESURVEY DATA only (with repeats) using RANDOM FOREST ---#

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

# Set number of iterations
reps = 99

st = Sys.time()
res = list()
for(i in 1:reps){
res[[i]] <- run_validation_RF(datatype = 'ReSurv', 
                  response_var_name = 'S',
                  predictor_formula = '~ x + y + elev + plot_size + year + habitat',
                  training_strategy_ReSurv = 'random',
                  format_lnRRchange_preds_method = 'end.vs.start',
                  trees_rf = hyperpar[['trees']], mtry_rf = hyperpar[['mtry']], min_n_rf = hyperpar[['min_n']])
#cat(i,' - ')
}
Sys.time()-st

final_res <- list(
  # preds = res %>% map(\(x){x$preds}) %>% bind_rows(.id = 'rep'), 
  eval  = res %>% map(\(x){x$eval }) %>% bind_rows(.id = 'rep') # export evaluation results only
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
write_rds(final_res, './data/validation/ReSu.only.rep_test_validation.rds')


quit(save = 'no')