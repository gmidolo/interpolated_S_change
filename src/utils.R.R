#### Format ResurveyEUdata#####
format_ReSurveyEurope <- \(training_strategy = c('start', 'end', 'random', 'full'),
                           path_resurvey_clean = './data/input/ReSurveyEurope.csv'
                           ){

if (!(training_strategy %in% c("start", "end", "random", "full"))) {
  stop('Wrong training_strategy: it must be either "start", "end", "random", or "full"!')
}

# 1. Load clean resurvey
resurvey_clean <- read_csv(path_resurvey_clean, show_col_types = F)

# 2. Prepare data for Train and external CV
if (training_strategy == 'random') {
  traintest_data <- resurvey_clean %>%
       arrange(resurv_id, year) %>%
       group_by(resurv_id) %>%
       slice_sample(n=1) %>%
       ungroup() 
  external_data <- resurvey_clean %>% anti_join(traintest_data,'plot_id')
return(list(traintest_data = traintest_data, external_data = external_data))
 }

if (training_strategy == 'start') {
  traintest_data <- resurvey_clean %>%
    group_by(resurv_id) %>%
    arrange(year) %>%
    slice_head(n = 1) %>%
    ungroup()
  external_data <- resurvey_clean %>%
    anti_join(traintest_data, by = 'plot_id')
return(list(traintest_data = traintest_data, external_data = external_data))
 }

if (training_strategy == 'end') {
  traintest_data <- resurvey_clean %>%
    group_by(resurv_id) %>%
    arrange(year) %>%
    slice_tail(n = 1) %>%
    ungroup()
  external_data <- resurvey_clean %>%
    anti_join(traintest_data, by = 'plot_id')
return(list(traintest_data = traintest_data, external_data = external_data))
 }

if (training_strategy == 'full') {
return(list(traintest_data = resurvey_clean))
 }
}

#### Format 'change' prediction ####

format_lnRRchange_preds <- \(data, method = c('end.vs.start', 'random.within'), observed_value_name = 'S', predicted_value_name = '.pred'){

if (!(method %in% c('end.vs.start', 'random.within'))) {
  stop('Wrong method: it must be either "end.vs.start" or "random.within"!')
}

# rename names of observed vs. predicted
names(data)[which(names(data) %in% observed_value_name)] <- '.observed' 
names(data)[which(names(data) %in% predicted_value_name)] <- '.pred' 

# Compare last plot vs the first: method = 'end.vs.start'
if(method == 'end.vs.start'){
  obs_vs_pred <- data %>%
       # we focus only on min and max point in the time series
       group_by(resurv_id) %>%
       filter(year == min(year) | year ==max(year)) %>%
       # if multiple censuses are available at the start or endpoint, we randomly choose one
       arrange(resurv_id, year) %>%
       group_by(resurv_id, year) %>%
       sample_n(1) %>%
       ungroup() 
       }

# Compare two random points in time (but still cronologically ordered): method = 'random.within'
if(method == 'random.within'){
  obs_vs_pred <- data %>%
       # if multiple censuses are available at the start or endpoint, we randomly choose one
       group_by(resurv_id, year) %>%
       slice_sample(n = 1) %>%
       ungroup() %>%
       # now sample two plots per resurvey, independently from their positioning along the time series (not necessarely min vs. max)
       group_by(resurv_id) %>%
       slice_sample(n = 2) %>%
       # arrange year in ascending order
       ungroup() %>%
       arrange(resurv_id, year) 
}

# Define point in time (either `1` or `2`) within each resurv_id
obs_vs_pred <- obs_vs_pred %>%
       # now define year id (either `1`` or `2``) within each resurv_id
       group_by(resurv_id) %>%
       mutate(resurvyear_id = paste0('time_', row_number()), .after = resurv_id) %>%
       ungroup() %>%
                  
       # gather data based on obvserved and predicted
       gather('S_type','S_value', c(.observed, .pred)) %>%
       arrange(resurv_id, resurvyear_id)


# Separate observed vs predicted, and calculate change
obs <- obs_vs_pred %>%
           select(ReSur_type, resurv_id, resurvyear_id, S_type, S_value) %>%
           filter(S_type == '.observed') %>% select(-S_type) %>%
           spread(resurvyear_id, S_value) %>%
           mutate(obs_change = log(time_2/time_1)) %>%
           select(-contains('time')) 
prd <- obs_vs_pred %>%
           select(ReSur_type, resurv_id, resurvyear_id, S_type, S_value) %>%
           filter(S_type == '.pred') %>% select(-S_type) %>%
           spread(resurvyear_id, S_value) %>%
           mutate(prd_change = log(time_2/time_1)) %>%
           select(-contains('time')) 

# Finalize the dataset
obs_vs_pred <- left_join(obs, prd, by = join_by(ReSur_type, resurv_id))

return(obs_vs_pred)
  

}
                        
#### Run validation ####
run_validation_RF <- \(datatype = c('EVA','ReSurv','EVA_ReSurv'), 
                       response_var_name = 'S',
                       predictor_formula = '~ x + y + elev + plot_size + year + habitat',
                       training_strategy_ReSurv = 'random',
                       format_lnRRchange_preds_method = 'end.vs.start',
                       trees_rf = 1000, mtry_rf = 3, min_n_rf = 5){

if (!(datatype %in% c('EVA','ReSurv','EVA_ReSurv'))) {
  stop('Wrong method: it must be either "EVA", "ReSurv", or "EVA_ReSurv"!')
}

# Load data
dat_ReSurveyEurope <- format_ReSurveyEurope(training_strategy = training_strategy_ReSurv)
dat <- dat_ReSurveyEurope 

# Use EVA data, if training is on EVA data only
if(datatype == 'EVA') {
  dat[['traintest_data']] <- read_csv('./data/input/EVA.csv', show_col_types = F) 
}

# Add EVA data to ReSurveyEU, if training is on EVA+ReSurveyEU data
if(datatype == 'EVA_ReSurv') {
  dat[['traintest_data']] <- bind_rows(dat[['traintest_data']], read_csv('./data/input/EVA.csv', show_col_types = F))
  }

# Set name of the response variable (in case multiple responses want to be assessed; here 'S'=species richness is the default)
# for dat_ReSurveyEurope (ReSurveyEU-only training; 'ReSurv')
names(dat_ReSurveyEurope[['traintest_data']])[which(names(dat_ReSurveyEurope[['traintest_data']]) %in% response_var_name)] <- 'response_var'
names(dat_ReSurveyEurope[['external_data']]) [which(names(dat_ReSurveyEurope[['external_data']])  %in% response_var_name)] <- 'response_var'
# for other types of trainings ('EVA' & 'EVA_ReSurv')
names(dat[['traintest_data']])[which(names(dat[['traintest_data']]) %in% response_var_name)] <- 'response_var'
names(dat[['external_data']]) [which(names(dat[['external_data']])  %in% response_var_name)] <- 'response_var'

# Split the data (prepare for modeling in RF)
dat_split <- initial_split(dat[['traintest_data']], # Initial split
                           prop = 4/5,
                           strata = response_var) 
dat_train <- training(dat_split) # train data

### 1 Model fit and internal evaluation (train vs test) ###
# Define model
spec <- rand_forest(
  trees = trees_rf, 
  min_n = min_n_rf,
  mtry = mtry_rf
) %>%
  set_mode('regression') %>%
  set_engine('ranger', importance = 'impurity', seed = 1975) 

# Define recipe
f <- formula(paste0('response_var', predictor_formula)) # get formula
rec <- recipe(formula=f, data = dat_train)
if(str_detect(predictor_formula, 'habitat')){ # Transform the habitat to a factor variable, if this is present in the predictors
  rec <- rec %>%
   step_string2factor(habitat, levels = c('Forest', 'Grassland', 'Scrub', 'Wetland'))
}

# Define workflow
wflow <- workflow() %>%
  add_model(spec) %>%
  add_recipe(rec) 
wflow

# Last fit
lfit <- wflow %>%
  last_fit(dat_split)

# Extract workflow of the last fit
last_fit_wflow <- extract_workflow(lfit)

# Collect metrics
last_fit_metrics <- collect_metrics(lfit)

# Save predictions
last_fit_preds <- collect_predictions(lfit)


### 2 External evaluation - STATIC (plots in ReSurvey EU - not used to train nor test data) ###

# Predict model over the external dataset
external_fit_preds_static <- augment(last_fit_wflow, dat_ReSurveyEurope[['external_data']])

# Define set of metrics to evaluate the model
eval_metrics <- metric_set(rmse, rsq) 

# Collect predictions
external_fit_metrics_static <- external_fit_preds_static %>% 
  eval_metrics(response_var, .pred)

### 3 External evaluation - CHANGE ###

# Predict model over the entire dataset
fulldb_fit_preds_static <- augment(last_fit_wflow, bind_rows(dat_ReSurveyEurope))

# Collect predictions on change
fulldb_preds_change <- format_lnRRchange_preds(
  data = fulldb_fit_preds_static, 
  method = format_lnRRchange_preds_method,
  observed_value_name = 'response_var',
  predicted_value_name = '.pred'
) %>%
filter(is.finite(obs_change)) 

# Collect predictions
fulldb_metrics_change <- fulldb_preds_change %>% 
  eval_metrics(obs_change, prd_change) 

### 4 Export the results ###
res <- list(
  preds = list(internal_static = last_fit_preds %>% select(.pred, response_var) %>% setNames(c('.pred','.obs')), 
               external_static = external_fit_preds_static %>% select(.pred, response_var) %>% setNames(c('.pred','.obs')), 
               external_change = fulldb_preds_change %>% select(prd_change, obs_change, ReSur_type) %>% setNames(c('.pred','.obs','ReSur_type'))
               ) %>% bind_rows(.id = '.validation'),

  eval = list(internal_static = last_fit_metrics, 
              external_static = external_fit_metrics_static, 
              external_change = fulldb_metrics_change) %>%
              bind_rows(.id = '.validation') %>%
              select(.validation, .metric, .estimate) %>%
              pivot_wider(names_from = .metric, values_from = .estimate) %>%
              mutate(cor = sqrt(rsq))

  #, lastfit = lfit # save model results? Currently suprressed as fitted models takes quite some space
)

return(res)

}
  



