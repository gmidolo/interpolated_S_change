################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 07.07.2025
################################################################################

# Description: Analyze error (RMSE) response to spatial and temporal distances

################################################################################

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(sf)
  library(terra)
})

# Source function to format Resurvey data
source('./src/utils.R')

#### 1. Spatial distance ####

# Define decades 
decades <- tibble(
  decade_name = c('1961-1970', '1971-1980', '1981-1990', '1991-2000', '2001-2010', '2011-2020'),
  min_year = c(1961, 1971, 1981, 1991, 2001, 2011),
  max_year = c(1970, 1980, 1990, 2000, 2010, 2020)
)

# Collect results (distances between test and train data across decades)
manual_breaks <- c(-Inf, 0, 0.5, 0.75, 1, 2.5, 5, Inf)
manual_labels <- c('-Inf','0-0.5', '0.5-0.75', '0.75-1', '1-2.5', '2.5-5', '>5')
d_spat <- list()
for(i in decades$decade_name) {
  d_spat[[i]] <- paste0('./data/spatial/training_data_distance_csv/spatial_distance_',i,'.csv.gz') %>%
    read_csv(show_col_types = F) %>%
    mutate(space_dist_km = space_dist/1000) %>%
    mutate(
    space_dist_km_cat = cut(space_dist_km,
      breaks = manual_breaks,
      labels = manual_labels,
      right = F,
      include.lowest = TRUE)
    ) %>%
    group_by(decade_name, space_dist_km_cat) %>%
    summarise(
      RMSE = rmse_vec(truth = S, estimate = .pred),
      no.obs = n(),
      no.obs.pretty = scales::comma(no.obs),
      .groups = 'drop'
    )
}
d_spat <- bind_rows(d_spat)

no_test_data <- d_spat$no.obs %>% sum() # number of plots available in the testing data

# Plot
spat_cat_plot <- ggplot(d_spat, aes(x = space_dist_km_cat, y = RMSE)) +
  geom_col(aes(fill = no.obs)) +
  geom_text(aes(label = no.obs.pretty),
            vjust = -0.5,
            size = 3) +
  labs(
    subtitle = paste0('Testing data (EVA & ReSurveyEurope) (total no. plots = ',scales::comma(no_test_data),')'), 
    title = 'Spatial distance effects on model residuals',
    x = 'Distance from training data [km]',
    y = expression('RMSE: observed vs. predicted ' * italic(S)),
    fill = 'No. plots'
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.text = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~decade_name, nrow=1) 

# Export to the `fig/diagnostic` folder
ggsave('./fig/diagnostic/distance_effects_spatial.jpg', spat_cat_plot, width=13.5, height = 6, dpi=600)


#### 2. Temporal distance (in ReSurveyEurope) ####

# We will first fit the model and apply workflow as similarly done for model validation:
set.seed(123)
hyperpar <- c(trees = 1000, min_n = 5, mtry = 3)
pf <- '~ x + y + elev + plot_size + year + habitat'
datatype = 'ReSurv'
response_var_name = 'S'
predictor_formula = pf
training_strategy_ReSurv = 'start_or_end' # train the model with either start or end point, not points in between
format_lnRRchange_preds_method = 'end.vs.start'
trees_rf = hyperpar[['trees']]
mtry_rf = hyperpar[['mtry']]
min_n_rf = hyperpar[['min_n']]

# Load data
dat_ReSurveyEurope <- format_ReSurveyEurope(training_strategy = training_strategy_ReSurv)
dat <- dat_ReSurveyEurope 

if(datatype == 'EVA') {
  dat[['traintest_data']] <- read_csv('./data/input/EVA.csv', show_col_types = F) #%>% sample_n(25000)
}

if(datatype == 'EVA_ReSurv') {
  dat[['traintest_data']] <- bind_rows(dat[['traintest_data']], read_csv('./data/input/EVA.csv', show_col_types = F))
  }

# Set name of the response variable?
names(dat_ReSurveyEurope[['traintest_data']])[which(names(dat_ReSurveyEurope[['traintest_data']]) %in% response_var_name)] <- 'response_var'
names(dat_ReSurveyEurope[['external_data']]) [which(names(dat_ReSurveyEurope[['external_data']])  %in% response_var_name)] <- 'response_var'
names(dat[['traintest_data']])[which(names(dat[['traintest_data']]) %in% response_var_name)] <- 'response_var'
names(dat[['external_data']]) [which(names(dat[['external_data']])  %in% response_var_name)] <- 'response_var'

# Define model
dat_split <- initial_split(dat[['traintest_data']], # Initial split
                           prop = 4/5,
                           strata = response_var) 
dat_train <- training(dat_split) # train data
spec <- rand_forest(
  trees = trees_rf, 
  min_n = min_n_rf,
  mtry = mtry_rf
) %>%
  set_mode('regression') %>%
  set_engine('ranger', importance = 'impurity', seed = 1975) 
rec <- recipe(formula(paste0('response_var', predictor_formula)), data = dat_train)

# Transform the habitat to a factor variable, if this is present in the predictors
if(str_detect(predictor_formula, 'habitat')){
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

# Predict model over the entire dataset
fulldb_fit_preds_static <- augment(last_fit_wflow, bind_rows(dat_ReSurveyEurope))

# Collect predictions on change
method = format_lnRRchange_preds_method
observed_value_name = 'response_var'
predicted_value_name = '.pred'

# rename names of observed vs. predicted
names(fulldb_fit_preds_static)[which(names(fulldb_fit_preds_static) %in% observed_value_name)] <- '.observed' 
names(fulldb_fit_preds_static)[which(names(fulldb_fit_preds_static) %in% predicted_value_name)] <- '.pred' 

# Compare last plot vs the first: method = 'end.vs.start'
if(method == 'end.vs.start') {
  obs_vs_pred <- fulldb_fit_preds_static %>%
    # we focus only on min and max point in the time series
    group_by(resurv_id) %>%
    filter(year == min(year) | year == max(year)) %>%
    # if multiple years are available in a given midpoint, take just one
    arrange(resurv_id, year) %>%
    group_by(resurv_id, year) %>%
    sample_n(1) %>%
    ungroup()
}

# Get ranges of years per resurvey
obs_vs_pred_yrs <- obs_vs_pred %>%
  select(resurv_id) %>%
  unique() %>%
  left_join(
    obs_vs_pred %>% 
    select(resurv_id, year) %>% 
    group_by(resurv_id) %>%
    filter(year==min(year)) %>%
    rename(min_yr = year)
  ) %>%
    left_join(
    obs_vs_pred %>% 
    select(resurv_id, year) %>% 
    group_by(resurv_id) %>%
    filter(year==max(year)) %>%
    rename(max_yr = year)
  ) %>%
  mutate(range = max_yr - min_yr)

# Define point in time (either `1` or `2`) within each resurv_id
obs_vs_pred <- obs_vs_pred %>%
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
obs_vs_pred <- left_join(obs, prd, by = join_by(ReSur_type, resurv_id)) %>%
  # calc root squared error
  mutate(
    rsqe = sqrt((obs_change - prd_change)^2)
  ) %>%
  # add range years
  left_join(obs_vs_pred_yrs, by='resurv_id')

# this is the rsq of the model (pred. vs. obs. delta S change)
cor(obs_vs_pred$obs_change,obs_vs_pred$prd_change)^2

# Plotting: separate predictions into bins of temporal ranges
manual_breaks <- c(
  1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf
)

manual_labels <- c(
  '1-5', '6-10', '11-15', '16-20', '21-25', '26-30',
  '31-35', '36-40', '41-45', '46-50', '51-55', '56-60',
  '61-65', '66-72'
)

eval_metrics_cat <- obs_vs_pred %>%
  mutate(
    range_cat = cut(range,
      breaks = manual_breaks,
      labels = manual_labels,
      right = TRUE,
      include.lowest = TRUE
    )
  ) %>%
  group_by(range_cat) %>%
  summarise(
    RMSE = rmse_vec(truth = obs_change, estimate = prd_change),
    no.obs = n(),
    no.obs.pretty = scales::comma(no.obs)
  ) 

temp_cat_plot <- ggplot(eval_metrics_cat, aes(x = range_cat, y = RMSE)) +
  geom_col(aes(fill = no.obs)) +
  geom_text(aes(label = no.obs.pretty),
            vjust = -0.5,
            size = 3) +
  labs(
    subtitle = paste0('* species richness changes in ReSurveyEurope (no. plots = ', scales::comma(nrow(obs_vs_pred)),')'),
    title = 'Temporal distance effects on model residuals*',
    x = 'Distance from training data [years]',
    y = expression('RMSE: observed vs. predicted lnRR(' * italic(Delta) *' '* italic(S) * ')'),
    fill = 'No. plots'
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
temp_cat_plot

ggsave('./fig/diagnostic/distance_effects_temporal_cat.jpg', temp_cat_plot, width=7, height = 6, dpi=600)