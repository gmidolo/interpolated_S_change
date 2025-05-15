#--- INTERPOLATE RANDOM FOREST ---#

# Standardize plot size to median by habitat?
standardize_plot.size = TRUE

#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(tidymodels)
    library(sf)
  }
)

# Source function to format Resurvey data
source('./src/validation/functions_valid.R')

# Load EVA+ReSurveyEU data
set.seed(123)
dat <- bind_rows(
    read_csv('./data/input/EVA.csv', show_col_types = F), # Load EVA data
    format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]  # Load ReSurveyEU (static, using one random point in the survey)
  ) %>%
  ## work on a subset?
  # sample_n(100) %>%
  ## select variables for modeling
  select(plot_id, S, x, y, elev, year, plot_size, habitat) 

# Load model
m <- './data/models/RF.last_fit.rds' %>%
  read_rds()

# Extract workflow for predictions
m_wf <- extract_workflow(m)

# Define habitat plot size medians?
hab_plot.size <- dat %>%
  group_by(habitat) %>%
  summarise(plot_size_median = median(plot_size)) %>%
  ungroup()

#### 2. Temporal changes predictions in EVA ####

st = Sys.time()

# Let's predict S for each plot in each of these years
pred_years <- seq(1960, 2020, 1)
# pred_years <- seq(1960, 2020, 20)

# Run predictions
pred_dat <- dat
for(i in pred_years) {
  cat(i, ' - ')
  pdi <- pred_dat 
  pdi <- pdi %>% mutate(year = i) # change year
  if(standardize_plot.size){ # standardize plot size to median by habitat?
  pdi <- pdi %>% 
   select(-plot_size) %>% 
   left_join(hab_plot.size, 'habitat') %>%
   rename(plot_size = plot_size_median)
  }
  pred_dat[paste0('S_pred_',i)] <- predict(object = m_wf, 
                                           new_data = pdi 
                                           ) %>%  pull(.pred)
}

# Calculate changes for focal time periods: 1960, 1980, 2000, 2020
pred_focalchange <- pred_dat %>%
  select(plot_id, contains('_pred_')) %>%
 # Calculate percentages of changes:
  mutate(S_perc.change_1960.1980 = 100*((S_pred_1980 - S_pred_1960)/S_pred_1960)) %>%
  mutate(S_perc.change_1980.2000 = 100*((S_pred_2000 - S_pred_1980)/S_pred_1980)) %>%
  mutate(S_perc.change_2000.2020 = 100*((S_pred_2020 - S_pred_2000)/S_pred_2000)) %>%
  mutate(S_perc.change_1960.2020 = 100*((S_pred_2020 - S_pred_1960)/S_pred_1960)) %>%
 # Calculate log response ratios:      
  mutate(S_lnRR.change_1960.1980 = log(S_pred_1980/S_pred_1960)) %>%
  mutate(S_lnRR.change_1980.2000 = log(S_pred_2000/S_pred_1980)) %>%
  mutate(S_lnRR.change_2000.2020 = log(S_pred_2020/S_pred_2000)) %>%
  mutate(S_lnRR.change_1960.2020 = log(S_pred_2020/S_pred_1960)) %>%
# Calculate absolute change:      
  mutate(S_abs.change_1960.1980 = S_pred_1980-S_pred_1960) %>%
  mutate(S_abs.change_1980.2000 = S_pred_2000-S_pred_1980) %>%
  mutate(S_abs.change_2000.2020 = S_pred_2020-S_pred_2000) %>%
  mutate(S_abs.change_1960.2020 = S_pred_2020-S_pred_1960) %>%
 # round to 3 digits
  mutate_all(round, 3) %>%
  # remove preds
  select(-contains('_pred_'))

# Calculate linear slope estimates for each plot, estimated with predicted S ~ year (period 1960-2020)
pred_lmslope <- pred_dat %>%
  select(plot_id, contains('_pred_')) %>%
  gather('year', 'S_pred', contains('_pred_')) %>%
  mutate(year = as.numeric(gsub("\\D", "", year))) %>%
  group_by(plot_id) %>%
  do(tidy(lm(S_pred ~ year, data = .))) %>%
  filter(term == 'year') %>%
  select(plot_id, estimate, std.error) %>%
  setNames(c('plot_id', 'lm.slope_estimate', 'lm.slope_std.error'))


# Calculate linear slope estimates for each plot, estimated with predicted S ~ year but for each 20 yrs intervals
period = c('1960.1980','1980.2000','2000.2020')
min_p = c(1960, 1980, 2000)
max_p = c(1980, 2000, 2020)
res_lm_periods = list()
for(i in period) {
  cat('PERIOD: ', i, ' - ')
  res_lm_periods[[i]] <- pred_dat %>%
   select(plot_id, contains('_pred_')) %>%
   gather('year', 'S_pred', contains('_pred_')) %>%
   mutate(year = as.numeric(gsub("\\D", "", year))) %>%
   filter(year >= min_p[which(period %in% i)]) %>%
   filter(year <= max_p[which(period %in% i)]) %>%
   group_by(plot_id) %>%
   do(tidy(lm(S_pred ~ year, data = .))) %>%
   filter(term == 'year') %>%
   select(plot_id, estimate, std.error) %>%
   setNames(c('plot_id', paste0('lm.slope_estimate',i), paste0('lm.slope_std.error',i)))
}

# Join predictions made
pred_dat <- pred_dat %>%
            left_join(pred_focalchange, 'plot_id') %>%
            left_join(pred_lmslope, 'plot_id') %>%
            left_join(res_lm_periods$`1960.1980`, 'plot_id') %>%
            left_join(res_lm_periods$`1980.2000`, 'plot_id') %>%
            left_join(res_lm_periods$`2000.2020`, 'plot_id')


## Export predictions
if(standardize_plot.size){
 file_name = './data/preds/preds_stdpltsz.rf.csv'
} else {
 file_name = './data/preds/preds.rf.csv'
}

write_csv(pred_dat, file_name)
print(Sys.time()-st)

quit(save='no')