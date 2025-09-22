################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 05.07.2025
################################################################################

# Description: Summarise interpolation stats using raw (all trees) predictions 
#              across main habitat types

################################################################################

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
})

# Source function to format ReSurveyEU
source('./src/utils.R')

# Set up 
standardize_plot.size <- TRUE
pred_years <- seq(1960, 2020, 1)
output_file <- './data/preds/pdp/habitat_pdp_trends_alltrees.csv'

# Load model and extract workflow
model_path <- './data/models/RF.last_fit.rds'
m <- read_rds(model_path)
m_wf <- extract_workflow(m)

# Load data
set.seed(123)
dat <- bind_rows(
  read_csv('./data/input/EVA.csv.gz', show_col_types = FALSE),
  format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]
) %>%
  select(plot_id, S, x, y, elev, year, plot_size, habitat)

# Compute median plot size per habitat
hab_plot.size <- dat %>%
  group_by(habitat) %>%
  summarise(plot_size_median = median(plot_size), .groups = 'drop')

# Interpolate over study period
dat <- dat %>%
  filter(year >= 1960 & year <= 2020)

# Prediction loop
pred_res <- list()
st <- Sys.time()
for (i in pred_years) {
 
 message(paste0('Year: ', i))
 
  pdi <- dat %>% mutate(year = i) # change year
  if(standardize_plot.size){ # standardize plot size to median by habitat?
   pdi <- pdi %>% 
    select(-plot_size) %>% 
    left_join(hab_plot.size, 'habitat') %>%
    rename(plot_size = plot_size_median)
 }  
  # get raw ranger predictions
  praw <- predict(m_wf, new_data = pdi, type = 'raw', opts = list(predict.all = TRUE)) 
  praw <- as_tibble(praw) 

  # get stats
  pred_res[[as.character(i)]] <- cbind(pdi, praw) %>%
    group_by(habitat, year) %>%
    summarise(across(starts_with('prediction.'), mean), .groups = 'drop') %>%
    gather('.pred_key', '.pred', starts_with('prediction.')) %>%
    group_by(habitat, year) %>%
    summarise(
      median = median(.pred),
      quantile0.05 = quantile(.pred, .05),
      quantile0.95 = quantile(.pred, .95),
      n=n(),
      sd = sd(.pred),
      mean = mean(.pred),
      lower_ci_95 = mean - (1.96 * (sd/sqrt(n))),
      upper_ci_95 = mean + (1.96 * (sd/sqrt(n))),
     .groups = 'drop'
  )

}
print(Sys.time() - st)

# Combine predictions across years
pred_res_df <- bind_rows(pred_res, .id='year')

# Save result
write_csv(pred_res_df, output_file)

# Quit
quit(save = 'no')