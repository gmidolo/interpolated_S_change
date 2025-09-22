################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 05.07.2025
################################################################################

# Description: Summarise interpolation stats using raw (all trees) predictions 
#              across main habitat types and biogeographic region (Figure 3)

################################################################################

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(tidymodels)
    library(sf)
  }
)

# Source function to format ReSurveyEU
source('./src/utils.R')

# Set up 
standardize_plot.size <- TRUE
pred_years <- seq(1960, 2020, 1)
output_file <- './data/preds/pdp/habitat.biogeo_pdp_trends_alltrees.csv'

# Load biogeographic regions
biogeoregions <- read_rds('./data/spatial/biogeoregions.rds') 

# Load EVA+ReSurveyEU data
set.seed(123)
dat <- bind_rows(
    read_csv('./data/input/EVA.csv.gz', show_col_types = F), # Load EVA data
    format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]  # Load ReSurveyEU (static, using one random point in the survey)
  ) %>%
  ## select variables for modeling
  select(plot_id, S, x, y, elev, year, plot_size, habitat) 

# Define habitat plot size medians
hab_plot.size <- dat %>%
  group_by(habitat) %>%
  summarise(plot_size_median = median(plot_size)) %>%
  ungroup()

# Select period of interest
dat <- dat %>%
    filter(year >= 1960 & year <= 2020)

# Intersect biogeographical regions with data
int <- as.list(st_intersects(biogeoregions, dat %>% st_as_sf(coords=c('x','y'), crs=25832)))
dati=list()
for(i in 1:length(int)){
  dati[[i]] = dat[int[[i]],]
}
names(dati) <- biogeoregions$biogeo
dat <- dati %>% bind_rows(.id='biogeo')

# Get no. plots per habitat
hab_plot.count <- dat %>%
    group_by(habitat, biogeo) %>%
    summarise(no.plots = n(), .groups = 'drop') 

# Load model and extract workflow
model_path <- './data/models/RF.last_fit.rds'
m <- read_rds(model_path)
m_wf <- extract_workflow(m)

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
  pred_res[[as.character(i)]] <- pdi %>% 
    left_join(hab_plot.count, by = join_by(biogeo, habitat)) %>%
    cbind(praw) %>%
    group_by(habitat, biogeo, year, no.plots) %>%
    summarise(across(starts_with('prediction.'), mean), .groups = 'drop') %>%
    gather('.pred_key', '.pred', starts_with('prediction.')) %>%
    group_by(habitat, biogeo, year, no.plots) %>%
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