################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 01.09.2025
################################################################################

# Description: Spatio-Temporal block cross-validation for Random Forests

################################################################################

#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(tidymodels)
    library(doParallel)
    library(GGally)
  }
)

# Source function to format Resurvey data
source('./src/utils.R')

# Prepare data for modeling and split train and test dataset
set.seed(123)
dat <- 
  bind_rows(
    read_csv('./data/input/EVA.csv.gz', show_col_types = F), # Load EVA data
    format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]  # Load ReSurveyEU (static, using one random point in the survey)
  ) %>%
  ## select variables for modeling
  select(plot_id, S, x, y, elev, year, plot_size, habitat) 


#### 2. Spatio-Temporal CV ####

# Load results of the tuning procedure
tune_res <- read_rds('./data/models/RF.tune_res.rds') 

# Select best set of hyperparameters (based on RMSE)
tune_best <- select_best(tune_res, metric = 'rmse')

# Define recipe
rec <- recipe(S ~ x + y + elev + plot_size + year + habitat, data = dat) %>%
  step_string2factor(habitat, levels = c('Forest', 'Grassland', 'Scrub', 'Wetland'))

# Define model based on tuning results
spec <- rand_forest(
  trees = 1000, 
  min_n = tune(),
  mtry = tune()
) %>%
  set_mode('regression') %>%
  set_engine('ranger', importance = 'impurity', seed = 1975) %>%
  finalize_model(tune_best)

# Define workflow
wflow <- workflow() %>%
  add_model(spec) %>%
  add_recipe(rec) %>%
  finalize_workflow(tune_best)
wflow

# Define spatio-temporal blocks using kmeans clustering
dat_folds <- dat
k <- 100 # number of blocks (=no. centers for kmean clustering)
kmc <- kmeans(scale(dat_folds[c('year','x','y')]), centers = k, iter.max = 100)
dat_folds$fold_id <- kmc$cluster

# Range, head and tails of the count of plots for each fold
dat_folds$fold_id %>% 
  table() %>% 
  hist(main = 'No. of observations acorss spatiotemporal blocks')

# Reshuffle fold ids and turn it into factor (optional) 
dat_folds$fold_id <- factor(dat_folds$fold_id, levels = sample(unique(dat_folds$fold_id)))

# Vizualize blocks
d2p <- dat_folds %>%
  sample_n(99999) %>% #display limited amount of plots
  rename(Northing=y, Easting=x, `Year of sampling`=year)
pm <- GGally::ggpairs(d2p, 
                      columns = c('Easting','Northing','Year of sampling'), 
                      ggplot2::aes(colour = fold_id, alpha=.6),
                      upper = "blank")

# Export figure
ggsave(filename = './fig/diagnostic/spatiotemporal_blocking_kmeans_100centers.jpg', 
       plot = pm + ggtitle('K-means clustering of the training data based upon their distribution\nin the space and time (no. centers = 100)'), 
       width = 7, 
       height = 6.75)

# Get CV folds
nfolds <- 10
cv_spatiotemporal_folds <- group_vfold_cv(dat_folds,
                                          group = fold_id, 
                                          v = nfolds) 

# Vizualize folds
fold_no = 1  # e.g. plot fold no. 1
d2p <- bind_rows(
  analysis(cv_spatiotemporal_folds$splits[[fold_no]]) %>% mutate(fold_type='Train'),
  assessment(cv_spatiotemporal_folds$splits[[fold_no]]) %>% mutate(fold_type='Test')
) %>%
  sample_n(10000) %>%
  rename(Northing=y, Easting=x, `Year of sampling`=year)

pm <- GGally::ggpairs(
  d2p,
  columns = c('Easting', 'Northing', 'Year of sampling'),
 aes(color = fold_type, alpha = .6),
  # only map color
  upper = 'blank',
  legend = 7
) +
  labs(color = 'Dataset') +
  theme(legend.title = element_text(size = 10)) +
  scale_alpha(guide = "none") 

# Export figure
ggsave(filename = './fig/diagnostic/spatiotemporal_blocking_kmeans_100centers_examplefold.jpg', 
       plot = pm + ggtitle('Spatio-Temporal block CV; example for a single fold'), 
       width = 7, 
       height = 6.75)

# Perform CV
st = Sys.time()
cl <- makePSOCKcluster(10)
registerDoParallel(cl)
cv_res <- wflow %>%
  fit_resamples(
    preprocessor = rec,
    resamples = cv_spatiotemporal_folds,
    control = control_resamples(verbose = T),
    metrics = metric_set(rmse, rsq)
  )
print(Sys.time()-st)

# Export CV raw results
cv_res %>%
  write_rds('./data/models/RF.cv.spatiotemporal_res.rds')

# Export CV metrics
cv_res_metrics <- cv_res %>%
  collect_metrics(summarize = F) 
print(head(cv_res_metrics))

cv_res_metrics %>%  
  write_csv('./data/models/RF.cv.spatiotemporal_metrics.csv')

# stop paralleling
stopCluster(cl)
stopImplicitCluster()

# quit
quit(save='no')