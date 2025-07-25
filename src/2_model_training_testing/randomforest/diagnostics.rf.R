################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 04.07.2025
################################################################################

# Description: Tuning results, feature importance, model evaluation,
#              distribution of the residuals, partial dependence plots, H-stats

################################################################################

#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(tidymodels)
    library(vip)
    library(hstats)
    library(sf)
    library(terra)
  }
)

# Source function to format Resurvey data
source('./src/utils.R')

# Prepare data for modeling and split train and test dataset
set.seed(123)
dat_split <- 
  bind_rows(
    read_csv('./data/input/EVA.csv.gz', show_col_types = F), # Load EVA data
    format_ReSurveyEurope(training_strategy = 'random')[['traintest_data']]  # Load ReSurveyEU (static, using one random point in the survey)
  ) %>%
  ## select variables for modeling
  select(plot_id, S, x, y, elev, year, plot_size, habitat) %>%
  ## split the data
  initial_split(prop = 4/5, strata = S) # We will use 80% of the data for training, 20% for testing

# Subset training set
dat_train <- training(dat_split) 

# Load model fit
m <- list.files('./data/models', pattern = 'RF.last_fit', full.names = T) %>%
  read_rds()

# Load CV results
cv_res <- list.files('./data/models', pattern = 'RF.cv_res', full.names = T) %>%
  read_rds()

# Load tuning results
tune_res <- list.files('./data/models', pattern = 'RF.tune_res', full.names = T) %>%
  read_rds()

#### 2. Model evaluation ####

# Plot tuning results
tune_res_plot <- autoplot(tune_res) +
 theme_bw() +
 ggtitle('Tuning results - Random forest (`ranger`) (trees=1000)')
ggsave('./fig/diagnostic/RF.tune_res_plot.jpg', tune_res_plot, width = 6, height = 5, dpi=600)

# Show performance metrics obtained on random CV
collect_metrics(cv_res)

# Show performance metrics evaluated on testing data (RMSE and R-squared)
collect_metrics(m)

# Display features importance
var_imp <- extract_workflow(m) %>%
  extract_fit_parsnip() %>%
  vip(geom = 'col') +
  theme_minimal() + 
  ggtitle('Variable importance - Random forest')+
  labs(y = 'Importance (node impurity)')
ggsave('./fig/diagnostic/RF.var_imp.jpg', var_imp, width = 5, height = 4.5, dpi=600)

# Collect predictions
pred_test <- augment(extract_workflow(m), testing(dat_split))

# Model residuals
pred_test$resid <- (pred_test$S-pred_test$.pred)

# Plot obs. vs. predicted
breaks_axes <- seq(0,140,20) # define breaks on the x and y axes
pred_test_plot <- pred_test %>% # plot
  ggplot(aes(S, .pred)) +
  geom_hex(bins=45) +
  scale_fill_gradient(low = 'lightblue', high = 'midnightblue', trans='log10') +
  coord_fixed() + 
  scale_x_continuous(breaks=breaks_axes) + scale_y_continuous(breaks=breaks_axes) +
  labs(x='Observed S\n(Testing data)', 
       y='Predicted S', 
       fill='N. Plots', 
       title='Observed vs. predicted richness (S) in Random forest', 
       subtitle=paste0('Pearson correlation: ', round(cor(pred_test$.pred,pred_test$S), 2))) +
  geom_abline(lty = 2, color = 'red', lwd=.8, alpha=.8) +
  theme_bw() + theme(legend.position = 'bottom') 
pred_test_plot
ggsave('./fig/diagnostic/RF.pred_test_plot.jpg', pred_test_plot, width = 7, height = 5, dpi=600)


# 3. Geographic maps of model residuals ####

# load EU map
EU <- read_rds('./data/spatial/EU_shape_map.rds')

# get raster
r <- rast(res = 25*1000, extent=ext(EU), crs=crs(EU)) 
r <- rasterize(pred_test %>% select(x,y) %>% as.matrix(), r, values=pred_test$resid, fun=mean)

#plot
map_residuals <- ggplot() +
 geom_raster(data=as.data.frame(r, xy=T), aes(x,y,fill=mean))+
 scale_fill_gradient2(low='brown', high='midnightblue', mid='lightyellow') +
 geom_sf(data=EU, fill=NA, color=alpha('black',0.5)) +
 theme(axis.title=element_blank())+
 labs(fill='Mean S residuals')+
 ggtitle('Distribution of RandomForest model residuals (25 km resolution)')
map_residuals
ggsave('./fig/diagnostic/RF.map_distribution_of_residuals.jpg', map_residuals, width = 6.5, height = 6, dpi=600)

#### 4. Partial dependence plots ####

# Extract model workflow
mwf <- extract_workflow(m)

# Extract profiles for each variable
pdp_single_dat <- list()
for(i in c('x', 'y', 'elev', 'plot_size', 'year')){
  cat(i, ' |> ')
  pdp_single_dat[[i]] <- partial_dep(mwf, v = i, X = dat_train, BY = 'habitat')$data
}
pdp_single_dat <- map(pdp_single_dat,
                      function(x){x %>% setNames(c('Habitat', 'x', 'y'))}
                      ) %>%
                      bind_rows(.id = 'Predictor')

pdp_single_dat <- pdp_single_dat %>%
  mutate(Predictor_pretty = recode(Predictor,
    'elev' = 'Elevation (m)',
    'x' = 'x - Easting (m)',
    'y' = 'y - Northing (m)',
    'plot_size' = 'Plot size (meter squared)',
    'year' = 'Time (year)',    
  ))

p <- ggplot(pdp_single_dat, aes(x, y, col = Habitat)) + 
     geom_line(size=1.25, alpha=0.8) +
     # geom_smooth(size=0.75, se=F, alpha=0.8) +
     facet_wrap(~Predictor_pretty, scales = 'free_x') + 
     labs(y='Species richness') +
     # scale_x_continuous(breaks = scales::pretty_breaks(n=4)) +
     theme_bw() +
     theme(axis.title.x = element_blank())
p
ggsave('./fig/diagnostic/RF.pdp.jpg', p, width = 7.5, height = 4.5, dpi=600)

# Calculate H stats (interactions of features)
set.seed(234)
system.time(
  hs <- hstats(mwf, 
               X = dat_train[c('x', 'y', 'elev', 'plot_size', 'year','habitat')]
               # ,n_max = round((dat_train %>% nrow)*0.01)) # Use 1% of the data
               )
)
summary(hs)

hs_p <- plot(hs, fill='#3d3c3c', top_m=7) + 
 theme_bw()
hs_p
ggsave('./fig/diagnostic/RF.Hstats.jpg', hs_p, width = 8, height = 4.5, dpi=600)

# 2D pdp
grd <- t(combn(c('x', 'y', 'elev', 'plot_size', 'year'
                 ) , 2 )) 

pd2d_list <- list()
for(i in 1:nrow(grd)) {
  plot_name <- paste0(grd[i,1], '_x_', grd[i,2])
  cat(plot_name, ' |> ')
  pd2d_list[[plot_name]] <- partial_dep(
    mwf, 
    v = c(grd[i,1],grd[i,2]),  
    X = dat_train,
    grid_size = 10000
    )  
}

pd2d_dat <- pd2d_list %>%
            # map(function(x){x$data %>% pivot_longer(1:2)}) %>%
            map(function(x){x$data %>% setNames(c('x','y','z'))}) %>%
            bind_rows(.id = 'interact') %>%
            separate(interact, into=c('x_name','y_name'), sep='_x_') %>%
            mutate(x_name = factor(x_name, unique(grd[,1]))) %>%
            mutate(y_name = factor(y_name, unique(grd[,2])))

pd2d_plt <- ggplot(pd2d_dat, aes(x,y,fill=z)) +
            geom_raster() +
            facet_grid(y_name~x_name, scales='free', switch = "y") +
            scale_fill_viridis_c() +
            theme_bw() +
            theme(axis.title = element_blank()) +
            labs(fill='S')
pd2d_plt

ggsave('./fig/diagnostic/RF.pdp2d.jpg', pd2d_plt, width = 8, height = 6.75, dpi=600)

# quit
quit(save='no')