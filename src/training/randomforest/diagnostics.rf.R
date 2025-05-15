#--- DIAGNOSTICS RANDOM FOREST ---#


#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(tidymodels)
    library(doParallel)
    library(vip)
    library(hstats)
    library(sf)
    library(terra)
  }
)

# Source function to format Resurvey data
source('./src/validation/functions_valid.R')

# Prepare data for modeling and split train and test dataset
set.seed(123)
dat_split <- 
  bind_rows(
    read_csv('./data/input/EVA.csv', show_col_types = F), # Load EVA data
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

# Display variable importance
var_imp <- extract_workflow(m) %>%
  extract_fit_parsnip() %>%
  vip(geom = 'col') +
  theme_minimal() + 
  ggtitle('Variable importance - Random forest')+
  labs(y = 'Importance (node impurity)')
ggsave('./fig/diagnostic/RF.var_imp.jpg', var_imp, width = 5, height = 4.5, dpi=600)
ggsave('./fig/diagnostic/RF.var_imp.pdf', var_imp, width = 4, height = 3.5)

# Collect predictions
pred_test <- augment(extract_workflow(m), testing(dat_split))

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
  #geom_smooth(col='chartreuse3', fill='#67cd0079', lty = 1, alpha=.1, lwd=.55) +
  geom_abline(lty = 2, color = 'red', lwd=.8, alpha=.8) +
  theme_bw() + theme(legend.position = 'bottom') 
pred_test_plot
ggsave('./fig/diagnostic/RF.pred_test_plot.jpg', pred_test_plot, width = 7, height = 5, dpi=600)


# Plot residulas
regions.name <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria',
                 'Corsica', 'Crete', 'Croatia', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany',
                 'Greece', 'Hungary', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein',
                 'Lithuania', 'Luxembourg', 'Malta', 'Moldova', 'Montenegro', 'Netherlands', 'North Macedonia',
                 'Norway', 'Poland', 'Portugal', 'Romania', 'Sardinia', 'Serbia', 'Sicily',
                 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Ukraine', 'United Kingdom')

EU <- './data/spatial/euro+med_map/euro+med.map.shp' %>%
  read_sf %>%
  semi_join(data.frame(name = regions.name), by = 'name') %>% #include only the needed regions
  st_transform(crs = 25832) %>% #reproject
  st_simplify(dTolerance = 1000) # simplify borders  

pred_test$resid <- (pred_test$S-pred_test$.pred) # model residuals
hist(pred_test$resid)

#rasterize
map_residuals=list()
for (h in c('Forest','Grassland','Scrub','Wetland')) {
  
  br = c(-Inf,-5,-2.5, -0.5,0.5,2.5, 5, Inf)
  lb = c('< -5','-5 – -2.5','-2.5 – -0.5','-0.5 – +0.5', '+0.5 – +2.5', '+2.5 – +5', '> +5')
  
  df.cols = data.frame(lb, cols=hcl.colors(length(lb), 'Spectral'))
  
  pdh <- pred_test %>% filter(habitat==h)
  contas = pdh %>%
    summarise(n = paste0('n = ', prettyNum(n(),big.mark=',', scientific=F))) 
  
  min5 <- function(x){sum(x)>=5}
  r0 <- rast(res = 50*1000, extent=ext(EU), crs=crs(EU)) 
  r.min5 <- rasterize(pdh %>% select(x,y) %>% as.matrix(), r0, values=1, fun=min5)
  r.min5 <- clamp(r.min5, lower=1, value=FALSE)
  r <- rasterize(pdh %>% select(x,y) %>% as.matrix(), r0, values=pdh$resid, fun=mean)
  r <- mask(r, r.min5)
  newvals.cont = as.vector(values(r))
  newvals.cat = cut(newvals.cont,
                    breaks = br, labels = lb)
  values(r) <- newvals.cat
  
  map_residuals[[h]] <- ggplot() +
    geom_raster(data=as.data.frame(r, xy=T), aes(x,y,fill=mean))+
    geom_sf(data=EU, fill=NA, color=alpha('black',0.5)) +
    theme(axis.title=element_blank())+
    labs(fill='Species richness\nresiduals (mean)') +
    ggtitle(h)+
    scale_fill_manual(values=df.cols$cols[df.cols$lb %in% newvals.cat])+
    geom_text(data=contas, x=-283716.1, y=7629361, aes(label = n), fontface=2, size=4)+
    theme(title = element_text(face=2, size=12),
          axis.title = element_blank())
}
map_residuals_combined <- cowplot::plot_grid(map_residuals[[1]],map_residuals[[2]],map_residuals[[3]],map_residuals[[4]])
map_residuals_combined
ggsave('./fig/diagnostic/RF.map_distribution_of_residuals.jpg', map_residuals_combined, width = 6.5, height = 6, dpi=600)
ggsave('./fig/diagnostic/RF.map_distribution_of_residuals.pdf', map_residuals_combined, width = 10, height = 6)

#### 3. Partial dependence plots ####

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
ggsave('./fig/diagnostic/RF.pdp.pdf', p, width = 7.5, height = 4.5)

# data.frame(x=c(0, 1e06, 2e06), y=c(4.5e06, 5.5e06, 6.5e06)) %>%
#   sf::st_as_sf(coords=c('x','y'), crs=25832) %>%
#   sf::st_transform('WGS84') %>%
#   sf::as_Spatial() %>%
#   as.data.frame() %>%
#   mutate_all(round, 1)

# Calculate H stats
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
ggsave('./fig/diagnostic/RF.Hstats.pdf', hs_p, width = 8, height = 4.5)

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



#### 4. Partial dependence plots (using DALEX) ####

# library(DALEX); library(DALEXtra)

# explainer <- explain_tidymodels(
#   model = extract_workflow(m),
#   data = select(dat_train, lon:habitat),
#   y = dat_train$S,
#   verbose = T
# )

# # Plot time
# pdp_time <- model_profile(
#   explainer,
#   variables = 'year',
#   N = NULL,
#   groups = 'habitat'
# )

# pdp_time_plot <- as_tibble(pdp_time$agr_profiles) %>%
#   mutate(`_label_` = str_remove(`_label_`, 'workflow_')) %>%
#   ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
#   geom_line(size = 1, alpha = 0.8) +
#   labs(
#     x = 'Time (year)',
#     y = 'Species richness',
#     color = 'Habitat',
#     title = 'Partial dependence plot',
#     subtitle = 'Predictions from Random forest'
#   ) +
#   scale_x_continuous(labels = seq(1945, 2022, 15),
#                      breaks = seq(1945, 2022, 15))+
#   theme_bw()
# ggsave('./fig/diagnostic/RF.pdp_time_plot.jpg', pdp_time_plot, width = 5, height = 4.5, dpi=600)


# # Plot plot size
# pdp_plot_size <- model_profile(
#   explainer,
#   variables = 'plot_size',
#   N = NULL,
#   groups = 'habitat'
# )

# pdp_plot_size_plot <- as_tibble(pdp_plot_size$agr_profiles) %>%
#   mutate(`_label_` = str_remove(`_label_`, 'workflow_')) %>%
#   ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
#   geom_line(size = 1, alpha = 0.8) +
#   labs(
#     x = 'Plot size (square m)',
#     y = 'Species richness',
#     color = 'Habitat',
#     title = 'Partial dependence plot',
#     subtitle = 'Predictions from Random forest'
#   ) +
#   scale_x_continuous(labels = c(1, 100, 250, 500, 750, 1000),
#                      breaks = c(1, 100, 250, 500, 750, 1000))+
#   theme_bw()
# pdp_plot_size_plot
# ggsave('./fig/diagnostic/RF.pdp_plot_size_plot.jpg', pdp_plot_size_plot, width = 5, height = 4.5, dpi=600)



quit(save='no')
