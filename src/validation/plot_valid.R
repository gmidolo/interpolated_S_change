#--- PLOT VALIDATION RESULTS VIA RESURVEY DATA & EVA using RANDOM FOREST ---#


# Load packages
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(tidymodels)
  }
)

# 1. Plot Validation results (multi tests) ####

# Get results
res <-  './data/validation/multi_test_validation.rds' %>%
 read_rds()
res$EVA_only$eval
res$ReSurv_only$eval
res$EVA_and_ReSurv$eval


res_preds <- map(res, \(x) x$preds) %>% bind_rows(.id = 'Training data')
res_preds$.validation <-  str_replace_all(res_preds$.validation, '_', ' ') %>% str_to_sentence()
res_preds$.validation <- paste0(res_preds$.validation, ' S')
res_preds$.validation <- str_replace_all(res_preds$.validation, 'change S', 'S change')
res_preds$obj <- paste0('Training: ', res_preds$`Training data`, '\nTesting: ', res_preds$.validation)
res_preds$obj <- str_remove_all(res_preds$obj, '_only') %>% str_replace_all('_and_',' + ')

conv = data.frame(
  obj = unique(res_preds$obj)
)
conv$obj2 =  c("Training: EVA\nTesting: Static S (20% testing)","Training: EVA\nTesting: Static S (ReSurvey)", "Training: EVA\nTesting: ΔS (ReSurvey)", 
              "Training: ReSurveyEU\nTesting: Static S (20% testing)", "Training: ReSurveyEU\nTesting: Static S (ReSurvey)", "Training: ReSurveyEU\nTesting: ΔS (ReSurvey)",
               "Training: EVA + ReSurveyEU\nTesting: Static S (20% testing)", "Training: EVA + ReSurveyEU\nTesting: Static S (ReSurvey)", "Training: EVA + ReSurveyEU\nTesting: ΔS (ReSurvey)"
              )
res_preds <- res_preds %>%
 left_join(conv) %>%
 select(-obj) %>%
 rename(obj=obj2)

res_preds$obj <- factor(res_preds$obj,
                       c("Training: ReSurveyEU\nTesting: Static S (20% testing)", "Training: EVA\nTesting: Static S (20% testing)", "Training: EVA + ReSurveyEU\nTesting: Static S (20% testing)",
                         "Training: ReSurveyEU\nTesting: Static S (ReSurvey)", "Training: EVA\nTesting: Static S (ReSurvey)", "Training: EVA + ReSurveyEU\nTesting: Static S (ReSurvey)", 
                         "Training: ReSurveyEU\nTesting: ΔS (ReSurvey)", "Training: EVA\nTesting: ΔS (ReSurvey)", "Training: EVA + ReSurveyEU\nTesting: ΔS (ReSurvey)"
                       )
                        )

res_counts <- res_preds %>% group_by(obj) %>% summarise(n=n()) 
res_counts$n <- str_remove_all(format(res_counts$n,big.mark=",",scientific=FALSE), ' ')

res_eval <- map(res, \(x) x$eval) %>% bind_rows(.id = 'Training data')
res_eval$.validation <-  str_replace_all(res_eval$.validation, '_', ' ') %>% str_to_sentence()
res_eval$.validation <- paste0(res_eval$.validation, ' S')
res_eval$.validation <- str_replace_all(res_eval$.validation, 'change S', 'S change')
res_eval$obj <- paste0('Training: ', res_eval$`Training data`, '\nTesting: ', res_eval$.validation)
res_eval$obj <- str_remove_all(res_eval$obj, '_only') %>% str_replace_all('_and_',' + ')

conv = data.frame(
  obj = unique(res_eval$obj)
)
conv$obj2 =  c("Training: EVA\nTesting: Static S (20% testing)","Training: EVA\nTesting: Static S (ReSurvey)", "Training: EVA\nTesting: ΔS (ReSurvey)", 
               "Training: ReSurveyEU\nTesting: Static S (20% testing)", "Training: ReSurveyEU\nTesting: Static S (ReSurvey)", "Training: ReSurveyEU\nTesting: ΔS (ReSurvey)",
               "Training: EVA + ReSurveyEU\nTesting: Static S (20% testing)", "Training: EVA + ReSurveyEU\nTesting: Static S (ReSurvey)", "Training: EVA + ReSurveyEU\nTesting: ΔS (ReSurvey)"
              )
res_eval <- res_eval %>%
 left_join(conv) %>%
 select(-obj) %>%
 rename(obj=obj2)

res_eval$obj <- factor(res_eval$obj,
                       c("Training: ReSurveyEU\nTesting: Static S (20% testing)", "Training: EVA\nTesting: Static S (20% testing)", "Training: EVA + ReSurveyEU\nTesting: Static S (20% testing)",
                         "Training: ReSurveyEU\nTesting: Static S (ReSurvey)", "Training: EVA\nTesting: Static S (ReSurvey)", "Training: EVA + ReSurveyEU\nTesting: Static S (ReSurvey)", 
                         "Training: ReSurveyEU\nTesting: ΔS (ReSurvey)", "Training: EVA\nTesting: ΔS (ReSurvey)", "Training: EVA + ReSurveyEU\nTesting: ΔS (ReSurvey)"
                       )
                        )
res_eval <- res_eval %>%
left_join(res_counts,'obj') %>%
mutate(txt = paste0('RMSE = ', round(rmse, 2), '\nrsq = ', round(rsq, 2), '\ncor = ', round(cor,2), '\nn = ', n)) %>%
 left_join(
  res_preds %>% group_by(obj) %>% 
   summarise(.obs=range(.obs) %>% quantile(probs=0.175),
             .pred=range(.pred) %>% quantile(probs=0.835))
 ) 

p <- ggplot(res_preds, aes(.obs, .pred)) +
 geom_hex(bins = 45) +
 geom_label(data=res_eval, aes(label=txt), size = 2.8, fill=NA, col='grey20') +
 facet_wrap(.~obj, scales = 'free') +
 scale_fill_gradient(low = 'lightblue', high = 'midnightblue', trans='log10') +
 labs(x='Observed', 
      y='Predicted', 
      fill='N. plots') +
  geom_abline(lty = 2, color = 'red', lwd=.5, alpha=.5) +
  theme_bw()
p

ggsave('fig/diagnostic/valid.rf.jpg', p, width = 10, height = 7.5, dpi = 600)
#ggsave('fig/diagnostic/valid.rf.pdf', p, width = 10, height = 7.5)

# 2. Distribution of Observed vs Predicted in Resurvey Europe ####
hist_plt <- res_preds %>% filter(obj == 'Training: EVA + ReSurveyEU\nTesting: ΔS (ReSurvey)') %>%
  rename(Predicted=.pred, Observed=.obs) %>%
  gather('k','v',Predicted:Observed) %>%
  ggplot(aes(v, fill = k)) + 
  geom_histogram(alpha = 0.5, aes(y = after_stat(count)), position = 'identity', bins = 50, color='grey')+
  theme_bw()+
  labs(fill='', y='Frequency', x='Species richness change (lnRR)')+
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3))
hist_plt 

ggsave(
  'fig/diagnostic/hist_deltaS_ReSurvey.rf.jpg', hist_plt, width = 5, height = 4, dpi = 600
)


# 3. Comparisons of different ReSurveyEU sampling types (permanent vs. resampling) ####
d2p = res_preds %>% filter(!is.na(ReSur_type))
e2p = d2p %>% group_by(obj, ReSur_type) %>% summarise(cor=cor(.obs, .pred), n = n()) %>% mutate(rsq=cor^2) %>%
 mutate(txt = paste0('rsq = ', round(rsq, 2), '\ncor = ', round(cor,2), '\nn = ', n)) %>%
  left_join(
  d2p %>% group_by(obj, ReSur_type) %>% 
   summarise(.obs=range(.obs) %>% quantile(probs=0.175),
             .pred=range(.pred) %>% quantile(probs=0.835))
 ) 

p <- ggplot(d2p, aes(.obs, .pred)) +
 geom_hex(bins = 45) +
 geom_label(data=e2p, aes(label=txt), size = 2.8, fill=NA, col='grey20') +
 facet_wrap(ReSur_type~obj, scales = 'free') +
 scale_fill_gradient(low = 'lightblue', high = 'midnightblue', trans='log10') +
 labs(x='Observed', 
      y='Predicted', 
      fill='N. plots') +
  geom_abline(lty = 2, color = 'red', lwd=.5, alpha=.5) +
  theme_bw()
p
ggsave('fig/diagnostic/valid.rf_SamplingType.jpg', p, width = 10, height = 6.5, dpi = 600)
