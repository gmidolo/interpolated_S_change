#--- TRENDS MAP ---#


#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages(
  {library(tidyverse)}
)


# Load predictions
dat <- read_csv('./data/preds/preds_stdpltsz.rf.csv', show_col_types = F)
str(dat)

#filter only dat from 1960 to 2020
dat <- dat %>%
  filter(year >= 1960 & year <= 2020)

prop.table(table(dat$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more
prop.table(table(dat$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more

dat_hi=dat %>% filter(habitat == 'Forest')
prop.table(table(dat_hi$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more in forests
prop.table(table(dat_hi$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more in forests
mean(dat_hi$S_perc.change_1960.2020) # fraction of plots losing -20% or more

dat_hi=dat %>% filter(habitat == 'Grassland')
prop.table(table(dat_hi$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more in grasslands
prop.table(table(dat_hi$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more in grasslands
mean(dat_hi$S_perc.change_1960.2020) # fraction of plots losing -20% or more

dat_hi=dat %>% filter(habitat == 'Scrub')
prop.table(table(dat_hi$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more in scrubs
prop.table(table(dat_hi$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more in scrubs
mean(dat_hi$S_perc.change_1960.2020) # fraction of plots losing -20% or more

dat_hi=dat %>% filter(habitat == 'Wetland')
prop.table(table(dat_hi$S_perc.change_1960.2020 >= 20))[2]*100 # fraction of plots gaining 20% or more in wetlands
prop.table(table(dat_hi$S_perc.change_1960.2020 <= -20))[2]*100 # fraction of plots losing -20% or more in wetlands
mean(dat_hi$S_perc.change_1960.2020) # fraction of plots losing -20% or more


### Define periods
periods <- c('From 1960 to 1980', 'From 1980 to 2000', 'From 2000 to 2020', 'From 1960 to 2020')
period.dat <- data.frame(
  periods=periods,
  min=c(1960, 1980, 2000, 1960),
  max=c(1980, 2000, 2020, 2020)
)

# calc mean values
means_dats <- dat %>%
  filter(year >= 1960 & year <= 2020) %>% # select only plots sampled within that period
  select(plot_id, habitat, contains('S_pred_')) %>%
  gather('year', 'S_pred', contains('S_pred_')) %>%
  mutate(year = as.numeric(gsub("\\D", "", year))) %>%
  group_by(habitat, year) %>%
  summarise(mean = mean(S_pred), sd = sd(S_pred), n=n())  %>%
  ungroup() 

# reproduce PDP-stle plot:
means_dats %>%
  ggplot(aes(year,mean,color=habitat, group=habitat))+
  geom_path(linewidth = 1) +
  labs(y='mean species richness', x='Year') +
  theme_bw()
means_dats %>%
  ggplot(aes(year,mean,color=habitat, group=habitat))+
  geom_path(linewidth = 1) +
  labs(y='mean species richness', x='Year') +
  theme_bw()+
  facet_wrap(~habitat, scales='free_y')

period_slope = list()
period = list()
for (i in periods[1:3]) {
  st=Sys.time()
  cat('Period: ', i,' - Start')
  
  max_i = period.dat %>% filter(periods==i) %>% pull(max)
  min_i = period.dat %>% filter(periods==i) %>% pull(min)
  
  period[[i]] <- dat %>%
    select(plot_id, habitat, contains('S_pred_')) %>%
    gather('year', 'S_pred', contains('S_pred_')) %>%
    mutate(year = as.numeric(gsub("\\D", "", year))) %>%
    filter(year >= min_i & year <= max_i) 
  
  period[[i]] %>% ggplot(aes(year, S_pred)) + facet_wrap(~habitat, scales = 'free_y') + geom_hex(bins=12)+ geom_smooth(method = 'lm')
  
  # calc slope estimate
  period_slope[[i]] <- period[[i]] %>%
    group_by(habitat) %>%
    do(broom::tidy(lm(S_pred ~ year , data = .))) %>%
    filter(term == 'year')
  cat('Period: ', i,' - Done')
  print(Sys.time()-st)
  
}
period_slope <- period_slope %>% bind_rows(.id='period')
period <- period %>% bind_rows(.id='period')

period_slope$slope_decades <- period_slope$estimate*10
period_slope$slope_decades %>% hist()
period_slope$slope_decades %>% range()
vals = round(seq(-1, 1, 0.01), 2)
pals = hcl.colors(length(vals), 'Spectral')


pdf('./fig/generaltrend_stdpltsz/legenda.pdf', height = 5, width = 3.5)
legend_image <- as.raster(matrix(rev(colorspace::adjust_transparency(pals, 0.95)), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Species gained/loss\nper decade')
text(x=1.5, y = seq(0,1,l=5), labels = quantile(as.numeric(vals), seq(0,1,.25)))
rasterImage(legend_image, 0, 0, 1,1)
dev.off()


your.number=0.512434
which.min(abs(vals - your.number))

means_dats$mean %>% range()
y_limits <- c(13.8, 29.1)

means_cntrd <- means_dats %>%
  select(1:3) %>%
  left_join(
    means_dats %>% filter(year==1960) %>% select(habitat, mean) %>% rename(mean.1960=mean), 'habitat'
  ) %>%
  group_by(habitat) %>%
  mutate(
    mean.change.1960 = mean-mean.1960,
    perc.change.1960 = 100*(mean.change.1960/mean.1960),
    mean.habitat = mean(mean),
    mean.change = mean-mean.habitat,
    perc.change = 100*(mean.change/mean.habitat)
  )

p_means=list()
p_means.1960=list()
p_means.mean=list()
hab = c('Forest','Grassland','Scrub','Wetland')
for (h in hab) {
  
  t1 <- period_slope %>% filter(period=='From 1960 to 1980') %>% filter(habitat==h) %>% pull(slope_decades)
  t1 <- pals[which(vals %in% round(t1, 2))]
  t2 <- period_slope %>% filter(period=='From 1980 to 2000') %>% filter(habitat==h) %>% pull(slope_decades)
  t2 <- pals[which(vals %in% round(t2, 2))]
  t3 <- period_slope %>% filter(period=='From 2000 to 2020') %>% filter(habitat==h) %>% pull(slope_decades)
  t3 <- pals[which(vals %in% round(t3, 2))]
  
  scaleFUN <- function(x) sprintf("%.1f", x)

  ## Plot means
  p_means[[h]] <- means_dats %>%
    filter(habitat==h) %>%
    ggplot(aes(year, mean)) +

    annotate('rect', xmin=1960, xmax=1980, ymin=-Inf, ymax=Inf, alpha=.8, fill=t1) +
    annotate('rect', xmin=1980, xmax=2000, ymin=-Inf, ymax=Inf, alpha=.8, fill=t2) +
    annotate('rect', xmin=2000, xmax=2020, ymin=-Inf, ymax=Inf, alpha=.8, fill=t3) +

    geom_vline(xintercept = 1960, lty=3, linewidth=.8, col ='grey')+
    geom_vline(xintercept = 1980, lty=3, linewidth=.8, col ='grey')+
    geom_vline(xintercept = 2000, lty=3, linewidth=.8, col ='grey')+
    geom_vline(xintercept = 2020, lty=3, linewidth=.8, col ='grey')+

    # #plot 95%CI
    # geom_ribbon(aes(ymin=mean-1.96*(sd/sqrt(n)), ymax=mean+(sd/sqrt(n))), fill='grey30', alpha=1)+

    # #plot SD
    # geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd), fill='grey30', alpha=.8)+

    # plot path
    geom_path(linewidth = 1, color='grey30') +

    # plot linear regressions per periods
    geom_smooth(data = means_dats %>% filter(year>=1960 & year<=1980 & habitat==h), method = 'lm', color='black', se=F, linewidth=.50) +
    geom_smooth(data = means_dats %>% filter(year>=1980 & year<=2000 & habitat==h), method = 'lm', color='black', se=F, linewidth=.50) +
    geom_smooth(data = means_dats %>% filter(year>=2000 & year<=2020 & habitat==h), method = 'lm', color='black', se=F, linewidth=.50) +

    #facet_wrap(~habitat, scales='free_y') +
    ggtitle(h)+
    labs(y='mean species richness', x='Year') +
    theme_bw()+
    theme(axis.title = element_blank(),
          panel.grid = element_blank())+

    #scale_y_continuous(labels=scaleFUN) + # fix y axis limits
    lims(y=y_limits)

  ## Plot relative changes
  y_limits_centrd <- c(-2.5, 2.5)
  p_means.1960[[h]] <- means_cntrd %>%
    filter(habitat==h) %>%
    ggplot(aes(year, mean.change.1960)) +

    annotate('rect', xmin=1960, xmax=1980, ymin=-Inf, ymax=Inf, alpha=.8, fill=t1) +
    annotate('rect', xmin=1980, xmax=2000, ymin=-Inf, ymax=Inf, alpha=.8, fill=t2) +
    annotate('rect', xmin=2000, xmax=2020, ymin=-Inf, ymax=Inf, alpha=.8, fill=t3) +
    
    # plot zero change line
    geom_segment(x = 1960, y = 0, xend = 2020, yend = 0, lty=3, color='grey30', linewidth=.3) +

    # geom_vline(xintercept = 1960, lty=3, linewidth=.8, col ='grey')+
    # geom_vline(xintercept = 1980, lty=3, linewidth=.8, col ='grey')+
    # geom_vline(xintercept = 2000, lty=3, linewidth=.8, col ='grey')+
    # geom_vline(xintercept = 2020, lty=3, linewidth=.8, col ='grey')+

    # #plot 95%CI
    # geom_ribbon(aes(ymin=median-1.96*(sd/sqrt(n)), ymax=median+(sd/sqrt(n))), fill='grey30', alpha=1)+

    # #plot SD
    # geom_ribbon(aes(ymin=median-sd, ymax=median+sd), fill='grey30', alpha=.8)+

    # plot path
    geom_path(linewidth = 1, color='grey30') +

    # plot linear regressions per periods
    geom_smooth(data = means_cntrd %>% filter(year>=1960 & year<=1980 & habitat==h), method = 'lm', color='black', se=F, linewidth=.50) +
    geom_smooth(data = means_cntrd %>% filter(year>=1980 & year<=2000 & habitat==h), method = 'lm', color='black', se=F, linewidth=.50) +
    geom_smooth(data = means_cntrd %>% filter(year>=2000 & year<=2020 & habitat==h), method = 'lm', color='black', se=F, linewidth=.50) +

    #facet_wrap(~habitat, scales='free_y') +
    ggtitle(h)+
    labs(y='mean species richness', x='Year') +
    theme_bw()+
    theme(axis.title = element_blank(),
          panel.grid = element_blank())+

    scale_y_continuous(
      limits = y_limits_centrd,
      #labels = round(seq(-2, 2, 1) + (means_cntrd %>% filter(habitat==h) %>% pull(mean.1960) %>% unique()),1),
      labels = c( '-2', '-1',  round((means_cntrd %>% filter(habitat==h) %>% pull(mean.1960) %>% unique()),1),  '+1',  '+2'),
      breaks = seq(-2, 2, 1)
    )

  ## Plot relative changes to the mean
  y_limits_centrd <- c(-1.5, 1.5)
  p_means.mean[[h]] <- means_cntrd %>%
    filter(habitat==h) %>%
    ggplot(aes(year, mean.change)) +

    annotate('rect', xmin=1960, xmax=1980, ymin=-Inf, ymax=Inf, alpha=.8, fill=t1) +
    annotate('rect', xmin=1980, xmax=2000, ymin=-Inf, ymax=Inf, alpha=.8, fill=t2) +
    annotate('rect', xmin=2000, xmax=2020, ymin=-Inf, ymax=Inf, alpha=.8, fill=t3) +
    
    # plot zero change line
    # geom_hline(yintercept = 0, lty=3, color='grey30', linewidth=.8) +
    
    geom_vline(xintercept = 1960, lty=3, linewidth=.8, col ='grey')+
    geom_vline(xintercept = 1980, lty=3, linewidth=.8, col ='grey')+
    geom_vline(xintercept = 2000, lty=3, linewidth=.8, col ='grey')+
    geom_vline(xintercept = 2020, lty=3, linewidth=.8, col ='grey')+

    # #plot 95%CI
    # geom_ribbon(aes(ymin=median-1.96*(sd/sqrt(n)), ymax=median+(sd/sqrt(n))), fill='grey30', alpha=1)+

    # #plot SD
    # geom_ribbon(aes(ymin=median-sd, ymax=median+sd), fill='grey30', alpha=.8)+

    # plot path
    geom_path(linewidth = 1, color='grey30') +

    # plot linear regressions per periods
    geom_smooth(data = means_cntrd %>% filter(year>=1960 & year<=1980 & habitat==h), method = 'lm', color='black', se=F, linewidth=.50) +
    geom_smooth(data = means_cntrd %>% filter(year>=1980 & year<=2000 & habitat==h), method = 'lm', color='black', se=F, linewidth=.50) +
    geom_smooth(data = means_cntrd %>% filter(year>=2000 & year<=2020 & habitat==h), method = 'lm', color='black', se=F, linewidth=.50) +

    #facet_wrap(~habitat, scales='free_y') +
    ggtitle(h)+
    labs(y='mean species richness', x='Year') +
    theme_bw()+
    theme(axis.title = element_blank(),
          panel.grid = element_blank())+

    scale_y_continuous(
      limits = y_limits_centrd,
      #labels = round(seq(-2, 2, 1) + (means_cntrd %>% filter(habitat==h) %>% pull(mean.1960) %>% unique()),1),
      labels = c( '-1.5', '-1','-0.5',  means_cntrd %>% group_by(habitat) %>% summarize(mean=mean(mean)) %>% filter(habitat==h) %>% pull(mean) %>% round(1), '+0.5',  '+1',  '+1.5'),
      breaks = seq(-1.5, 1.5, .5)
    )
                       
}

cowplot::plot_grid(p_means$Forest,p_means$Grassland,p_means$Scrub, p_means$Wetland)
cowplot::plot_grid(p_means.1960$Forest,p_means.1960$Grassland,p_means.1960$Scrub, p_means.1960$Wetland)
cowplot::plot_grid(p_means.mean$Forest,p_means.mean$Grassland,p_means.mean$Scrub, p_means.mean$Wetland)

ggsave("./fig/generaltrend_stdpltsz/minitrends.pdf", cowplot::plot_grid(p_means$Forest,p_means$Grassland,p_means$Scrub,p_means$Wetland), width = 5*0.77, height = 4.5*0.77)
ggsave("./fig/generaltrend_stdpltsz/minitrends.changeto1960.pdf", cowplot::plot_grid(p_means.1960$Forest,p_means.1960$Grassland,p_means.1960$Scrub,p_means.1960$Wetland), width = 5*0.77, height = 4.5*0.77)


## Histogram with perc.change ##

d2hist <- dat %>%
  filter(year >= 1960 & year <= 2020) %>%
  mutate(slpdec=S_perc.change_1960.2020)
d2hist$slpdec %>% range
d2hist$slpdec %>% mean
d2hist$slpdec %>% median
hist_total = d2hist %>% 
  ggplot(aes(x=slpdec))+
  geom_histogram(bins = 22, fill='grey', color='grey20') +
  scale_y_continuous(labels = scales::unit_format(unit = "k", scale = 1e-3))+
  theme_bw()+
  geom_vline(aes(xintercept=mean(slpdec)), color="red", linetype=1, linewidth=1, alpha=.7) +
  geom_vline(aes(xintercept=0), color="black", linetype=2, linewidth=.5) +
  scale_x_continuous(breaks = c(-150,-100,-50,-10,10,50,100,150), limits = c(-100,190)) +
  theme(panel.grid.minor = element_blank())+
  labs(x=expression('Species richness change (%)'*'100*'*'(('*S[2020]-S[1960]*')'/S[1960]*')'), y='N. plots')+
  theme(axis.text = element_text(size=11),
        axis.text.x = element_text(angle = 45, hjust=1))
hist_total
ggsave("./fig/generaltrend_stdpltsz/hist_pchange.pdf", hist_total, width = 4, height = 4)