################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 07.07.2025
################################################################################

# Description: Plot summary correlogram for each 250-km grid cell

################################################################################

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
})

d <- './data/spatial/moran_correlog_data/correlog_df.csv' %>%
  read_csv(show_col_types = F)
d$id %>% unique() %>% length()
d$moranI %>% hist()

p <- d %>% 
  ggplot(aes(y=moranI, x=distance_meters/1000)) +
  geom_path(aes(group=id), alpha=.1, color='midnightblue') + 
  geom_hline(yintercept = 0, lty = 3) + 
  geom_smooth(color='red3') + 
  scale_y_continuous(
    breaks = c(-0.4, -0.2, 0, 0.2, 0.4),
    labels = c('< -0.4', '-0.2', '0', '0.2', '> 0.4'),
    name = expression("Moran's " * italic("I")),
    limits = c(-0.4,0.4)
  ) +
  scale_x_continuous(
        breaks = seq(0, 250, 50),
        name = 'Distance [km]'
  ) +
  theme_bw()

# Export in the 'fig/diagnostic' folder
ggsave('./fig/diagnostic/correlogram.jpg', p, width = 5, height = 4)