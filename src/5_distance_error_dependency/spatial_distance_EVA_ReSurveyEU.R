################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 07.07.2025
################################################################################

# Description: Count the number of plots in ReSurveyEurope grouped by
#              (spatial) distance classes to the EVA training data

################################################################################

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
})

# Define breaks
manual_breaks <- c(-Inf, 0, 0.5, 0.75, 1, 2.5, 5, Inf)
manual_labels <- c('-Inf','0-0.5', '0.0-0.75', '0.75-1', '1-2.5', '2.5-5', '>5')

# Get spatial distances for each resurvey plot
d <- './data/spatial/training_data_distance_csv/spatial_distance_ReSurveyEU_from_EVA.csv' %>%
  read_csv(show_col_types = F) %>%
    mutate(space_dist_km = space_dist/1000) %>%
    mutate(
    space_dist_km_cat = cut(space_dist_km,
      breaks = manual_breaks,
      labels = manual_labels,
      right = F,
      include.lowest = TRUE)
    )

# Plot
p <- d %>%
  group_by(space_dist_km_cat) %>%
  summarise(no.plots = n(),
            no.plots.perc = paste0( round(100*n()/nrow(d), 2), ' %')) %>%
  ggplot(aes(space_dist_km_cat, no.plots)) +
  geom_col() +
  geom_text(aes(label = no.plots.perc),
            vjust = -0.5,
            size = 3) +
  labs(
    x = 'Minimum distance from the EVA data [km]',
    y = 'No. plots (ReSurveyEurope)',
  ) +
  theme_bw()

# Export to `/fig/diagnostic` folder
ggsave('./fig/diagnostic/distance_ReSuEU_from_EVA.jpg', p, width = 4.5, height = 4, dpi=600)