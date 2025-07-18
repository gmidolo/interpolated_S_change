################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Date: 18.07.2025
################################################################################

# Description: Create dynamic maps of species richness dynamics on a shiny app

################################################################################

#### 1. Load and prepare data ####

# Load packages
library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(viridis)
library(tidyr)
library(readr)

# Load prediction data
dat <- './data/preds/preds_stdpltsz.rf.csv' %>%
  read_csv(show_col_types = FALSE) %>%
  dplyr::select(x, y, habitat, year, contains('S_pred_')) %>%
  filter(year >= 1960 & year <= 2020)

# Manually centering plots by rounding coordinates
fact = 1000*10 # 10 km distance 
dat <- dat %>%
  mutate(x = round(x / fact) * fact,
         y = round(y / fact) * fact) %>%
  group_by(x, y, habitat) %>%
  summarise(
    n = n(), # number of plots
    across(where(is.numeric), mean, .names = "{.col}"), # average predictions across nearest plots per year
    .groups = 'drop'
  )


#### 2. Define UI ####

ui <- fluidPage(
  titlePanel("Six decades of change in alpha diversity of European plant communites"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mode", "Select Mapping Mode:",
                  choices = c("Species richness change", "Species richness per year")),
      selectInput("habitat", "Select Habitat:",
                  choices = c("Forest", "Grassland", "Scrub", "Wetland")),
      
      div(style = "font-size: 1.0em; font-style: italic; margin-top: 2px;",
          textOutput("plot_size_note")), # dynamic note below habitat selector
      
      conditionalPanel(
        condition = "input.mode == 'Species richness change'",
        sliderInput("years", "Select Time Range:", 
                    min = 1960, max = 2020, value = c(1960, 2020), sep = ""),
        radioButtons("metric", "Metric of change:",
                     choices = c("Percentage (%)" = "perc",
                                 "Log Response Ratio" = "lnrr",
                                 "No. Species" = "diff"))
      ),
      conditionalPanel(
        condition = "input.mode == 'Species richness per year'",
        sliderInput("single_year", "Select Year:",
                    min = 1960, max = 2020, value = 1960, sep = "")
      )
    ),
    mainPanel(
      leafletOutput("map", height = "800px")
    )
  )
)


# 3. Define server ####

server <- function(input, output, session) {
  
  output$plot_size_note <- renderText({
    req(input$habitat)
    
    sizes <- c(
      "Forest" = "300 m²",
      "Grassland" = "20 m²",
      "Scrub" = "64 m²",
      "Wetland" = "50 m²"
    )
    
    paste0("Predictions at plot size = ", sizes[input$habitat])
  })
  
  dat_sf <- reactive({
    st_as_sf(dat, coords = c("x", "y"), crs = 25832) %>% 
      filter(habitat == input$habitat) %>% 
      st_transform(4326)
  })
  
  change_data <- reactive({
    req(input$years[1] < input$years[2])
    dat_hab <- dat_sf()
    y1 <- paste0("S_pred_", input$years[1])
    y2 <- paste0("S_pred_", input$years[2])
    
    dat_hab <- dat_hab %>% 
      mutate(change_value = case_when(
        input$metric == "perc"  ~ 100 * ((.data[[y2]] - .data[[y1]]) / .data[[y1]]),
        input$metric == "lnrr"  ~ log(.data[[y2]] / .data[[y1]]),
        input$metric == "diff"  ~ .data[[y2]] - .data[[y1]]
      ))
    
    dat_hab <- dat_hab %>%
      mutate(change_cat = case_when(
        input$metric == "perc" ~ cut(change_value,
                                     breaks = c(-Inf, -50, -25, -10, -5, 5, 10, 25, 50, Inf),
                                     labels = c('< -50%', '-50% – -25%', '-25% – -10%', '-10% – -5%',
                                                '-5% – 5%', '5% – 10%', '10% – 25%', '25% – 50%', '> 50%'),
                                     include.lowest = TRUE
        ),
        input$metric == "lnrr" ~ cut(change_value,
                                     breaks = c(-10, -0.5, -0.1, -0.05, 0.05, 0.1, 0.5, 10),
                                     labels = c('< -0.5', '-0.5 – -0.1', '-0.1 – -0.05', '-0.05 – 0.05',
                                                '0.05 – 0.1', '0.1 – 0.5', '> 0.5'),
                                     include.lowest = TRUE
        ),
        input$metric == "diff" ~ cut(change_value,
                                     breaks = c(-Inf, -10, -5, -1, 1, 5, 10, Inf),
                                     labels = c('< -10', '-10 – -5', '-5 – -1', '-1 – 1',
                                                '1 – 5', '5 – 10', '> 10'),
                                     include.lowest = TRUE
        )
      ))
    
    dat_hab
  })
  
  snapshot_data <- reactive({
    dat_hab <- dat_sf()
    yr <- paste0("S_pred_", input$single_year)
    dat_hab %>%
      mutate(S_value = log10(.data[[yr]]))  # Keep log10 richness for coloring
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      setView(lng = 15, lat = 57, zoom = 4)
  })
  
  observe({
    zoom <- input$map_zoom
    if (is.null(zoom)) zoom <- 4  # default zoom
    
    leafletProxy("map") %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    if (input$mode == "Species richness change") {
      dat_plot <- change_data()
      
      if (input$metric == "perc") {
        levels_lab <- c('< -50%', '-50% – -25%', '-25% – -10%', '-10% – -5%',
                        '-5% – 5%', '5% – 10%', '10% – 25%', '25% – 50%', '> 50%')
      } else if (input$metric == "lnrr") {
        levels_lab <- c('< -0.5', '-0.5 – -0.1', '-0.1 – -0.05', '-0.05 – 0.05',
                        '0.05 – 0.1', '0.1 – 0.5', '> 0.5')
      } else {
        levels_lab <- c('< -10', '-10 – -5', '-5 – -1', '-1 – 1',
                        '1 – 5', '5 – 10', '> 10')
      }
      
      cols <- hcl.colors(length(levels_lab), "Spectral")
      pal <- colorFactor(palette = cols, levels = levels_lab)
      
      # Adjust point size by zooming level
      fac = 0.2
      zoomy = input$map_zoom
      scale_factor = ifelse(zoomy*fac<=1, 1, zoomy*fac)
      
      leafletProxy("map") %>%
        addCircleMarkers(data = dat_plot,
                         radius = ~(log(n)+2) * scale_factor,
                         color = ~pal(change_cat),
                         stroke = FALSE,
                         fillOpacity = 0.7,
                         popup = ~paste0("Mean species richness change: ", round(change_value, 2),
                                         "<br>No. plots: ", n)) %>%
        addLegend("topright", pal = pal, values = dat_plot$change_cat,
                  title = "Species richness change", opacity = 0.9)
      
    } else {
      dat_plot <- snapshot_data()
      
      ## define min-max S across all sets of predictions
      # s_min <- round(dat[,6:66] %>% min %>% log10)
      # s_max <- round(dat[,6:66] %>% max %>% log10)
      s_min <- round(3 %>% min %>% log10)
      s_max <- round(91 %>% max %>% log10)
      
      log_breaks <- pretty(c(s_min, s_max), n = 9)
      log_breaks <- log_breaks[log_breaks >= s_min & log_breaks <= s_max]
      
      backtransformed_labels <- round(10^log_breaks, 0)
      
      pal <- colorNumeric(palette = "plasma", domain = c(s_min, s_max))
      
      # Adjust point size by zooming level
      fac = 0.2
      zoomy = input$map_zoom
      scale_factor = ifelse(zoomy*fac<=1, 1, zoomy*fac)
      
      leafletProxy("map") %>%
        addCircleMarkers(data = dat_plot,
                         radius = ~(log(n)+2) * scale_factor, 
                         color = ~pal(S_value),
                         stroke = FALSE,
                         fillOpacity = 0.7,
                         popup = ~paste0("Mean species richness: ", round(10^S_value, 2),
                                         "<br>No. plots: ", n)) %>%
        addLegend("topright",
                  colors = pal(log_breaks),
                  labels = backtransformed_labels,
                  title = "Species richness",
                  opacity = 0.9)
    }
  })
  
}


#### 4. Run the app ####

shinyApp(ui, server)
