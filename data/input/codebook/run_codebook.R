# R code to generate Data Dictionary (Codebook) for the data stored in this repository

# Author: Gabriele Midolo
# Date: 15-10-2025

#### 1. Load packages ####
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(codebookr)
})

#### 2. Load Data ####
dat <- bind_rows(
  read_csv('./data/input/EVA.csv.gz', show_col_types = FALSE),
  read_csv('./data/input/ReSurveyEU_clean.csv.gz', show_col_types = FALSE)
)

#### 3. Add attributes ####
dat <- dat %>%
  # Variable: database
  cb_add_col_attributes(
    database,
    description = "Database (EVA or ReSurveyEurope)", 
    source = "Aggregated field obtained from 'For EVA (Y/N)' and 'ReSurvey plot (Y/N)' in the raw data",
    col_type = "Categorical",
    value_labels = c(
      "European Vegetation Archive core data (EVA)" = "EVA",
      "ReSurveyEurope" = "ReSurveyEU")
  ) %>%
  
  # Variable: resurv_id
  cb_add_col_attributes(
    resurv_id,
    description = "Unique identifier for Resurvey plot", 
    source = "Aggregated field obtained by grouping the following columns in the raw data: 'RS_PROJTYP', 'ReSurvey site', and 'ReSurvey plot'",
    col_type = "Categorical"
  ) %>%
  
  # Variable: plot_id
  cb_add_col_attributes(
    plot_id,
    description = "Unique identifier for plot observation",
    source = "Orgiginal EVA/ReSurveyEurope data (original name: 'PlotObservationID' or 'PlotID')",
    col_type = "Categorical"
  ) %>%
  
  # Variable: ReSur_site
  cb_add_col_attributes(
    ReSur_site,
    description = "Identifier for resurvey site",
    source = "Original ReSurveyEurope database field (original name: 'ReSurvey site')",
    col_type = "Categorical"
  ) %>%
  
  # Variable: ReSur_plot
  cb_add_col_attributes(
    ReSur_plot,
    description = "Identifier for resurvey plot",
    source = "Original ReSurveyEurope database field  (original name: 'ReSurvey plot')",
    col_type = "Categorical"
  ) %>%
  
  # Variable: ReSur_obs
  cb_add_col_attributes(
    ReSur_obs,
    description = "Identifier for resurvey observation",
    source = "Original ReSurveyEurope database field (original name: 'ReSurvey observation')",
    col_type = "Categorical"
  ) %>%
  
  # Variable: ReSur_time
  cb_add_col_attributes(
    ReSur_time,
    description = "Time of the observation of a resurvey in the time serie",
    source = "ReSurveyEurope database fields (original name: 'RS_TIME')",
    col_type = "Categorical"
  ) %>%
  
  # Variable: ReSur_type
  cb_add_col_attributes(
    ReSur_type,
    description = "Resurvey plot location type (either Permanent or Resampling)",
    source = "Modified from ReSurveyEurope raw data (original name: 'RS_PROJTYP')",
    col_type = "Categorical",
    value_labels = c(
      'Resurveyed plots lacking accurate relocation information ("quasi-permanent")' = 'Resampling',
      'Resurveyed plots at precisely relocated sites' = 'Permanent'
    )
  ) %>% 
  
  # Variable: dataset
  cb_add_col_attributes(
    dataset,
    description = "EVA/ReSurveyEurope dataset name",
    source = "Orgiginal EVA/ReSurveyEurope data (original name: 'Dataset')",
    col_type = "Categorical"
  ) %>%
  
  # Variable: ESy
  cb_add_col_attributes(
    ESy,
    description = "EUNIS habitat code (level-3, where possible)",
    source = "Orgiginal EVA/ReSurveyEurope data (original name: 'Expert System')",
    col_type = "Categorical"
  ) %>%
  
  # Variable: S
  cb_add_col_attributes(
    S,
    description = "Species richness of vascular plants", 
    source = "Obtained by calculating the number of unique vascular plant species ('matched_concept') per plot using the '*_notJUICE_species.csv' raw data",
    col_type = "Numeric"
  ) %>%
  
  # Variable: habitat
  cb_add_col_attributes(
    habitat,
    description = "Level-1 EUNIS habitat type (= 'Forest' or 'Grassland' or 'Scrub' or 'Wetland')",
    source = "Obtained by using the level-1 level information, the first letter of 'ESy' (original name: 'Expert System')",
    col_type = "Categorical",
    value_labels = c(
      "Forest" = "Forest",
      "Grassland" = "Grassland",
      "Scrub" = "Scrub",
      "Wetland" = "Wetland"
    ),
    skip_pattern = "Only to plots categorized either as forest (code ‘T’), grassland (code ‘R’), scrub (code ‘S’), or wetland (code ‘Q’) were included"
  ) %>%
  
  # Variable: lon
  cb_add_col_attributes(
    lon,
    description = "Longitude in WGS84; unit: degrees",
    source = "Orgiginal EVA/ReSurveyEurope data (original name: 'Longitude')",
    col_type = "Numeric"
  ) %>%
  
  # Variable: lat
  cb_add_col_attributes(
    lat,
    description = "Latitude in WGS84; unit: degrees",
    source = "Orgiginal EVA/ReSurveyEurope data (original name: 'Latitude')",
    col_type = "Numeric"
  ) %>%
  
  # Variable: x
  cb_add_col_attributes(
    x,
    description = "Longitude in EPSG:25832 (northing); unit: m",
    source = "Projection calculation of 'lon' using sf::st_transform(..., crs = 25832)",
    col_type = "Numeric"
  ) %>%
  
  # Variable: y
  cb_add_col_attributes(
    y,
    description = "Latitude in EPSG:25832 (easting); unit: m",
    source = "Projection calculation of 'lat' using sf::st_transform(..., crs = 25832)",
    col_type = "Numeric"
  ) %>%
  
  # Variable: elev
  cb_add_col_attributes(
    elev,
    description = "Elevation a.s.l. at plot location (unit: m)",
    source = "Data obtained from Digital Elevation Model with 90-m horizontal resolution from the European Space Agency; Available at: https://doi.org/10.5069/G9028PQB. Last accessed 12 August 2024",
    col_type = "Numeric"
  ) %>%
  
  # Variable: year
  cb_add_col_attributes(
    year,
    description = "Year of sampling (unit: YYYY)",
    source = "Extracted from full sampling date of EVA/ReSurveyEurope raw data (original name: 'Date of recording')",
    col_type = "Numeric",
    skip_pattern = "Only plots sampled between 1945 and 2023 were included"
  ) %>%
  
  # Variable: plot_size
  cb_add_col_attributes(
    plot_size,
    description = "Plot size (area) (unit: m²)",
    source = "Orgiginal EVA/ReSurveyEurope data (original name: 'Relevé area (m²)')",
    col_type = "Numeric",
    skip_pattern = "Only plots with known sizes and ranging from 1 to 100 m² for grasslands, scrub and wetlands, and from 100 to 1000 m², were included"
  ) %>%
  
  # Variable: x_mean
  cb_add_col_attributes(
    x_mean,
    description = "Centroid longitude (in EPSG:25832) of ReSurveyEurope plot observations assigned to the same 'resurv_id'; used to quantify location uncertainty of the observations",
    source = "Calculated variable (grouped mean of 'x' by 'resurv_id')",
    col_type = "Numeric"
  ) %>%
  
  # Variable: y_mean
  cb_add_col_attributes(
    y_mean,
    description = "Centroid latitude (in EPSG:25832) of ReSurveyEurope plot observations assigned to the same 'resurv_id'; used to quantify location uncertainty of the observations",
    source = "Calculated variable (grouped mean of 'y' by 'resurv_id')",
    col_type = "Numeric"
  ) %>%
  
  # Variable: max_dist_m
  cb_add_col_attributes(
    max_dist_m,
    description = "Maximum distance (in m) between of ReSurveyEurope plot observations assigned to the same 'resurv_id'; used to quantify location uncertainty of the observations",
    source = "Calculated variable (based on x/y coordinates)",
    col_type = "Numeric",
    skip_pattern = "Only available for ReSurveyEurope data. Only records with 'max_dist_m' <= 100 were included."
  )


#### 4. Get codebook and export ####

# Get the codebook
codebook_doc <- codebook(
  df = dat,
  title = 'Data Dictionary (Codebook) for the EVA and ReSurveyEurope Combined Dataset',
  subtitle = 'Six decades of losses and gains in alpha diversity of European plant communities by Midolo, G., Clark, A. T., Chytrý, M., Essl, F., Dullinger, S., Jandt, U., ... & Keil, P. (2025). ',
  description = 'This document provides a detailed data dictionary for the combined EVA and ReSurveyEurope dataset, including variable descriptions, data sources, and original names in the raw data.'
)

# Export to Word
print(codebook_doc, target = './data/input/codebook/codebook.docx')