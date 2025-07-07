## Input .csv data description

This table describes the columns found in `EVA.csv.gz` and `ReSurveyEU_clean.csv.gz`:

| Column Name | Description |
| :---------- | :----------------------------------------------------------------- |
| `database` | Database (`EVA` or `ReSurveyEU`) |
| `resurv_id` | Unique identifier for Resurvey plot |
| `plot_id` | Unique identifier for plot observation |
| `ReSur_site` | Identifier for resurvey site (original values from ReSurveyEurope database) |
| `ReSur_plot` | Identifier for resurvey plot (original values from ReSurveyEurope database) |
| `ReSur_obs` | Identifier for resurvey observation (original values from ReSurveyEurope database) |
| `ReSur_time` | Time of the observation of a resurvey (original values from ReSurveyEurope database) |
| `ReSur_type` | Resurvey plot type (either `Permanent` or `Resampling`) |
| `dataset` | Dataset name |
| `ESy` | EUNIS habitat code (level-3, where possible) |
| `S` | Species richness |
| `habitat` | Level 1 EUNIS habitat category (= 'Forest' or 'Grassland' or 'Scrub' or 'Wetland') |
| `lon` | Longitude in WGS84 |
| `lat` | Latitude in WGS84 |
| `x` | Longitude in EPSG:25832 |
| `y` | Latitude in EPSG:25832 |
| `elev` | Elevation a.s.l. in m |
| `year` | Year of sampling |
| `plot_size` | Plot size in squared m |
| `x_mean` | Centroid longitude (in EPSG:25832) of plots observations assigned to the same resurv_id |
| `y_mean` | Centroid latitude (in EPSG:25832) of plots observations assigned to the same resurv_id |
| `max_dist_m` | Maximum distance (in m) between plots observations assigned to the same resurv_id |