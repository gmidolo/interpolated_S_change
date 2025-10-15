# Six decades of losses and gains in alpha diversity of European plant communities

## Data and R code repository

This repository (<https://github.com/gmidolo/interpolated_S_change>) contains the **data** and **R code** for our study, allowing for the reproduction of the main analyses and supplementary materials.

We interpolated spatiotemporal changes in vascular plant species richness using a new method based on machine learning that does not require temporal replication at sites. Using 698,692 one-time survey vegetation plots from the [European Vegetation Archive](https://euroveg.org/eva-database/), we estimated trends in vascular plant alpha diversity across Europe and validated our approach against 22,852 independent time series from [ReSurveyEurope](https://euroveg.org/resurvey/). 

NOTE: We have excluded some large outputs (model files and site-level predictions) from this repository to keep its size manageable. A copy of this repository including additional data necessary for reproducing our analyses is stored on Zenodo (DOI: [10.5281/zenodo.15836616](https://doi.org/10.5281/zenodo.15836616)).

An interactive map exploring interpolated spatiotemporal changes in species richness can be accessed at [gmidolo.shinyapps.io/interpolated_s_change_app](https://gmidolo.shinyapps.io/interpolated_s_change_app/). Its code is deposited in a separate GitHub repository ([gmidolo/interpolated_S_change_app](https://github.com/gmidolo/interpolated_S_change_app)).

#### Authors:
**Gabriele Midolo** <a href="https://orcid.org/0000-0003-1316-2546" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Adam Thomas Clark <a href="https://orcid.org/0000-0002-8843-3278" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Milan Chytrý <a href="https://orcid.org/0000-0002-8122-3075" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Franz Essl <a href="https://orcid.org/0000-0001-8253-2112" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Stefan Dullinger <a href="https://orcid.org/0000-0003-3919-0887" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Ute Jandt <a href="https://orcid.org/0000-0002-3177-3669" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Helge Bruelheide <a href="https://orcid.org/0000-0003-3135-0356" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Olivier Argagnon <a href="https://orcid.org/0000-0003-2069-7231" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Idoia Biurrun <a href="https://orcid.org/0000-0002-1454-0433" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Alessandro Chiarucci <a href="https://orcid.org/0000-0003-1160-235X" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Renata Ćušterevska <a href="https://orcid.org/0000-0002-3849-6983" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Pieter De Frenne <a href="https://orcid.org/0000-0002-8613-0943" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Michele De Sanctis <a href="https://orcid.org/0000-0002-7280-6199" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Jürgen Dengler <a href="https://orcid.org/0000-0003-3221-660X" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Jan Divíšek <a href="https://orcid.org/0000-0002-5127-5130" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Tetiana Dziuba <a href="https://orcid.org/0000-0001-8621-0890" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Rasmus Ejrnæs <a href="https://orcid.org/0000-0003-2538-8606" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Emmanuel Garbolino <a href="https://orcid.org/0000-0002-4954-6069" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Estela Illa <a href="https://orcid.org/0000-0001-7136-6518" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Anke Jentsch <a href="https://orcid.org/0000-0002-2345-8300" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Borja Jiménez-Alfaro <a href="https://orcid.org/0000-0001-6601-9597" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Jonathan Lenoir <a href="https://orcid.org/0000-0003-0638-9582" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Jesper Erenskjold Moeslund <a href="https://orcid.org/0000-0001-8591-7149" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Francesca Napoleone <a href="https://orcid.org/0000-0002-3807-7180" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Remigiusz Pielech <a href="https://orcid.org/0000-0001-8879-3305" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Sabine B. Rumpf <a href="https://orcid.org/0000-0001-5909-9568" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Irati Sanz-Zubizarreta <a href="https://orcid.org/0009-0000-9816-2574" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Vasco Silva <a href="https://orcid.org/0000-0003-2729-1824" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Jens-Christian Svenning <a href="https://orcid.org/0000-0002-3415-0862" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Grzegorz Swacha <a href="https://orcid.org/0000-0002-6380-2954" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Martin Večeřa <a href="https://orcid.org/0000-0001-8507-791X" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Denys Vynokurov <a href="https://orcid.org/0000-0001-7003-6680" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>,
Petr Keil <a href="https://orcid.org/0000-0003-3017-1858" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15"/></a>


## Table of Contents

* [Contacts](#contacts)
* [Data (data folder)](#data-data-folder)
    * [Input data](#raw-input-data---input)
    * [Model results](#model-results---models-not-deposited-here)
    * [Model predictions](#model-predictions---data-preds)
    * [Spatial - data/spatial](#spatial---data-spatial)
* [R Code (src folder)](#r-code-src-folder)
    * [1. Tuning, training, testing Random Forests and XGBoost](#1-tuning-training-testing-random-forests-and-xgboost---1_model_training_testing)
        * [Random Forests](#random-forests-randomforest)
        * [XGBoost](#xgboost-xgboost)
    * [2. Validation using time series data](#2-validation-using-time-series-data---2_validation)
    * [3. Interpolation (predict Random Forests over time)](#3-interpolation-predict-random-forests-over-time---3_interpolation)
    * [4. Spatial and temporal distance effects](#4-spatial-and-temporal-distance-effects---4_distance_error_dependency)
        * [Model performance response to spatial and temporal distance to the training data](#model-performance-response-to-spatial-and-temporal-distance-to-the-training-data)
        * [Residuals spatial autocorrelation](#residuals-spatial-autocorrelation)
    * [5. Plot main figures](#5-plot-main-figures---5_plot_figures)
    * [6. Raw data processing (not reproducible)](#6-preprocessing-raw-eva-and-resurveyeurope-data-not-reproducible---6_raw_data_processing)
    * [Utility Functions](#utility-functions)
* [Figures (fig folder)](#figures-fig-folder)
* [Metadata](#metadata)
    * [1. General Dataset Information](#1-general-dataset-information)
    * [2. Dataset Description](#2-dataset-description)
    * [3. Access and Licensing](#3-access-and-licensing)
    * [4. Methods and Processing](#4-methods-and-processing)
* [License](#license)
* [Citation](#citation)


---

### Contacts:
**Gabriele Midolo** Department of Spatial Sciences, Faculty of Environmental Sciences  
Czech University of Life Sciences Prague, Praha-Suchdol, Czech Republic  
ORCID: [0000-0003-1316-2546](https://orcid.org/0000-0003-1316-2546)  
Email: midolo@fzp.czu.cz

---

## Data ([`data`](data) folder)

### Raw input data - [`input`](data/input)

The EVA and ReSurveyEurope data for this project, including the species lists for each plot, are not stored in this repository. They can be only obtained by requesting access from the [European Vegetation Archive (EVA) Coordinating Board](https://euroveg.org/eva-database/) using the appropriate data request form (see [`How To Obtain Data`](https://euroveg.org/eva-database/obtaining-data)). Please reference this project's DOI ([10.58060/250x-we61](https://doi.org/10.58060/250x-we61)) in your request.

Check the **[Metadata](#metadata)** section for more information on the raw data and the data stored in this repository. Metadata are also available in .md and .docx format: [/data/input/metadata](/data/input/metadata)
A **Data Dictionary (Codebook)** is available in .docx format: [/data/input/codebook](/data/input/codebook).

The folder contains input data files used for the analyses:
- [`input/EVA.csv.gz`](data/input/EVA.csv.gz): A .gz-compressed CSV file containing selected vegetation plots from the EVA database.
- [`input/ReSurveyEU_clean.csv.gz`](data/input/ReSurveyEU_clean.csv.gz): A .gz-compressed CSV file with selected plots from the ReSurveyEurope dataset.

Overview of EVA and ReSurveyEurope data stored in this repository:

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


### Model results - [`models`](data/models) (*Not Deposited Here*)

The folder contains model results and related objects (last fit, tuning results, and cross validation results).
Files are deposited in the `data/models` folder of the Zenodo repository (DOI: [10.5281/zenodo.15836616](https://doi.org/10.5281/zenodo.15836616)).

| File | Method | Description |
| :---------- | :---------- | :--------------------------------------------------- |
| `XGB.tune_res.rds` | XGBoost | Tuning results; output from `tune::tune_grid()` |
| `XGB.last_fit.rds` | XGBoost | Final fitted model object; output from `tune::last_fit()` |
| `XGB.cv_res.rds` | XGBoost | Cross-validation results from random CV; output from `tune::fit_resamples()` |
| `XGB.cv_metrics.csv` | XGBoost | Summary stats from random CV; output from `tune::collect_metrics(summarize=FALSE)` |
| `RF.tune_res.rds` | Random Forests | Tuning results; output from `tune::tune_grid()` |
| `RF.last_fit.rds` | Random Forests | Final fitted model object; output from `tune::last_fit()` |
| `RF.cv_res.rds` | Random Forests | Cross-validation results from random CV; output from `tune::fit_resamples()` |
| `RF.cv.temporal_metrics.csv` | Random Forests | Summary stats from temporal-block CV; output from `tune::collect_metrics(summarize=FALSE)` |
| `RF.cv.spatiotemporal_metrics.csv` | Random Forests | Summary stats from spatiotemporal-block CV; output from `tune::collect_metrics(summarize=FALSE)` |
| `RF.cv.spatial_1km_metrics.csv` | Random Forests | Summary stats from spatial-block CV (1 km block resolution); output from `tune::collect_metrics(summarize=FALSE)` |
| `RF.cv.spatial_10km_metrics.csv` | Random Forests | Summary stats from spatial-block CV (10 km block resolution); output from `tune::collect_metrics(summarize=FALSE)` |
| `RF.cv.spatial_100km_metrics.csv` | Random Forests | Summary stats from spatial-block CV (100 km block resolution); output from `tune::collect_metrics(summarize=FALSE)` |
| `RF.cv_metrics.csv` | Random Forests | Summary stats from random CV; output from `tune::collect_metrics(summarize=FALSE)` |

### Model predictions - [`data/preds`](data/preds)

- Plot-level predictions for species richness (*S*) values in each year from 1960 to 2020, and calculated Δ*S* using various metrics and across different time periods (`preds_stdpltsz.rf.csv` file; *not deposited here due to large size*: see the [Zenodo repository](https://doi.org/10.5281/zenodo.15836616))
- Summary stats ([`preds/pdp`](data/preds/pdp)) to plot partial dependence curves (S change over time)

### Spatial - [`data/spatial`](data/spatial)

- [`EU_shape_map.rds`](data/spatial/EU_shape_map.rds): .rds object containing study area / [Euro+Med](https://europlusmed.org/) regions; requires `sf` R package
- [`biogeoregions.rds`](data/spatial/biogeoregions.rds): .rds object containing European biogeographic regions; modified from [eea.europa.eu](https://www.eea.europa.eu/en/analysis/maps-and-charts/biogeographical-regions-in-europe-2); requires `sf` R package
- Output data of spatial and temporal distance effects, and data for spatial correlograms (*not deposited here*: see the [Zenodo repository](https://doi.org/10.5281/zenodo.15836616))

## R Code ([`src`](src) folder)

### 1. Tuning, training, testing Random Forests and XGBoost - [`1_model_training_testing`](src/1_model_training_testing)

#### Random Forests [`randomforest`](src/1_model_training_testing/randomforest)
- [`tuning.rf.R`](src/1_model_training_testing/randomforest/tuning.rf.R): Train and tune Random Forests model
- [`cv.rf.R`](src/1_model_training_testing/randomforest/cv.rf.R): Random cross validation for the Random Forests model
- [`cv.block.temporal.rf.R`](src/1_model_training_testing/randomforest/cv.block.temporal.rf.R): Temporal block cross validation for Random Forests
- [`cv.block.spatial.rf.R`](src/1_model_training_testing/randomforest/cv.block.spatial.rf.R): Spatial block cross validation for the Random Forests
- [`cv.block.spatiotemporal.rf.R`](src/1_model_training_testing/randomforest/cv.block.spatiotemporal.rf.R): Spatio-Temporal block cross validation for Random Forests
- [`diagnostics.rf.R`](src/1_model_training_testing/randomforest/diagnostics.rf.R): Evaluate and interpret Random Forests model performance, feature importance, spatial residuals distribution, and feature interactions

#### XGBoost [`xgboost`](src/1_model_training_testing/xgboost)
- [`tuning.xgb.R`](src/1_model_training_testing/xgboost/tuning.xgb.R): Train and tune XGBoost model
- [`cv.xgb.R`](src/1_model_training_testing/xgboost/cv.xgb.R): Random cross validation for the XGBoost model

### 2. Validation using time series data - [`2_validation`](src/2_validation)

- [`run_validation_all_tests.R`](src/2_validation/run_validation_all_tests.R): Perform various validation tests assessing model performance over different testing data
- [`plot_validation.R`](src/2_validation/plot_validation.R): Plot results of various validation tests
- [`run_validation_ReSurveyEurope_repeats.R`](src/2_validation/run_validation_ReSurveyEurope_repeats.R): Validate model approach using ReSurveyEurope data (repeat random sampling of plots 100 times)

### 3. Interpolation (predict Random Forests over time) - [`3_interpolation`](src/3_interpolation)

- [`full_ensemble_predictions.R`](src/3_interpolation/full_ensemble_predictions.R): In each plot, predict species richness (*S*) values for each year from 1960 to 2020, and calculate Δ*S* using different metrics
- [`raw_predictions_habitat.R`](src/3_interpolation/raw_predictions_habitat.R): Calculate summary statistics for *S* (95% CI around the mean, prediction intervals) from 1960 to 2020 for each habitat type
- [`raw_predictions_biogeo.R`](src/3_interpolation/raw_predictions_biogeo.R): Calculate summary statistics for *S* (95% CI around the mean, prediction intervals) from 1960 to 2020 for each habitat type and biogeographic region

### 4. Spatial and temporal distance effects - [`4_distance_error_dependency`](src/4_distance_error_dependency)

#### Model performance response to spatial and temporal distance to the training data
- [`spatial_distance_compute.R`](src/4_distance_error_dependency/spatial_distance_compute.R): Calculate spatial distance rasters for test plots relative to training data, and for ReSurveyEurope plots relative to EVA plots
- [`spatial_distance_EVA_ReSurveyEU.R`](src/4_distance_error_dependency/spatial_distance_EVA_ReSurveyEU.R): Count the number of plots in ReSurveyEurope grouped by spatial distance classes relative to the EVA training data
- [`spatial_and_temporal_distance_analysis.R`](src/4_distance_error_dependency/spatial_and_temporal_distance_analysis.R): Assess how model performance (root mean squared error, RMSE) varies across different spatial and temporal distance classes

#### Residuals spatial autocorrelation
- [`moran_correlog_compute.R`](src/4_distance_error_dependency/moran_correlog_compute.R): Calculate spatial autocorrelation (Moran's *I*) of model residuals over 250 km grid cells
- [`moran_correlog_plot.R`](src/4_distance_error_dependency/moran_correlog_plot.R): Plot spatial autocorrelation of model residuals (correlogram)

### 5. Plot main figures - [`5_plot_figures`](src/5_plot_figures)

- [`histogram_perc_change.R`](src/5_plot_figures/histogram_perc_change.R): Distribution of interpolated Δ*S* (%) across all plots (Figure 2a of the main manuscript)
- [`plot_trends_raw_predictions_habitat.R`](src/5_plot_figures/plot_trends_raw_predictions_habitat.R): Interpolated *S* response to time for each habitat type (Figure 2b of the main manuscript)
- [`plot_trends_raw_predictions_biogeoregions.R`](src/5_plot_figures/plot_trends_raw_predictions_biogeoregions.R): Interpolated *S* response to time for each habitat type and biogeographic region (Figure 3 of the main manuscript)
- [`mapping_perc_change.R`](src/5_plot_figures/mapping_perc_change.R): Distribution maps of interpolated Δ*S* (%) for each habitat and time period (Figure 4 of the main manuscript)

### 6. Preprocessing raw EVA and ReSurveyEurope data (not reproducible) - [`6_raw_data_processing`](src/6_raw_data_processing)

**N.B. This part of the code is used to prepare the raw data from EVA and ReSurveyEurope. It is not directly reproducible unless a data request is made to the EVA and ReSurveyEurope governing board.**

- [`1.prepare.data.R`](src/6_raw_data_processing/1.prepare.data.R): Main script to preprocess raw data retrieved in EVA/ReSurveyEurope proj. no. 222 (DOI: [ 10.58060/250x-we61](https://doi.org/10.58060/250x-we61))
- [`2.duplicate.search.R`](src/6_raw_data_processing/2.duplicate.search.R): Remove presumed or actual duplicate plots within and across EVA and ReSurveyEurope data (plots with the same year of sampling, geographic coordinates, and species composition)
- [`3.clean.ReSurveyEU.R`](src/6_raw_data_processing/3.clean.ReSurveyEU.R): Remove plots in ReSurveyEurope with 'uncertain' plots location (i.e., $\ge$ 100 m distance between observations of the same plots)

### Utility Functions

- [`utils.R`](src/utils.R): Custom functions used to load static ReSurveyEurpe data and run model validations


## Figures ([`fig`](fig) folder)

Contains figures presented in the main manuscript and supporting information.

## Metadata

Metadata are also available in .md and .docx format: [/data/input/metadata](/data/input/metadata).
A **Data Dictionary (Codebook)** is available in .docx format: [/data/input/codebook](/data/input/codebook).

### 1. General Dataset Information

|  | **Description** |
|-------------------------------|-----------------------------------------|
| **Dataset names** | European Vegetation Archive (EVA); ReSurveyEurope |
| **Version** | Version 2024-09-19 (DOI: <https://doi.org/10.58060/hgrb-sw46>) |
| **Project name** | "EVA project \# 222 – 2024-09-12 Interpolated dynamics of local plant diversity in European vegetation - G. Midolo \| SELECTION 2024-10-31" (DOI: <https://doi.org/10.58060/250x-we61>) |
| **Date of creation** | Data selection date for project #222: 2024-10-31. The EVA database is in development since 2012 and first made available for use in research projects in 2014. The first data call for ReSurveyEurope was announced in 2020. |
| **Citation** | Chytrý, M., Hennekens, S. M., Jiménez‐Alfaro, B., Knollová, I., Dengler, J., Jansen, F., ... & Yamalov, S. (2016). European Vegetation Archive (EVA): an integrated database of European vegetation plots. *Applied Vegetation Science*, *19*(1), 173-180. <https://doi.org/10.1111/avsc.12191> <br> Knollová, I., Chytrý, M., Bruelheide, H., Dullinger, S., Jandt, U., Bernhardt‐Römermann, M., ... & Essl, F. (2024). ReSurveyEurope: A database of resurveyed vegetation plots in Europe. *Journal of Vegetation Science*, *35*(2), e13235. <https://doi.org/10.1111/jvs.13235> |
| **Data curators** | [The EVA Coordinating Board and EVA Council](https://euroveg.org/eva-database/who-we-are) <br> [The ReSurveyEurope Board](https://euroveg.org/resurvey/) |

------------------------------------------------------------------------

### 2. Dataset Description

|   | **Raw data (\*)** | **Data in the repository** |
|-----------------|----------------------|----------------------------------|
| **Summary** | EVA contains vegetation plot data across Europe, including species composition and plot metadata. ReSurveyEurope contains data in a similar format, but for plots with repeated measurements over time. | Subset restricted to plot observations relevant for analyzing temporal changes in vascular plant species richness across Europe. Includes processed and harmonized species and site-level data used for modelling species richness change. |
| **Provenance** | Compiled from 308 databases contributed by data owners under EVA and ReSurveyEurope governance. | Compiled from 263 databases contributed by data owners under EVA and ReSurveyEurope governance. |
| **Temporal coverage (range)** | 1873–2023 | 1945–2023 (model training); 1960–2020 (predictions/interpolation) |
| **Geographical coverage (range)** | Longitude (WGS84): -180.00 – 64.84; Latitude (WGS84): -90.00 – 80.15 (includes geographic outliers) | Longitude (WGS84): -10.52 – 38.79; Latitude (WGS84): 34.80 – 71.12 |
| **Sampling frame** | Vegetation plots categorized as various European habitats: marine, coastal, inland water, wetland, grassland, scrub, forest, inland habitats with little soil, and vegetated man-made habitats, as defined in the EUNIS Habitat Classification System. | Vegetation plots categorized exclusively as forest, grassland, scrub, and wetland vegetation, as defined in the EUNIS Habitat Classification System. |
| **Number of records and plots** | No. of plots: 1,745,721; core EVA: 1,676,182; ReSurveyEurope plot observations: 103,397 | No. of core EVA: 675,840 vegetation plots; core ReSurveyEurope plot observations: 73,886 (from 22,852 resurvey plots) |
| **Number of variables** | 46 fields in the header metadata (raw) | 22 fields in the shared data |

(\*) These values refer to the raw data released for project \# 222 (<https://doi.org/10.58060/250x-we61>)

------------------------------------------------------------------------

### 3. Access and Licensing

|   | **Raw data (\*)** | **Data in the repository** |
|-----------------|----------------------|----------------------------------|
| **Access conditions** | Data access follows either free-access, semi-restricted, or restricted models, depending on the availability regime assigned by the database custodians. In any case, **the data are only accessible through a data request to the EVA and ReSurveyEurope Governing Board.** | Data access follows either free-access, semi-restricted, or restricted models, depending on the availability regime assigned by the database custodians. The data stored in this repository can be used to reproduce the main analyses (models, predictions, figures). **Use in other works or publications requires Governing Board approval.** |
| **How to obtain access** | Data can be accessed through a request to the Governing Board, following [Article 5 of the EVA rules](https://euroveg.org/download/eva-rules.pdf). More info: <https://euroveg.org/eva-database/obtaining-data> | Through this repository ([`input`](data/input)). **To use these data elsewhere, a data request must be submitted to the Governing Board**. |
| **License / Terms of Use** | Depending on the availability regime assigned by the database custodians and EVA/ReSurveyEurope Governing Board. | Derived data follow the same access constraints. Metadata and code in this repository are licensed under [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/). |
| **Data availability statement** | Not publicly accessible due to third-party ownership. | All data-processing scripts and data that are essential to reproduce the analyses are publicly available in this repository. |

(\*) These values refer to the raw data released for project \# 222 (<https://doi.org/10.58060/250x-we61>)

------------------------------------------------------------------------

### 4. Methods and Processing

|   | **Raw data (\*)** | **Data in the repository** |
|-----------------|----------------------|----------------------------------|
| **Data collection methods** | Records of the abundance and/or occurrence of plant species found in vegetation plots collected in the field. | Not available (data are elaborated based on the raw data). |
| **Inclusion / exclusion criteria** | Includes different vegetation types, plot sizes, quality levels, and time periods available in the raw data. Eligibility of databases to be included in EVA and ReSurveyEurope is regulated by the Governing Board. | Vegetation plots with full lists of vascular plant species categorized as forest, grassland, scrub, and wetland, with plot size 1–1000 m², valid geographic coordinates, and sampled between 1945–2023. Detailed selection criteria are in the main manuscript. |
| **Data harmonization** | Managed by the EVA and ReSurveyEurope curators (species abundances and nomenclature). | Same as 'raw data'. |
| **Data processing (main) steps** | Managed by EVA and ReSurveyEurope curators. | 1\. Application of inclusion/exclusion criteria to the raw data. <br> 2. Calculation of species richness (number of unique vascular plant species) for each plot using the species list data from the raw data. |
| **Quality control** | Conducted by data providers and EVA/ReSurveyEurope curators. | Conducted by Gabriele Midolo with help from database custodians. Included coordinate plausibility checks (manual and automated), species richness plausibility checks (manual), and duplicate removal within and between EVA and ReSurveyEurope (automated). |
| **Processing code** | Not available. | All R scripts documenting processing steps and quality control are stored in this repository ([src/6_raw_data_processing](src/6_raw_data_processing)). |
| **Software environment** | Varies by data provider. | R version 4.4.2. R packages for data processing: `tidyverse`, `sf`, `terra`. |
| **Reproducibility** | Not reproducible without subset definition. | Fully reproducible for EVA-authorized users using the R code stored in this repository. |

(\*) These values refer to the raw data released for project \# 222 (<https://doi.org/10.58060/250x-we61>)

## License

**Data** are available under the terms of the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International license (CC BY-NC-ND 4.0) (<https://creativecommons.org/licenses/by-nc-nd/4.0/>).

**Code** are available under the terms of the GNU General Public License v3.0 (GPL-3.0) (<https://www.gnu.org/licenses/gpl-3.0.html>).

## Citation

*This repository is not linked to any publication yet.*