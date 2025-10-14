# Metadata for EVA and ReSurveyEurope

**Date:** 2025-10-14\
**Authors:** Gabriele Midolo, on behalf of the Author Team\
**Contact:** [gabriele.midolo\@gmail.com](mailto:gabriele.midolo@gmail.com)\
**Persistent Identifier (Zenodo):** [10.5281/zenodo.15836616](https://doi.org/10.5281/zenodo.15836616)\
**Accessed data:** European Vegetation Archive (EVA) and ReSurveyEurope datasets

------------------------------------------------------------------------

## 1. General Dataset Information

|  | **Description** |
|-------------------------------|-----------------------------------------|
| **Dataset names** | European Vegetation Archive (EVA); ReSurveyEurope |
| **Version** | Version 2024-09-19 (DOI: <https://doi.org/10.58060/hgrb-sw46>) |
| **Project name** | "EVA project \# 222 – 2024-09-12 Interpolated dynamics of local plant diversity in European vegetation - G. Midolo \| SELECTION 2024-10-31" (DOI: <https://doi.org/10.58060/250x-we61>) |
| **Date of creation** | Data selection date for project #222: 2024-10-31. The EVA database is in development since 2012 and first made available for use in research projects in 2014. The first data call for ReSurveyEurope was announced in 2020. |
| **Citation** | Chytrý, M., Hennekens, S. M., Jiménez‐Alfaro, B., Knollová, I., Dengler, J., Jansen, F., ... & Yamalov, S. (2016). European Vegetation Archive (EVA): an integrated database of European vegetation plots. *Applied Vegetation Science*, *19*(1), 173-180. <https://doi.org/10.1111/avsc.12191> <br> Knollová, I., Chytrý, M., Bruelheide, H., Dullinger, S., Jandt, U., Bernhardt‐Römermann, M., ... & Essl, F. (2024). ReSurveyEurope: A database of resurveyed vegetation plots in Europe. *Journal of Vegetation Science*, *35*(2), e13235. <https://doi.org/10.1111/jvs.13235> |
| **Data curators** | [The EVA Coordinating Board and EVA Council](https://euroveg.org/eva-database/who-we-are) <br> [The ReSurveyEurope Board](https://euroveg.org/resurvey/) |

------------------------------------------------------------------------

## 2. Dataset Description

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

## 3. Access and Licensing

|   | **Raw data (\*)** | **Data in the repository** |
|-----------------|----------------------|----------------------------------|
| **Access conditions** | Data access follows either free-access, semi-restricted, or restricted models, depending on the availability regime assigned by the database custodians. In any case, **the data are only accessible through a data request to the EVA and ReSurveyEurope Governing Board.** | Data access follows either free-access, semi-restricted, or restricted models, depending on the availability regime assigned by the database custodians. The data stored in this repository can be used to reproduce the main analyses (models, predictions, figures). **Use in other works or publications requires Governing Board approval.** |
| **How to obtain access** | Data can be accessed through a request to the Governing Board, following [Article 5 of the EVA rules](https://euroveg.org/download/eva-rules.pdf). More info: <https://euroveg.org/eva-database/obtaining-data> | Through this repository ([`input`](data/input)). **To use these data elsewhere, a data request must be submitted to the Governing Board**. |
| **License / Terms of Use** | Depending on the availability regime assigned by the database custodians and EVA/ReSurveyEurope Governing Board. | Derived data follow the same access constraints. Metadata and code in this repository are licensed under [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/). |
| **Data availability statement** | Not publicly accessible due to third-party ownership. | All data-processing scripts and data that are essential to reproduce the analyses are publicly available in this repository. |

(\*) These values refer to the raw data released for project \# 222 (<https://doi.org/10.58060/250x-we61>)

------------------------------------------------------------------------

## 4. Methods and Processing

|   | **Raw data (\*)** | **Data in the repository** |
|-----------------|----------------------|----------------------------------|
| **Data collection methods** | Records of the abundance and/or occurrence of plant species found in vegetation plots collected in the field. | Not available (data are elaborated based on the raw data). |
| **Inclusion / exclusion criteria** | Includes different vegetation types, plot sizes, quality levels, and time periods available in the raw data. Eligibility of databases to be included in EVA and ReSurveyEurope is regulated by the Governing Board. | Vegetation plots with full lists of vascular plant species categorized as forest, grassland, scrub, and wetland, with plot size 1–1000 m², valid geographic coordinates, and sampled between 1945–2023. Detailed selection criteria are in the main manuscript. |
| **Data harmonization** | Managed by the EVA and ReSurveyEurope curators (species abundances and nomenclature). | Same as 'raw data'. |
| **Data processing (main) steps** | Managed by EVA and ReSurveyEurope curators. | 1\. Application of inclusion/exclusion criteria to the raw data. <br> 2. Calculation of species richness (number of unique vascular plant species) for each plot using the species list data from the raw data. |
| **Quality control** | Conducted by data providers and EVA/ReSurveyEurope curators. | Conducted by Gabriele Midolo with help from database custodians. Included coordinate plausibility checks (manual and automated), species richness plausibility checks (manual), and duplicate removal within and between EVA and ReSurveyEurope (automated). |
| **Processing code** | Not available. | All R scripts documenting processing steps and quality control are stored in this repository (`src/raw_data_processing`). |
| **Software environment** | Varies by data provider. | R version 4.4.2. R packages for data processing: `tidyverse`, `sf`, `terra`. |
| **Reproducibility** | Not reproducible without subset definition. | Fully reproducible for EVA-authorized users using the R code stored in this repository. |

(\*) These values refer to the raw data released for project \# 222 (<https://doi.org/10.58060/250x-we61>)

------------------------------------------------------------------------
