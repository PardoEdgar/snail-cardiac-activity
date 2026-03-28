# snail-cardiac-activity

This repository contains all data and code used in the study:

"Cardiovascular adjustments during experimentally induced retraction and locomotion in Cornu aspersum"

## Contents
- data/: raw and processed datasets
- scripts/: R scripts for data extraction and analysis
- imagej/: ImageJ macros for pixel intensity calculation and stripe detection

## Reproducibility
All analyses were conducted in R. Scripts are provided in sequential order.

1. 01_heart_pixel_data_obtention.imj
2. 01_Optocardiography.R
3. 02_Extracting_HRV_metrics.R
4. 03_HR_HRV_vs_behavioral_states_analyses.R
5. 02_speed_data_obtention.imj
6. 04_Speed_data_curation.R
7. 05_Speed_vs_HR_analyses.R
8. 06_Body_size_vs_HR_analyses.R

   
## Requirements
- R (version 4.4.2)
- Required packages: tidyverse, ggplot2, etc.
- Fiji-ImageJ
- Required plugins: Java, CSBDeep, TensorFlow, TrackMate, StarDist 2D, TrackMate-StarDist 2D
  
## Data Availability
All data and code required to reproduce the analyses are included in this repository.

## Author
[E. Alejandro Pardo-Sarmiento]

## License
Code is licensed under the MIT License.
Data and figures are licensed under CC BY 4.0.
