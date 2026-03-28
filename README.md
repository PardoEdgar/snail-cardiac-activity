# snail-cardiac-activity

This repository contains all data and code used in the study:

"Cardiovascular adjustments during experimentally induced retraction and locomotion in Cornu aspersum"

## Contents
- data/: raw and processed datasets
- scripts/: R scripts for data extraction and analysis
- imagej/: ImageJ macros for pixel intensity calculation and stripe detection

## Reproducibility
All analyses were conducted in R. Scripts are provided in sequential order.

1. 01_data_cleaning.R  
2. 02_analysis.R  
3. 03_figures.R
   
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
