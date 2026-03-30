# snail-cardiac-activity

This repository contains all data and code used in the study:

**"Cardiovascular adjustments during experimentally induced retraction and locomotion in the invasive terrestrial snail *Cornu aspersum* (Müller, 1774)"**

---

## Overview

This repository provides a reproducible workflow to quantify cardiac activity and locomotion speed in the land snail *Cornu aspersum*. 

The approach combines optocardiography based on pixel intensity changes, automated stripe tracking on a snail treadmill using TrackMate, and deep learning-based segmentation with StarDist 2D. 

Signal processing and heart rate variability (HRV) analyses are implemented in R to investigate the relationship between physiological adjustments and experimentally induced behavioral states, such as retraction and locomotion.

---

## Contents

- `data/`: Raw and processed datasets  
- `scripts/`: R scripts for data extraction and analysis  
- `imagej/`: ImageJ macros for pixel intensity extraction and stripe detection  
- `Figures/`: Optocardiograms and Experimental setup images for Optocardiography
---

## Reproducibility

All analyses were conducted in R. Scripts are provided in sequential order:

1. `01_heart_pixel_data_obtention.imj`  
2. `01_Optocardiography.R`  
3. `02_Extracting_HRV_metrics.R`  
4. `03_HR_HRV_vs_behavioral_states_analyses.R`  
5. `02_Macro_Trackmate_stripe_tracking.ijm`  
6. `04_speed_curation_and_speed_vs_heartrate_analyses.R`  
7. `05_Body_size_vs_HR_analyses.R`  

---

## Requirements

### R
- R (version 4.4.2)
- Required packages:
  - tidyverse  
  - pracma  
  - gridExtra  
  - readxl  
  - signal  
  - writexl  
  - RHRV
  - lme4
  - MuMIn
  - scales
  - lmodel2

### Fiji / ImageJ
- Fiji-ImageJ
- Required plugins:
  - Java  
  - CSBDeep  
  - TensorFlow  
  - TrackMate  
  - StarDist 2D  
  - TrackMate-StarDist 2D  

---

## Data Availability

All data and code required to reproduce the analyses are included in this repository.

---

## Author

**E. Alejandro Pardo-Sarmiento**

---

## License

- Code: MIT License  
- Data and figures: CC BY 4.0  
