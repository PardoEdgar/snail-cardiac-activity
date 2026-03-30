---
title: "Extracting HRV metrics from behavioral states"
author: "E. Alejandro Pardo-Sarmiento"
date: "2025-07-09"
output: html_document
---

library(tidyverse)
library(writexl)
library(RHRV)

path <- "/all_valleys_complete.csv"
all_valleys_complete <- read_csv(path)

all_valleys_complete_retracted <- all_valleys_complete %>% 
  dplyr::filter(state == "Retracted")

all_valleys_complete_moving <- all_valleys_complete %>% dplyr::filter(state == "Moving")

extract_HRV_metrics_retracted <- function(hrv_obj, id) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]]  
     RR_ms <- hrv_obj$Beat 
    data.frame(
      ID = id,
      MeanHR_retracted= mean(hrv_obj$HR, na.rm = TRUE),
      SDNN_retracted= ta$SDNN,
      RMSSD_retracted= ta$rMSSD,
      RRms_retracted= mean(RR_ms$RR), 
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = id,
      MeanHR_retracted= NA,
      SDNN_retracted= NA,
      RMSSD_retracted= NA,
      RRms_retracted= NA,
      stringsAsFactors = FALSE
    )
  }
}

extract_metrics_snail_moving<- function(hrv_obj, id) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]] 
     RR_ms <- hrv_obj$Beat 
     data.frame(
      ID = id,
      MeanHR_moving= mean(hrv_obj$HR, na.rm = TRUE),
      SDNN_moving= ta$SDNN,
      RMSSD_moving= ta$rMSSD,
      RRms_moving= mean(RR_ms$RR), 
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = id,
      MeanHR_moving= NA,
      SDNN_moving= NA,
      RMSSD_moving= NA,
      RRms_moving= NA,
      stringsAsFactors = FALSE
    )
  }
}

grouped <- all_valleys_complete_retracted %>% group_by(ID)
data_by_snail<- grouped %>% group_split()
Real_IDs <- grouped %>% group_keys() %>% pull(1)
Results_HRV_retracted<- lapply(data_by_snail, function(df_snail) {
  HRV <- CreateHRVData()
  HRV <- LoadBeatVector(HRV, df_snail$time)
  HRV <- BuildNIHR(HRV)
  HRV <- InterpolateNIHR(HRV)
  HRV <- CreateTimeAnalysis(HRV, size = 80, interval = 7.8125)
  return(HRV)
})
names(Results_HRV_ret) <- as.character(Real_IDs)
HRV_metrics_retracted <- imap_dfr(Results_HRV_ret, ~ extract_HRV_metrics_retracted(.x, .y))

grouped <- all_valleys_complete_moving %>% group_by(ID)
data_by_snail<- grouped %>% group_split()
Real_IDs <- grouped %>% group_keys() %>% pull(1)
Results_HRV_moving<- lapply(data_by_snail, function(df_snail) {
  HRV <- CreateHRVData()
  HRV <- LoadBeatVector(HRV, df_snail$time)
  HRV <- BuildNIHR(HRV)
  HRV <- InterpolateNIHR(HRV)
  HRV <- CreateTimeAnalysis(HRV, size = 80, interval = 7.8125)
  return(HRV)
})
names(Results_HRV_moving) <- as.character(Real_IDs)
HRV_metrics_moving <- imap_dfr(Results_HRV_moving, ~ extract_metrics_snail_moving(.x, .y))

extract_RR_snail_retracted <- function(hrv_obj, id) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]]  
    RR_ms <- hrv_obj$Beat
    data.frame(
      ID = id,
      RRms_retracted= (RR_ms$RR),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = id,
      RRms_retracted= NA,
      stringsAsFactors = FALSE
    )
  }
}

RR_retracted <- imap_dfr(Results_HRV_ret, ~ extract_RR_snail_retracted(.x, .y))

extract_RR_snail_moving<- function(hrv_obj, id) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]]  
    RR_ms <- hrv_obj$Beat
    data.frame(
      ID = id,
      RRms_moving= (RR_ms$RR),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = id,
      RRms_moving= NA,
      stringsAsFactors = FALSE
    )
  }
}

RR_moving <- imap_dfr(Results_HRV_mov, ~ extract_RR_snail_moving(.x, .y))

pNN100 <- function(rr, x = 100) {
  diff_rr <- abs(diff(rr))   # diferencias sucesivas
  nnx <- sum(diff_rr > x)    # número de diferencias > x ms
  pnnx <- nnx / length(diff_rr) * 100
  return(pnnx)
}

pnn100_retracted <- RR_retracted %>%
  group_by(ID) %>%
  summarise(pNN100_retracted= pNN100(RRms_retracted, x = 100))

pnn100_moving <- RR_moving %>%
  group_by(ID) %>%
  summarise(pNN100_moving= pNN100(RRms_moving, x = 100)) 

pnn100_all <- left_join(pnn100_retracted, pnn100_moving, by = "ID")

HRV_metrics_all <- HRV_metrics_moving %>%
  left_join(HRV_metrics_retracted, by = "ID") %>%
  left_join(pnn100_all, by = "ID")
View(HRV_metrics_all)
write_csv(HRV_all_metrics, "HRV_all_metrics.csv")

