---
title: "Extracting HRV metrics from behavioral states"
author: "E. Alejandro Pardo-Sarmiento"
date: "2025-07-09"
output: html_document
---

library(tidyverse)
library(readxl)
library(writexl)
library(RHRV)

path <- "all_valleys_complete.xlsx"
all_valleys_complete <- read_xlsx(path)

all_valleys_complete_retracted <- all_valleys_complete %>% 
  dplyr::filter(state == "Retracted")

all_valleys_complete_moving <- all_valleys_complete %>% dplyr::filter(state == "Moving")

extract_HRV_metrics_retracted <- function(hrv_obj, id) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]]  
     RR_ms <- hrv_obj$Beat 
    data.frame(
      ID = id,
      MeanHR_ret = mean(hrv_obj$HR, na.rm = TRUE),
      SDNN_ret = ta$SDNN,
      RMSSD_ret = ta$rMSSD,
      RRms_ret = mean(RR_ms$RR), 
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = id,
      MeanHR_ret = NA,
      SDNN_ret = NA,
      RMSSD_ret = NA,
      RRms_ret = NA,
      stringsAsFactors = FALSE
    )
  }
}

extract_metrics_snail_mov <- function(hrv_obj, id) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]] 
     RR_ms <- hrv_obj$Beat 
     data.frame(
      ID = id,
      MeanHR_mov = mean(hrv_obj$HR, na.rm = TRUE),
      SDNN_mov = ta$SDNN,
      RMSSD_mov = ta$rMSSD,
      RRms_mov = mean(RR_ms$RR), 
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = id,
      MeanHR_mov = NA,
      SDNN_mov = NA,
      RMSSD_mov = NA,
      RRms_mov = NA,
      stringsAsFactors = FALSE
    )
  }
}

grouped <- all_valleys_complete_retracted %>% group_by(ID)
data_by_snail<- grouped %>% group_split()
Real_IDs <- grouped %>% group_keys() %>% pull(1)
Results_HRV_ret <- lapply(data_by_snail, function(df_snail) {
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
Results_HRV_mov <- lapply(data_by_snail, function(df_snail) {
  HRV <- CreateHRVData()
  HRV <- LoadBeatVector(HRV, df_snail$time)
  HRV <- BuildNIHR(HRV)
  HRV <- InterpolateNIHR(HRV)
  HRV <- CreateTimeAnalysis(HRV, size = 80, interval = 7.8125)
  return(HRV)
})
names(Results_HRV_mov) <- as.character(Real_IDs)
HRV_metrics_moving <- imap_dfr(Results_HRV_mov, ~ extract_metrics_snail_mov(.x, .y))

extract_RR_snail_retracted <- function(hrv_obj, id) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]]  
    RR_ms <- hrv_obj$Beat
    data.frame(
      ID = id,
      RRms_ret = (RR_ms$RR),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = id,
      RRms_ret = NA,
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
      RRms_mov = (RR_ms$RR),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = id,
      RRms_mov = NA,
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
  summarise(pNN100_ret = pNN100(RRms_ret, x = 100))

pnn100_moving <- RR_moving %>%
  group_by(ID) %>%
  summarise(pNN100_mov = pNN100(RRms_mov, x = 100)) 

pnn100_all <- left_join(pnn100_retracted, pnn100_moving, by = "ID")

HRV_metrics_all <- HRV_metrics_moving %>%
  left_join(HRV_metrics_retracted, by = "ID") %>%
  left_join(pnn100_all, by = "ID")

write_xlsx(HRV_all_metrics, "HRV_all_metrics.xlsx")

