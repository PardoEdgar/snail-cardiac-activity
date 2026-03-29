---
title: "Optocardiography"
author: "E. Alejandro Pardo-Sarmiento"
date: "2025-07-09"
output: html_document
---
library(tidyverse)
library(pracma)
library(readxl)
library(signal)
library(writexl)

#For Retracted snails

Available_IDs <- c(1:3, 5:8, 10:13, 19:21, 25:28, 30, 32)
All_retracted_results <- list()  # Create an empty list to store results
dir.create("Graphs_", showWarnings = FALSE)
for (id in Available_IDs) {
  filename <- paste0("data/raw/retracted_", id, "_contraction_curve.csv")
  
  if (!file.exists(filename)) {
    cat("File not found for ID", id, "\n")
    next
  }
  
  data <- read_csv(filename)

  data_long <- data %>%   
    select(contains("Mean(Oval")) %>%
    mutate(t = seq(0, 20, length.out = n())) %>%
    pivot_longer(-t, names_to = "Oval", values_to = "value") %>%
    mutate(
      num_Oval = as.numeric(str_extract(Oval, "\\d+")),
      t = t + (num_Oval - 1) * 20 
    )  

  results <- data_long %>% arrange(t) %>% 
    mutate(smoothed = sgolayfilt(value, p = 5, n = 25), trend = predict(loess(smoothed ~ t, span = 0.08), newdata = data.frame(t = t))) 

  derivative <- c(NA, diff(results$smoothed)) 
  changes <- sign(derivative)
  significant_changes <- c(NA, diff(changes))

  valley_indices <- which(significant_changes == 2)
  valley_values <- results$smoothed[valley_indices]

  trend_fit <- loess(smoothed ~ t, data = results, span = 0.08)
  trend <- predict(trend_fit, newdata = data.frame(t = results$t))
  threshold <- 0.99 * trend
  valid_valleys <- valley_indices[valley_values < threshold[valley_indices]]
  min_dist <- 18
  values <- results$smoothed[valid_valleys]
  filtered_valleys <- c()

  if (length(valid_valleys) == 1) {
    filtered_valleys <- valid_valleys
  } else if (length(valid_valleys) > 1) {
    current_group <- c(valid_valleys[1])
    filtered_valleys <- c()
    
    for (i in 2:length(valid_valleys)) {
      if ((valid_valleys[i] - valid_valleys[i - 1]) <= min_dist) {
        current_group <- c(current_group, valid_valleys[i])
      } else {
        min_valley <- current_group[which.min(results$smoothed[current_group])]
        filtered_valleys <- c(filtered_valleys, min_valley)
        current_group <- c(valid_valleys[i])
      }
    }
    
    if (length(current_group) > 0) {
      min_valley <- current_group[which.min(results$smoothed[current_group])]
      filtered_valleys <- c(filtered_valleys, min_valley)
    }
  }

  local_valley_threshold <- trend[filtered_valleys]
  filtered_valley_values <- results$smoothed[filtered_valleys]
  valley_trend_filter <- filtered_valley_values < local_valley_threshold
  filtered_valleys <- filtered_valleys[valley_trend_filter]

  if (length(filtered_valleys) > 0) {
    final_valleys <- tibble(
      time = results$t[filtered_valleys],
      value = results$smoothed[filtered_valleys]
    )
  } else {
    final_valleys <- NULL
  }

  derivative <- c(NA, diff(results$smoothed))
  changes <- sign(derivative)
  significant_changes <- c(NA, diff(changes))

  # Indices where it changes from positive to negative (local maxima)
  peak_indices <- which(significant_changes == -2)
  peak_values <- results$smoothed[peak_indices]

  trend_fit <- loess(smoothed ~ t, data = results, span = 0.08)
  trend <- predict(trend_fit, newdata = data.frame(t = results$t))

  threshold <- 1.01 * trend
  local_threshold <- threshold[peak_indices]

  valid_peaks <- peak_indices[peak_values > local_threshold]
  min_dist <- 18
  values <- results$smoothed[valid_peaks]

  filtered_peaks <- c()

  if (length(valid_peaks) == 1) {
    filtered_peaks <- valid_peaks
  } else if (length(valid_peaks) > 1) {
    current_group <- c(valid_peaks[1])
    filtered_peaks <- c()
    
    for (i in 2:length(valid_peaks)) {
      if ((valid_peaks[i] - valid_peaks[i - 1]) <= min_dist) {
        current_group <- c(current_group, valid_peaks[i])
      } else {
        max_peak <- current_group[which.max(results$smoothed[current_group])]
        filtered_peaks <- c(filtered_peaks, max_peak)
        current_group <- c(valid_peaks[i])
      }
    }
    
    if (length(current_group) > 0) {
      max_peak <- current_group[which.max(results$smoothed[current_group])]
      filtered_peaks <- c(filtered_peaks, max_peak)
    }
  }

  local_threshold_filtered <- trend[filtered_peaks]
  filtered_values <- results$smoothed[filtered_peaks]
  trend_filter <- filtered_values > local_threshold_filtered
  filtered_peaks <- filtered_peaks[trend_filter]

  if (length(filtered_peaks) > 0) {
    final_peaks <- tibble(
      time = results$t[filtered_peaks],
      value = results$smoothed[filtered_peaks]
    )
  } else {
    final_peaks <- NULL
  }

  data_smoothed <- bind_rows(results)

  cat("Processed ID", id, "\n")

  breaks <- seq(0, 80, by = 20)
  data_smoothed$interval_20s <- cut(
    data_smoothed$t,
    breaks = breaks,
    right = TRUE,
    include.lowest = TRUE
  )

  final_valleys$interval_20s <- cut(
    final_valleys$time,
    breaks = breaks,
    right = TRUE,
    include.lowest = TRUE
  )

  final_peaks$interval_20s <- cut(
    final_peaks$time,
    breaks = breaks,
    right = TRUE,
    include.lowest = TRUE
  )

  plot_individual <- ggplot(data_smoothed, aes(x = t)) +
    geom_line(aes(y = value, color = "Raw signal"), size = 0.8) +
    geom_line(aes(y = smoothed, color = "Smoothed signal"), size = 0.9) +
    geom_line(aes(y = trend, color = "Trend", linetype = "Trend"), size = 1) +
    geom_point(data = final_valleys, aes(x = time, y = value, color = "Valleys", shape = "Valleys"), size = 2) +
    geom_point(data = final_peaks, aes(x = time, y = value, color = "Peaks", shape = "Peaks"), size = 2) +

    labs(title = paste("Signals and detected peaks - ID", id),
         x = "Time (s)", y = "Intensity", color = "", linetype = "", shape = "") +

    facet_wrap(~interval_20s, scales = "free_x") +

    scale_color_manual(values = c(
      "Raw signal" = "gray70",
      "Smoothed signal" = "blue",
      "Trend" = "green",
      "Valleys" = "orange",
      "Peaks" = "red"
    )) +
    scale_linetype_manual(values = c("Trend" = "dashed")) +
    scale_shape_manual(values = c("Valleys" = 16, "Peaks" = 16)) +

    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
      axis.title = element_text(size = 14, color = "black"),
      axis.text = element_text(size = 12, color = "black"),
      strip.text = element_text(color = "black"),
      legend.text = element_text(color = "black")
    )

  ggsave(filename = paste0("Graphs_Behavioral_State", id, ".pdf"),
         plot = plot_individual, width = 12, height = 8, dpi = 300)

  All_retracted_results[[as.character(id)]] <- list(
    data_suavizado = data_smoothed %>% mutate(ID = id),
    Final_valleys = final_valleys %>% mutate(ID = id),
    Final_peaks = final_peaks %>% mutate(ID = id)
  )
}

Smothed_data_all <- map_dfr(All_retracted_results, "data_suavizado")
all_retracted_valleys <- map_dfr(All_retracted_results, "Final_valleys")
picos_all_rep <- map_dfr(All_retracted_results, "Final_peaks")

#For Moving snails
Available_IDs <- c(1:2, 5:8, 11:13, 19:20)
All_movement_results <- list()  # Create an empty list to store results
dir.create("Graphs_ID_free_moving", showWarnings = FALSE)
for (id in Available_IDs) {
  filename <- paste0("data/raw/moving_", id, "_contraction_curve.csv")
  
  if (!file.exists(filename)) {
    cat("File not found for ID", id, "\n")
    next
  }
  
  data <- read_csv(filename)

  data_long <- data %>%   
    select(contains("Mean(Oval")) %>%
    mutate(t = seq(0, 20, length.out = n())) %>%
    pivot_longer(-t, names_to = "Oval", values_to = "value") %>%
    mutate(
      num_Oval = as.numeric(str_extract(Oval, "\\d+")),
      t = t + (num_Oval - 1) * 20 
    )  

  results <- data_long %>% arrange(t) %>% 
    mutate(smoothed = sgolayfilt(value, p = 5, n = 25), trend = predict(loess(smoothed ~ t, span = 0.08), newdata = data.frame(t = t))) 

  derivative <- c(NA, diff(results$smoothed))  # Smoothing and derivative
  changes <- sign(derivative)
  significant_changes <- c(NA, diff(changes))

  # Indices where it changes from negative to positive (local minima)
  valley_indices <- which(significant_changes == 2)
  valley_values <- results$smoothed[valley_indices]

  trend_fit <- loess(smoothed ~ t, data = results, span = 0.08)
  trend <- predict(trend_fit, newdata = data.frame(t = results$t))
  threshold <- 0.99 * trend
  valid_valleys <- valley_indices[valley_values < threshold[valley_indices]]
  min_dist <- 16
  values <- results$smoothed[valid_valleys]
  filtered_valleys <- c()

  if (length(valid_valleys) == 1) {
    filtered_valleys <- valid_valleys
  } else if (length(valid_valleys) > 1) {
    current_group <- c(valid_valleys[1])
    filtered_valleys <- c()
    
    for (i in 2:length(valid_valleys)) {
      if ((valid_valleys[i] - valid_valleys[i - 1]) <= min_dist) {
        current_group <- c(current_group, valid_valleys[i])
      } else {
        min_valley <- current_group[which.min(results$smoothed[current_group])]
        filtered_valleys <- c(filtered_valleys, min_valley)
        current_group <- c(valid_valleys[i])
      }
    }
    
    if (length(current_group) > 0) {
      min_valley <- current_group[which.min(results$smoothed[current_group])]
      filtered_valleys <- c(filtered_valleys, min_valley)
    }
  }

  local_valley_threshold <- trend[filtered_valleys]
  filtered_valley_values <- results$smoothed[filtered_valleys]
  valley_trend_filter <- filtered_valley_values < local_valley_threshold
  filtered_valleys <- filtered_valleys[valley_trend_filter]

  if (length(filtered_valleys) > 0) {
    final_valleys <- tibble(
      time = results$t[filtered_valleys],
      value = results$smoothed[filtered_valleys]
    )
  } else {
    final_valleys <- NULL
  }

  derivative <- c(NA, diff(results$smoothed))
  changes <- sign(derivative)
  significant_changes <- c(NA, diff(changes))

  # Indices where it changes from positive to negative (local maxima)
  peak_indices <- which(significant_changes == -2)
  peak_values <- results$smoothed[peak_indices]

  trend_fit <- loess(smoothed ~ t, data = results, span = 0.08)
  trend <- predict(trend_fit, newdata = data.frame(t = results$t))

  threshold <- 1.01* trend
  local_threshold <- threshold[peak_indices]

  valid_peaks <- peak_indices[peak_values > local_threshold]
  min_dist <- 17
  values <- results$smoothed[valid_peaks]

  filtered_peaks <- c()

  if (length(valid_peaks) == 1) {
    filtered_peaks <- valid_peaks
  } else if (length(valid_peaks) > 1) {
    current_group <- c(valid_peaks[1])
    filtered_peaks <- c()
    
    for (i in 2:length(valid_peaks)) {
      if ((valid_peaks[i] - valid_peaks[i - 1]) <= min_dist) {
        current_group <- c(current_group, valid_peaks[i])
      } else {
        max_peak <- current_group[which.max(results$smoothed[current_group])]
        filtered_peaks <- c(filtered_peaks, max_peak)
        current_group <- c(valid_peaks[i])
      }
    }
    
    if (length(current_group) > 0) {
      max_peak <- current_group[which.max(results$smoothed[current_group])]
      filtered_peaks <- c(filtered_peaks, max_peak)
    }
  }

  local_threshold_filtered <- trend[filtered_peaks]
  filtered_values <- results$smoothed[filtered_peaks]
  trend_filter <- filtered_values > local_threshold_filtered
  filtered_peaks <- filtered_peaks[trend_filter]

  if (length(filtered_peaks) > 0) {
    final_peaks <- tibble(
      time = results$t[filtered_peaks],
      value = results$smoothed[filtered_peaks]
    )
  } else {
    final_peaks <- NULL
  }

  data_smoothed <- bind_rows(results)

  cat("Processed ID", id, "\n")

  breaks <- seq(0, 80, by = 20)
  data_smoothed$interval_20s <- cut(
    data_smoothed$t,
    breaks = breaks,
    right = TRUE,
    include.lowest = TRUE
  )

  final_valleys$interval_20s <- cut(
    final_valleys$time,
    breaks = breaks,
    right = TRUE,
    include.lowest = TRUE
  )

  final_peaks$interval_20s <- cut(
    final_peaks$time,
    breaks = breaks,
    right = TRUE,
    include.lowest = TRUE
  )

  plot_individual <- ggplot(data_smoothed, aes(x = t)) +
    geom_line(aes(y = value, color = "Raw signal"), size = 0.8) +
    geom_line(aes(y = smoothed, color = "Smoothed signal"), size = 0.9) +
    geom_line(aes(y = trend, color = "Trend", linetype = "Trend"), size = 1) +
    geom_point(data = final_valleys, aes(x = time, y = value, color = "Valleys", shape = "Valleys"), size = 2) +
    geom_point(data = final_peaks, aes(x = time, y = value, color = "Peaks", shape = "Peaks"), size = 2) +

    labs(title = paste("Signals and detected peaks - ID", id),
         x = "Time (s)", y = "Intensity", color = "", linetype = "", shape = "") +

    facet_wrap(~interval_20s, scales = "free_x") +

    scale_color_manual(values = c(
      "Raw signal" = "gray70",
      "Smoothed signal" = "blue",
      "Trend" = "green",
      "Valleys" = "orange",
      "Peaks" = "red"
    )) +
    scale_linetype_manual(values = c("Trend" = "dashed")) +
    scale_shape_manual(values = c("Valleys" = 16, "Peaks" = 16)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
      axis.title = element_text(size = 14, color = "black"),
      axis.text = element_text(size = 12, color = "black"),
      strip.text = element_text(color = "black"),
      legend.text = element_text(color = "black")
    )

  ggsave(filename = paste0("Graphs_ID_free_moving/free_moving", id, ".pdf"),
         plot = plot_individual, width = 12, height = 8, dpi = 300)

  All_movement_results[[as.character(id)]] <- list(
    data_suavizado = data_smoothed %>% mutate(ID = id),
    Final_valleys = final_valleys %>% mutate(ID = id),
    Final_peaks = final_peaks %>% mutate(ID = id)
  )
}

Smothed_data_all <- map_dfr(All_movement_results, "data_suavizado")
all_moving_valleys<- map_dfr(All_movement_results, "Final_valleys")
peaks_all_mov <- map_dfr(All_movement_results, "Final_peaks")

all_retracted_valleys <- all_retracted_valleys %>% 
  mutate(estado = "Retracted")
all_moving_valleys <- all_moving_valleys %>% 
  mutate(estado = "Moving")
all_valleys_complete <- bind_rows(all_retracted_valleys, all_moving_valleys)
write_xlsx(all_valleys_complete, "all_valleys_complete.xlsx")

