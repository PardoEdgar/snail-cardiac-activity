---
title: "Speed_analysis"
author: "E. Alejandro Pardo"
date: "2025-07-10"
output: html_document
---

library(tidyverse)
library(writexl)
library(stringr)
library(purrr)
library(lme4)
library(scales) 
library(lmerTest)
library(MuMIn)

path <- "/Valleys_speed_data.csv"
Valleys_speed_data <- read_csv(path)

path_2 <- "/Snail_mass_speed.csv"
Snail_mass_speed <- read_csv(path_2)

folder_path_speed <- "/Spot_tracking_results"
files <-  list.files(path = folder_path_speed, pattern = "*spots*", full.names = TRUE)
read_and_label <- function(file) {
  df <- read.csv(file)
  df <- df[-c(1:3), ]
  df <- df[, 1:9]
  df$ID <- as.numeric(df$ID)
  df$TRACK_ID <- as.numeric(df$TRACK_ID)
  df$QUALITY <- as.numeric(df$QUALITY)
  df$POSITION_X <- as.numeric(df$POSITION_X)
  df$POSITION_Y <- as.numeric(df$POSITION_Y)
  df$POSITION_Z <- as.numeric(df$POSITION_Z)
  df$POSITION_T <- as.numeric(df$POSITION_T)
  df$FRAME <- as.numeric(df$FRAME)
  df$source_file <- basename(file)  # Add filename column
  return(df)
  
  return(df)
}

data_list <- lapply(files, read_and_label)
Speed_results <- bind_rows(data_list)

Speed_results <- Speed_results %>%
  mutate(Section = as.numeric(str_extract(source_file, "(?<=Speed_)\\d+")), Snail = as.numeric(str_extract(source_file, "(?<=Mov_)\\d+")))

ranges <- Speed_results %>%
  group_by(Snail, Section, TRACK_ID) %>%
  summarise(
    start_frame = min(FRAME, na.rm = TRUE),
    end_frame = max(FRAME, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Snail, Section, start_frame)

filtered_ranges <- ranges %>%
  inner_join(ranges, by = c("Snail", "Section"), suffix = c("_a", "_b")) 

filtered_ranges <- filtered_ranges %>%   dplyr::filter(
    TRACK_ID_a != TRACK_ID_b,
    start_frame_a >= start_frame_b,
    end_frame_a <= end_frame_b
  ) %>%
   distinct(Snail, Section, TRACK_ID_remove = TRACK_ID_a)

no_contained_ranges <- ranges %>%
  anti_join(filtered_ranges, by = c("Snail", "Section", "TRACK_ID" = "TRACK_ID_remove"))

adjusted_ranges <- no_contained_ranges %>%
  group_by(Snail, Section) %>%
  arrange(start_frame) %>%
  mutate(
    previous_end_frame = lag(end_frame, default = -Inf),
    start_adjusted_frame = pmax(start_frame, previous_end_frame + 1)
  ) %>%
  dplyr::filter(start_adjusted_frame <= end_frame) %>%
  select(Snail, Section, TRACK_ID, start_frame = start_adjusted_frame, end_frame) %>%
  ungroup()

filtered_data <- Speed_results %>%
  inner_join(adjusted_ranges, by = c("Snail", "Section", "TRACK_ID")) %>%
  dplyr::filter(FRAME >= start_frame & FRAME <= end_frame)

All_speed_distances_results <- filtered_data %>%
  arrange(Snail, Section, TRACK_ID, FRAME) %>%
  group_by(Snail, Section, TRACK_ID) %>%
  mutate(
    distance = (POSITION_X - lag(POSITION_X)),
    speed = distance / 0.0333, 
  distance = replace_na(distance, 0)
  ) %>%
  ungroup()

All_speed_distances_results <- All_speed_distances_results %>%
  mutate(
    time = (FRAME * 0.0333) + ( (Section - 1) * 20 )
  )



instantaneous_heart_rate <- Valleys_speed_data %>% 
  arrange(ID, time) %>%
  group_by(ID) %>%
  mutate(
    interval = time - lag(time),
    frecuencia_inst = 60 / interval
  ) %>%
  ungroup()  %>% rename( "Snail" =ID)

`heart_rate_5s_(BPM)`<- Valleys_speed_data %>%
  arrange(ID, time) %>%
  group_by(ID) %>%
  mutate(NN = time - lag(time)) %>%
  mutate(time_5s = floor(time / 5) * 5) %>%
  group_by(ID, time_5s) %>%
  summarise(
    NNmean = mean(NN, na.rm = TRUE),
    `heart_rate_5s_(BPM)` = 60 / NNmean,              # tu enfoque
    beats_in_window = n(),
    .groups = "drop"
  )%>%
  ungroup()  %>% rename( "Snail" =ID)

speed_5s <- All_speed_distances_results %>%
  mutate(time_5s = (floor(time / 5) * 5)+5) %>%
  group_by(Snail, time_5s) %>%
  summarise(speed_5s = mean(speed, na.rm = TRUE), .groups = "drop")

distance_5s <- All_speed_distances_results %>%
  mutate(time_5s = (floor(time / 5) * 5)+ 5) %>%
  group_by(Snail, time_5s) %>%
  summarise(distance_5s = sum(distance, na.rm = TRUE), .groups = "drop")

Speed_vs_HeartRate_5_seconds<- inner_join(speed_5s, `heart_rate_5s_(BPM)`, by = c("Snail", "time_5s")) %>%
  inner_join(distance_5s, by = c("Snail", "time_5s"))

Speed_vs_HeartRate_5_seconds$`speed_(cm_min)`<- abs(Speed_vs_HeartRate_5_seconds$speed_5s * 6) 

Speed_vs_HeartRate_5_seconds <- Speed_vs_HeartRate_5_seconds %>% left_join(Snail_mass_speed, by="Snail")

write_xlsx(Speed_vs_HeartRate_5_seconds, "Speed_vs_HeartRate_5_seconds.xlsx")


shapiro.test(Speed_vs_HeartRate_5_seconds$`speed_(cm_min)`)
median(Speed_vs_HeartRate_5_seconds$`speed_(cm_min)`)
IQR(Speed_vs_HeartRate_5_seconds$`speed_(cm_min)`)
kruskal.test(`speed_(cm_min)` ~ Snail, data = Speed_vs_HeartRate_5_seconds)
range(Speed_vs_HeartRate_5_seconds$`speed_(cm_min)`)


ggplot(data = Speed_vs_HeartRate_5_seconds, aes(x = `speed_(cm_min)`, y = `heart_rate_5s_(BPM)`)) +
  geom_smooth(aes(group = as.factor(Snail)),color="black",
              size = 1.2, method = "lm", formula = y ~ x,
              se = TRUE, alpha = 0.1) +
  geom_point(aes(shape = as.factor(Snail), color= as.factor(Snail)),
             size = 1.8, stroke = 0.4, alpha = 0.7,
             color = "black", fill = "black") +
  scale_shape_manual(values = 0:25) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

modelo_mixto <- lmer(`heart_rate_5s_(BPM)` ~ (`speed_(cm_min)`) + `Total_mass(g)` + (1 | Snail), data = Speed_vs_HeartRate_5_seconds)
summary(modelo_mixto)
r.squaredGLMM(modelo_mixto)

