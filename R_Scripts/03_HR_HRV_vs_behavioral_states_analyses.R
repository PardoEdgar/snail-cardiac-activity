title: "Heart rate and HRV analysis"
author: "E. Alejandro Pardo"
date: "2025-08-10"
output: html_document
---

library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)

path <- ("/HRV_all_metrics.xlsx")
metrics_all <- read_xlsx(path)

large_metrics_all <-  metrics_all %>% 
  mutate(across(where(is.character), ~as.numeric(.))) %>%
  pivot_longer(
    cols = c(MeanHR_retracted, MeanHR_moving, SDNN_retracted, SDNN_moving, pNN100_retracted, pNN100_moving, RRms_moving, RRms_retracted),
    names_to = c(".value", "condition"),
    names_sep = "_"
  ) 

colnames(large_metrics_all)[colnames(large_metrics_all) == "ID"] <- "Snail"
large_metrics_all_filter <- large_metrics_all %>% dplyr::filter(!Snail=="5" )

MeanHR_r <- large_metrics_all %>%
  dplyr::filter(!is.na(MeanHR)) %>%
  select(Snail, condition, MeanHR) %>%
  pivot_wider(names_from = condition, values_from = MeanHR)

shapiro.test(MeanHR_r$retracted)###Normal
shapiro.test(MeanHR_r$moving)###Normal

t.test(MeanHR_r$retracted, MeanHR_r$moving, paired = TRUE)

mean(MeanHR_r$retracted)
mean(MeanHR_r$moving)

sd(MeanHR_r$retracted)
sd(MeanHR_r$moving)

RRs_analisis <- large_metrics_all %>%
  dplyr::filter(!is.na(RRms)) %>%
  select(Snail, condition, RRms) %>%
  pivot_wider(names_from = condition, values_from = RRms)

shapiro.test(RRs_analisis$retracted)###Normal
shapiro.test(RRs_analisis$moving)###noNormal

wilcox.test(RRs_analisis$retracted, RRs_analisis$moving, paired = TRUE)

median(RRs_analisis$retracted)
IQR(RRs_analisis$retracted)
median(RRs_analisis$moving)
IQR(RRs_analisis$moving)


SDNN_r <- large_metrics_all %>%
  dplyr::filter(!is.na(SDNN)) %>%
  select(Snail, condition, SDNN) %>%
  pivot_wider(names_from = condition, values_from = SDNN)

shapiro.test(SDNN_r$retracted)
shapiro.test(SDNN_r$moving)

wilcox.test(SDNN_r$retracted, SDNN_r$moving, paired = TRUE)

median(SDNN_r$retracted)
IQR(SDNN_r$retracted)

median(SDNN_r$moving)
IQR(SDNN_r$moving)

pNN100 <- large_metrics_all %>%
  dplyr::filter(!is.na(pNN100)) %>%
  select(Snail, condition, pNN100) %>%
  pivot_wider(names_from = condition, values_from = pNN100)


shapiro.test(pNN100$retracted)
shapiro.test(pNN100$moving)

wilcox.test(pNN100$retracted, pNN100$moving, paired = TRUE)

median(pNN100$retracted)
IQR(pNN100$retracted)

median(pNN100$moving)
IQR(pNN100$moving)


#without snail 5

MeanHR_r <- large_metrics_all_filter %>%
  dplyr::filter(!is.na(MeanHR)) %>%
  select(Snail, condition, MeanHR) %>%
  pivot_wider(names_from = condition, values_from = MeanHR)

shapiro.test(MeanHR_r$retracted)###Normal
shapiro.test(MeanHR_r$moving)###Normal

t.test(MeanHR_r$retracted, MeanHR_r$moving, paired = TRUE)


RRs_analisis <- large_metrics_all_filter %>%
  dplyr::filter(!is.na(RRms)) %>%
  select(Snail, condition, RRms) %>%
  pivot_wider(names_from = condition, values_from = RRms)

shapiro.test(RRs_analisis$retracted)###Normal
shapiro.test(RRs_analisis$moving)###Normal

t.test(RRs_analisis$retracted, RRs_analisis$moving, paired = TRUE)



SDNN_r <- large_metrics_all_filter %>%
  dplyr::filter(!is.na(SDNN)) %>%
  select(Snail, condition, SDNN) %>%
  pivot_wider(names_from = condition, values_from = SDNN)

shapiro.test(SDNN_r$retracted)
shapiro.test(SDNN_r$moving)

wilcox.test(SDNN_r$retracted, SDNN_r$moving, paired = TRUE)

pNN100 <- large_metrics_all_filter %>%
  dplyr::filter(!is.na(pNN100)) %>%
  select(Snail, condition, pNN100) %>%
  pivot_wider(names_from = condition, values_from = pNN100)

shapiro.test(pNN100$retracted)
shapiro.test(pNN100$moving)

wilcox.test(pNN100$retracted, pNN100$moving, paired = TRUE)


HR <- ggplot(large_metrics_all,
) +  geom_violin(aes(
  x = condition,
  y = MeanHR, fill = condition)) +  geom_boxplot(aes(
  x = condition,
  y = MeanHR, fill = condition),size =0.2, width = 0.2, outlier.shape = NA, alpha = 0.5) +
      theme_minimal(base_size = 14) +
  geom_jitter(aes(
  x = condition,
  y = MeanHR,
  group = Snail), size = 1, stroke=0, alpha=0.5, width = 0.02)+
  labs(
    x = "State",
    y = "Average heart rate (BPM)"
  ) +
  scale_x_discrete(limits = c("retracted", "moving")) +
  scale_fill_manual(values = c("retracted" = "white", "moving" = "white")) +
  scale_color_manual(values = c("retracted" = "white", "moving" = "white"))+ theme_classic()


NN_intervals <- ggplot(large_metrics_all,
) +  geom_violin(aes(
  x = condition,
  y = RRms, fill = condition)) +  geom_boxplot(aes(
  x = condition,
  y = RRms, fill = condition),size =0.2, width = 0.2, outlier.shape = NA, alpha = 0.5) +
      theme_minimal(base_size = 14) +
  geom_jitter(aes(
  x = condition,
  y = RRms,
  group = Snail), size = 1, stroke=0, alpha=0.5, width = 0.02)+
  labs(
    x = "State",
    y = "NN_intervals (ms)"
  ) +
  scale_x_discrete(limits = c("retracted", "moving")) +
  scale_fill_manual(values = c("retracted" = "white", "moving" = "white")) +
  scale_color_manual(values = c("retracted" = "white", "moving" = "white"))+ theme_classic()


SDNN <- ggplot(large_metrics_all,) +  geom_violin(aes(
  x = condition,
  y = SDNN, fill = condition)) +geom_boxplot(aes(
  x = condition,
  y = SDNN, fill = condition), width = 0.2, outlier.shape = NA, alpha = 0.5,size =0.2
  ) +
  theme_minimal(base_size = 14) + geom_jitter(aes(
  x = condition,
  y = SDNN,
  group = Snail), size = 1, stroke=0, alpha=0.5, width = 0.02) + 
  labs(
    x = "state",
    y = "SDNN (ms)"
  ) +  scale_x_discrete(limits = c("retracted", "moving")) +
  scale_fill_manual(values = c("retracted" = "white", "moving" = "white")) +
  scale_color_manual(values = c("retracted" = "white", "moving" = "white"))+ theme_classic()

pNN100 <- ggplot(large_metrics_all,
) +  geom_violin(aes(
  x = condition,
  y = pNN100, fill = condition)) + geom_boxplot(aes(
  x = condition,
  y = pNN100, fill = condition), width = 0.2, outlier.shape = NA, alpha = 0.5, size =0.2
  ) +
  theme_minimal(base_size = 14) +geom_jitter(aes(
  x = condition,
  y = pNN100,
  group = Snail), size = 1, stroke=0, alpha=0.5, width = 0.02) + 
  labs(
    x = "State",
    y = "pNN100 (%)"
  ) +
  scale_fill_manual(values = c("retracted" = "white", "moving" = "white")) +
  scale_color_manual(values = c("retracted" = "white", "moving" = "white"))+ theme_classic()


grid.arrange(HR, NN_intervals,SDNN, pNN100, ncol=2, nrow=3)
