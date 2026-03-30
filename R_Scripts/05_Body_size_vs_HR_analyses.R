---
title: "Body_mass_composition_analysis"
author: "E. Alejandro Pardo"
date: "2025-08-10"
output: html_document
---

library(dplyr)
library(lmodel2)

path_1 <- "C:/Users/jandr/OneDrive - Universidad del rosario/Heart_rate_speed_HRV/Datos/GitHub/HRV_all_metrics.csv"
HRV_all_metrics <- read_csv(path_1)
path_2 <- "C:/Users/jandr/OneDrive - Universidad del rosario/Heart_rate_speed_HRV/Datos/GitHub/Snail_mass_behavioral_states.csv"
Snail_mass_behavioral_states <- read_csv(path_2)

HRV_mass_all <- HRV_all_metrics %>%
  left_join(Snail_mass_behavioral_states, by = "Snail")

C.i. <- function(x, conf.level = 0.95, na.rm= T) {
  range_x <- range(x)
  median_x <- median(x)
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  standar_d <- sd(x)
  se <- sd_x/sqrt(n)
  cv <- sd_x/mean_x*100
  error_margin <- (qt((1+conf.level)/2, df = n - 1)) * se  
    Upper = mean_x + error_margin
    Lower = mean_x - error_margin
      return(c(range=range_x, median =median_x, mean = mean_x, lower_CI = Lower, upper_CI = Upper, standar =standar_d, cv=cv, n=n))
}

C.i.(HRV_mass_all$`Total_mass_(g)`)
C.i.(HRV_mass_all$`Dry_mass_(g)`)
C.i.(HRV_mass_all$`Shell_mass_(g)`)

Model1_mass <- lmodel2((HRV_mass_all$`Shell_mass_(g)`) ~ (HRV_mass_all$`Dry_mass_(g)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "OLS", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("Shell mass (g) = ", intercept, " + ", slope, " × Dry mass (g)")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_moving)) ~ (HRV_mass_all$`Total_mass_(g)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_moving= ", intercept, " + ", slope, " × Total mass (g) ")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_retracted)) ~ (HRV_mass_all$`Total_mass_(g)`), data=HRV_mass_all, "interval", "interval", 99)
Model1_mass
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_retracted= ", intercept, " + ", slope, " × Total_mass (g)")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_moving)) ~ (HRV_mass_all$`Shell_mass_(g)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_moving = ", intercept, " + ", slope, " × Shell mass (g)")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_retracted)) ~ (HRV_mass_all$`Shell_mass_(g)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_retracted= ", intercept, " + ", slope, " × `Shell mass(g)`")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_moving)) ~ (HRV_mass_all$`Dry_mass_(g)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_moving = ", intercept, " + ", slope, " × Dry mass (g)")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_retracted)) ~ (HRV_mass_all$`Dry_mass_(g)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_retracted = ", intercept, " + ", slope, " × Dry mass (g)")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_retracted)) ~ (HRV_mass_all$`Shell_Diameter_(mm)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_retracted = ", intercept, " + ", slope, " × Shell Diameter (mm)")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_moving)) ~ (HRV_mass_all$`Shell_Diameter_(mm)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_moving = ", intercept, " + ", slope, " × Shell Diameter (mm) ")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_retracted)) ~ (HRV_mass_all$`Shell_Height_(mm)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_retracted = ", intercept, " + ", slope, " × Shell Height (mm)")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")

Model1_mass <- lmodel2(((HRV_mass_all$MeanHR_moving)) ~ (HRV_mass_all$`Shell_Height_(mm)`), data=HRV_mass_all, "interval", "interval", 99)
rma_result<-Model1_mass$regression.results[Model1_mass$regression.results$Method == "SMA", ]
intercept <- round(rma_result$Intercept, 3)
slope <- round(rma_result$Slope, 3)
r_squared <- round(Model1_mass$rsquare, 3)
equation <- paste0("HR_moving = ", intercept, " + ", slope, " × Shell Height (mm) ")
rsq_text <- paste0("R² = ", r_squared)
cat(equation, "\n")
cat(rsq_text, "\n")
ci_lower <- round(rma_result$`P-perm (1-tailed)`, 3)
p_value <- Model1_mass$P.param
cat("P-value =", signif(p_value, 3), "\n")
