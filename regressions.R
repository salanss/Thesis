library(tidyverse)
library(readr)
library(lubridate)
library(lfe)

# baseline regressions

baseline_regression_raw <- read_rds("data/baseline_regression_raw.rds")

reg_funktio <- function(data_frame, ia_measure, dep_measure) {
  filtered <- data_frame %>% 
    filter(!(is.na(ia_measure)))
    lm_model <- lm(dep_measure ~ ia_measure, data = filtered)
    coefficients(lm_model)
}

filtered <- baseline_regression_raw %>% 
  filter(!(is.na(pin_dy_adj)))

lm_model <- felm(foreign_inst_percentage ~ pin_dy_adj|year|0|year, data = filtered)

summary(lm_model)

reg_funktio(baseline_regression_raw, pin_dy, foreign_inst_percentage)
