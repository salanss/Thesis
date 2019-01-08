library(tidyverse)
library(readr)
library(lubridate)
library(lfe)
library(stargazer)

# baseline regressions

baseline_regression_raw <- read_rds("data/baseline_regression_raw.rds")

reg_funktio <- function(data_frame, ia_measure, dep_measure) {
  filtered <- data_frame %>% 
    filter(!(is.na(ia_measure)))
    lm_model <- lm(dep_measure ~ ia_measure, data = filtered)
    coefficients(lm_model)
}

reg_funktio(baseline_regression_raw, pin_dy, foreign_inst_percentage)

filtered <- baseline_regression_raw %>% 
  filter(!(is.na(pin_dy_adj)))

lm_model_1 <- felm(foreign_inst_percentage ~ pin_dy_adj + log_market_cap + book_to_market + 
                   leverage + roa + tobin_q |year + permno|0|year + permno, data = filtered)

lm_model_2 <- felm(foreign_inst_percentage ~ pin_dy + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = filtered)

lm_model_3 <- felm(foreign_inst_percentage ~ pin_bh + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = filtered)

lm_model_4 <- felm(foreign_inst_percentage ~ pin_bhl + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = filtered)

stargazer(lm_model_1, lm_model_2, lm_model_3, lm_model_4, title = "Baseline regression results (H1)", out = ".html")

summary(lm_model_1)
