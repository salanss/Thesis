library(tidyverse)
library(readr)
library(lubridate)
library(lfe)
library(stargazer)
library(ggplot2)

# baseline regressions

baseline_regression_raw <- read_rds("data/baseline_regression_raw.rds")

# gather ia_measures for single column, create nested data frames and apply functional programming for lm model ->

baseline_regression <- baseline_regression_raw %>% 
  gather(ia_measure_name, ia_measure, pin_dy:mia) %>% 
  gather(dep_measure_name, dep_measure, inst_percentage:domestic_inst_percentage) %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  group_by(dep_measure_name, ia_measure_name) %>% 
  nest()

model_function <- function(df) {
  felm(dep_measure ~ ia_measure + log_market_cap + book_to_market + leverage +
         roa + tobin_q|year + sic_code|0|year+sic_code, data = df)
}

models <- baseline_regression %>%
  transmute(model = map(data, model_function))

models

l <- models[[1]]
  
b_fun <- function(mod){
  coefficients(mod)[[1]]
}

models %>% transmute(ia_measure_name,
                     beta = map_dbl(model, b_fun))

stargazer(l, title = "Baseline regression results (H1)", out = ".html")

models_list <- models %>% 
  mutate(models_list = map2(data, models, list))


lm_model <- felm(foreign_inst_percentage ~ pin_dy_adj + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q, data = baseline_regression_raw)


baseline_regression_raw %>% 
  filter(!is.na(pin_bhl)) %>% 
  filter(foreign_inst_percentage <= 1) %>% 
  ggplot(aes(pin_bhl, foreign_inst_percentage)) + geom_point() + facet_wrap(~ year, nc = 3)
