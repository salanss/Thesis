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

rnm <- function(df, ia_meas, dep_meas) {
  rnm_list <- set_names(c("ia_measure", "dep_measure"), syms(c(ia_meas, dep_meas)))
  rename(df, !!!rnm_list)
}

model_function <- function(df, ia_meas, dep_meas) {
  f <- dep_measure ~ ia_measure + log_market_cap  + leverage +
    roa + tobin_q|year + sic_code|0|year+sic_code
  f[[2]] <- sym(dep_meas)
  f[[3]][[2]][[2]][[2]][[2]][[2]][[2]][[2]] <- sym(ia_meas)
  felm(f, data = df)
}

baseline_names <- mutate(baseline_regression,
                         data_named = pmap(list(data, ia_measure_name, dep_measure_name), rnm),
                         model = pmap(list(data_named, ia_measure_name, dep_measure_name), model_function))


models <- baseline_regression %>%
  transmute(model = map(data, model_function))

models

l <- models[[1]]

stargazer(baseline_names$model, title = "Baseline regression results (H1)", out = ".html")

models_list <- models %>% 
  mutate(models_list = map2(data, models, list))


lm_model <- felm(foreign_inst_percentage ~ pin_dy_adj + log_market_cap  + 
                     leverage + roa + tobin_q, data = baseline_regression_raw)


lm_model2 <- felm(foreign_inst_percentage ~ pin_dy + log_market_cap  + 
                   leverage + roa + tobin_q, data = baseline_regression_raw)

stargazer(lm_model, lm_model2, title = "Baseline regression results (H1)", out = ".html")
