library(tidyverse)
library(readr)
library(lubridate)
library(lfe)
library(stargazer)

# baseline regressions

baseline_regression_raw <- read_rds("data/baseline_regression_raw.rds")

# gather ia_measures for single column, create nested data frames and apply functional programming for lm model ->

baseline_regression <- baseline_regression_raw %>% 
  gather(ia_measure_name, ia_measure, pin_dy:mia) %>% 
  #gather(dep_measure_name, dep_measure, inst_percentage:domestic_inst_percentage) %>% 
  select(-domestic_inst_percentage, -inst_percentage, -permno) %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  group_by(ia_measure_name) %>% 
  nest()

model_function <- function(df) {
  felm(foreign_inst_percentage ~ ia_measure + log_market_cap + book_to_market + leverage +
         roa + tobin_q|year + sic_code|0|year+sic_code, data = df)
}

models <- baseline_regression %>%
  transmute(model = map(data, model_function)) %>% 
  flatten_chr()

models

models[[1]]
  
b_fun <- function(mod){
  coefficients(mod)[[1]]
}

models %>% transmute(ia_measure_name,
                     beta = map_dbl(model, b_fun))

models_list <- models %>% 
  mutate(models_list = map2(data, models, list))

ia_measures <- baseline_regression_raw %>% 
  select(pin_dy, pin_dy_adj, psos_dy, pin_eho, pin_bhl, pin_bh, mia) %>% 
  colnames() 

reg_funktio <- function(.data, ia_measure) {
  #var <- rlang::ensym(ia_measure)
  filtered <- .data %>% 
    select("foreign_inst_percentage", "ia_measure", "log_market_cap", "book_to_market", 
           "leverage", "roa", "tobin_q", "year", "sic_code") %>% 
    filter_all(all_vars(!is.na(.))) 
  felm_model <- felm(formula(paste("foreign_inst_percentage ~ ", "ia_measure", sep = "") + "log_market_cap" + "book_to_market"
                             + "leverage" + "roa" + "tobin_q" |"year" + "sic_code"|0|"year" + "sic_code"), data = filtered)
  summary(felm_model)
  #filtered
}

baseline_regression_raw %>% 
  reg_funktio(pin_dy)

filtered <- baseline_regression_raw %>% 
  filter(!(is.na(pin_dy_adj)))

lm_model_1 <- felm(foreign_inst_percentage ~ pin_dy_adj + log_market_cap + book_to_market + 
                   leverage + roa + tobin_q |year + permno|0|year + permno, data = filtered, psdef=F)

summary(lm_model_1)

lm_model <- felm(foreign_inst_percentage ~ pin_dy_adj + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q, data = baseline_regression_raw)

lm_model

library(moderndive)
regression_points <- get_regression_points(lm_model)


library(ggplot2)

baseline_regression_raw %>% 
  filter(!is.na(pin_bhl)) %>% 
  filter(foreign_inst_percentage <= 1) %>% 
  ggplot(aes(pin_bhl, foreign_inst_percentage)) + geom_point() + facet_wrap(~ year, nc = 3)

lm_model_2 <- felm(foreign_inst_percentage ~ pin_dy + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = baseline_regression_raw, psdef = F)

lm_model_3 <- felm(foreign_inst_percentage ~ pin_bh + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = baseline_regression_raw)

lm_model_4 <- felm(foreign_inst_percentage ~ pin_bhl + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = baseline_regression_raw)

stargazer(models[[1]], models[[2]], title = "Baseline regression results (H1)", out = ".html")

summary(lm_model_1)


library(purrr)

df <- mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) #%>%
  map(summary) %>%
  map_dbl("r.squared")
