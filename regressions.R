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
  f <- dep_measure ~ ia_measure + log_market_cap  + book_to_market + 
    leverage + roa + tobin_q|year + sic_code|0|year+sic_code
  f[[2]] <- sym(dep_meas)
  f[[3]][[2]][[2]][[2]][[2]][[2]][[2]][[2]][[2]] <- sym(ia_meas)
  felm(f, data = df)
}

baseline_names <- mutate(baseline_regression,
                         data_named = pmap(list(data, ia_measure_name, dep_measure_name), rnm),
                         model = pmap(list(data_named, ia_measure_name, dep_measure_name), model_function))

stargazer(baseline_names$model, title = "Baseline regression results (H1)", out = "Baseline_results.html")

models_list <- models %>% 
  mutate(models_list = map2(data, models, list))


lm_model <- felm(foreign_inst_percentage ~ pin_dy_adj + log_market_cap  + 
                     leverage + roa + tobin_q, data = baseline_regression_raw)


lm_model2 <- felm(foreign_inst_percentage ~ pin_dy + log_market_cap  + 
                   leverage + roa + tobin_q, data = baseline_regression_raw)

stargazer(lm_model, lm_model2, title = "Baseline regression results (H1)", out = ".html")


## difference-in-differences regressions (did)

did_regression_raw <- read_rds("data/did_regression_raw.rds")

did_regression <- did_regression_raw %>% 
  mutate(year = year(event_date),
         log_market_cap = log(market_cap)) # log_market_cap = mean(log_market_cap, win_e.g. = [-15;-3]) or 
                                           # log_market_cap = log(mean(market_cap), win_e.g. = [-15;-3])

did_model1 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + permno|0|year+permno, data = did_regression)

did_model2 <- felm(domestic_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + permno|0|year+permno, data = did_regression)

did_model3 <- felm(inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + permno|0|year+permno, data = did_regression)

did_list <- list(did_model1, did_model2, did_model3)

stargazer(did_list, title = "Difference-in-differences regression results (H2)", out = "DiD H2 results.html")

library(ggplot2)
data <- did_regression
fpool <- as.factor(utown$pool)
futown <- as.factor(utown$utown)
event_date_year <- as.factor(data$year)
# treated <- as.factor(data$treated)
# after <- as.factor(data$after)
plot <- ggplot(data = data) + 
  geom_point(mapping = aes(x = treated*after, y = foreign_inst_percentage, 
                           color = treated))
plot
