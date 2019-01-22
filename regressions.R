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
  gather(ia_measure_name, ia_measure, bid_ask_spread:mia) %>% 
  gather(dep_measure_name, dep_measure, foreign_inst_percentage:domestic_breadth) %>% 
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

stargazer(baseline_names$model, column.labels =c("Foreign ownership",
                                                 "Foreign breadth", "Domestic ownership", "Domestic breadth"),
          dep.var.labels.include = F,
          column.separate = c(8, 8, 8, 8),
          title = "Baseline regression results (H1)", out = "Baseline_results.html")

baseline_regression_summary <- baseline_regression_raw %>% 
  select(-permno, -year, -sic_code) %>% 
  as.data.frame()

stargazer(baseline_regression_summary, title = "Baseline summary statistics", out = "Baseline_summary.html")

## difference-in-differences regressions (did)

did_regression_raw <- read_rds("data/did_regression_raw.rds")

summary(did_regression_raw)

# log_market_cap = mean(log_market_cap, win_e.g. = [-15;-3]) or 
# log_market_cap = log(mean(market_cap), win_e.g. = [-15;-3])

columns_for_summarise <- c("inst_percentage", "foreign_inst_percentage", "domestic_inst_percentage", 
                           "inst_breadth", "foreign_breadth", "domestic_breadth", 
                           "market_cap", "book_to_market", "leverage", "roa", "tobin_q")

did_regression <- did_regression_raw %>% 
  filter(quarter_index %in% c(-12:12)) %>% 
  group_by(permno, event_date, treated, after) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  mutate(year = year(event_date),
         log_market_cap = log(market_cap))
  

did_model1 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + permno|0|year+permno, data = did_regression)

did_model2 <- felm(domestic_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + permno|0|year+permno, data = did_regression)

did_model3 <- felm(foreign_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + permno|0|year+permno, data = did_regression)

did_model4 <- felm(domestic_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + permno|0|year+permno, data = did_regression)

did_list <- list(did_model1, did_model2, did_model3, did_model4)

stargazer(did_list, title = "Difference-in-differences regression results (H2)", out = "DiD H2 results.html")

data <- did_regression_raw %>% 
  mutate(treated = if_else(treated == 1, "treated", "control")) %>% 
  group_by(treated, quarter_index) %>% 
  summarise(foreign_inst_percentage = mean(foreign_inst_percentage),
            foreign_breadth = mean(foreign_breadth),
            domestic_inst_percentage = mean(domestic_inst_percentage),
            domestic_breadth = mean(domestic_breadth)) %>% 
  ungroup()

ggplot(data, aes(quarter_index, domestic_breadth, group = treated, color = treated)) + 
  geom_line() +
  geom_vline(xintercept=0) +
  theme_classic()

d <- baseline_regression_raw %>% 
  group_by(year) %>% 
  summarise(inst_percentage = mean(inst_percentage),
            foreign_inst_percentage = mean(foreign_inst_percentage),
            domestic_inst_percentage = mean(domestic_inst_percentage))

ggplot(d, aes(year)) + 
  #geom_line(aes(y = inst_percentage, colour = "inst_percentage")) + 
  geom_line(aes(y = foreign_inst_percentage, colour = "foreign_inst_percentage")) +
  #geom_line(aes(y = domestic_inst_percentage, colour = "domestic_inst_percentage")) + 
  theme_classic()

       