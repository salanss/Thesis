library(tidyverse)
library(lubridate)
library(lfe)
library(stargazer)
library(ggplot2)
library(MatchIt)

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

stargazer(baseline_names$model, column.labels =c("Foreign ownership", "Foreign shares by institutional shares",
                                                 "Foreign breadth", "Foreign breadth2", "Domestic ownership", 
                                                 "Domestic breadth"),
          dep.var.labels.include = F,
          column.separate = c(8, 8, 8, 8, 8, 8),
          omit.stat = c("ser"),
          add.lines = list(c("Industry fixed effects", rep("Yes", times = 32)), 
                           c("Year fixed effects", rep("Yes", times = 32))),
          title = "Baseline regression results (H1)", out = "Baseline_results.html")

baseline_regression_summary <- baseline_regression_raw %>% 
  select(-permno, -year, -sic_code) %>% 
  as.data.frame()

stargazer(baseline_regression_summary, title = "Baseline summary statistics", out = "Baseline_summary.html")

baseline_development <- baseline_regression_raw %>% 
  group_by(year) %>% 
  summarise(inst_percentage = mean(inst_percentage),
            foreign_inst_percentage = mean(foreign_inst_percentage),
            domestic_inst_percentage = mean(domestic_inst_percentage)) %>% 
  ungroup()

ggplot(baseline_development, aes(year)) + 
  geom_line(aes(y = inst_percentage, colour = "inst_percentage")) + 
  geom_line(aes(y = foreign_inst_percentage, colour = "foreign_inst_percentage")) +
  geom_line(aes(y = domestic_inst_percentage, colour = "domestic_inst_percentage")) + 
  theme_classic()

## difference-in-differences regressions (did)

did_regression_raw <- read_rds("data/did_regression_raw.rds") %>% 
  mutate(log_market_cap = log(market_cap),
         year = year(event_date)) %>% 
  group_by(permno, event_date, treated) %>%
  mutate(n = n_distinct(after)) %>%
  ungroup() %>% 
  filter(n > 1) %>%  # require for every permno to have before and after value for each event
  arrange(event_date, quarter_index, treated)

columns_for_summarise <- c("inst_percentage", "foreign_inst_percentage", "domestic_inst_percentage", 
                           "foreign_shares_by_institutional_shares","inst_breadth", "foreign_breadth", 
                           "foreign_breadth2", "domestic_breadth", "market_cap", "log_market_cap", 
                           "book_to_market", "leverage", "roa", "tobin_q", "analyst_coverage")

did_regression <- did_regression_raw %>% 
  filter(quarter_index %in% c(-4:4)) %>% 
  group_by(permno, event_date, year, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, treated, after) %>% 
  mutate(sic_code = last(sic_code)) %>% 
  ungroup()

did_regression_matched_temp1 <- did_regression %>% 
  group_by(permno, event_date, treated) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  mutate(n = row_number())

events <- did_regression_raw %>% select(event_date) %>% distinct() 

filter1 <- function(df, events) {
  df %>% 
    filter(event_date == events)
}

propensity_match <- function(df) {
  did_match <- matchit(treated ~ log_market_cap + book_to_market + analyst_coverage,
          method = "nearest", data = df, ratio = 2)
  df <- match.data(did_match)
  df
}

# produces propensity scores for permno-event_date-treated combinations (based on means)

did_regression_matched_temp2 <- map(events$event_date, ~filter1(did_regression_matched_temp1, .x)) %>%  
  map_df(~propensity_match(.x))

# now to join with before and after values

did_regression_matched_raw <- did_regression_matched_temp2 %>% 
  select(permno, event_date, treated) %>% 
  left_join(did_regression_raw)

did_regression_matched <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-4:4)) %>% 
  group_by(permno, event_date, year, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, treated, after) %>% 
  mutate(sic_code = last(sic_code)) %>% 
  ungroup()  

did_regression_summary <- did_regression_matched %>% 
  select(-permno, -year, -sic_code) %>% 
  as.data.frame()

stargazer(did_regression_summary, title = "DiD summary statistics", out = "DiD_summary.html")

summary(did_regression_matched)
  
did_model1 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_matched)

did_model2 <- felm(foreign_shares_by_institutional_shares ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_matched)

did_model3 <- felm(domestic_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_matched)

did_model4 <- felm(foreign_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_matched)

did_model5 <- felm(foreign_breadth2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_matched)

did_model6 <- felm(domestic_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_matched)

did_list <- list(did_model1, did_model2, did_model3, did_model4, did_model5, did_model6)

stargazer(did_list, title = "Difference-in-differences regression results (H2)", 
          omit.stat = c("ser"), 
          add.lines = list(c("Industry fixed effects", rep("Yes", times = 6)), 
                           c("Year fixed effects", rep("Yes", times = 6))),
          out = "DiD H2 results.html")

parallel_trend1 <- did_regression_matched_raw %>% 
  mutate(treated = if_else(treated == 1, "treated", "control")) %>% 
  group_by(treated, quarter_index) %>% 
  summarise(foreign_inst_percentage = median(foreign_inst_percentage),
            foreign_shares_by_institutional_shares = median(foreign_shares_by_institutional_shares),
            foreign_breadth = median(foreign_breadth),
            foreign_breadth2 = median(foreign_breadth2),
            domestic_inst_percentage = median(domestic_inst_percentage),
            domestic_breadth = median(domestic_breadth)) %>% 
  ungroup()

ggplot(parallel_trend1, aes(quarter_index, foreign_inst_percentage, group = treated, color = treated)) + 
  geom_line() +
  geom_vline(xintercept=0) +
  theme_classic()

ggsave("parallel_trend.png", last_plot())

parallel_trend2 <- did_regression_matched_raw %>% 
  mutate(treated = if_else(treated == 1, "treated", "control")) %>% 
  group_by(treated, quarter_index, event_date) %>% 
  summarise(foreign_inst_percentage = mean(foreign_inst_percentage),
            foreign_shares_by_institutional_shares = mean(foreign_shares_by_institutional_shares),
            foreign_breadth = mean(foreign_breadth),
            foreign_breadth2 = mean(foreign_breadth2),
            domestic_inst_percentage = mean(domestic_inst_percentage),
            domestic_breadth = mean(domestic_breadth)) %>% 
  ungroup()

ggplot(parallel_trend2, aes(quarter_index, foreign_shares_by_institutional_shares, group = treated, color = treated)) + 
  geom_line() +
  geom_vline(xintercept=0) +
  theme_classic() + facet_wrap(. ~ event_date)

ggsave("parallel_trend_facet.png", last_plot())


## H2 DiD without propensity score matching (all the rest)

did_regression_summary <- did_regression %>% 
  select(-permno, -year, -sic_code) %>% 
  as.data.frame()

stargazer(did_regression_summary, title = "DiD summary statistics", out = "DiD_summary_no_prop.html")

summary(did_regression)

did_model1 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression)

did_model2 <- felm(foreign_shares_by_institutional_shares ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression)

did_model3 <- felm(domestic_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression)

did_model4 <- felm(foreign_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression)

did_model5 <- felm(foreign_breadth2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression)

did_model6 <- felm(domestic_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression)

did_list <- list(did_model1, did_model2, did_model3, did_model4, did_model5, did_model6)

stargazer(did_list, title = "Difference-in-differences regression results (H2)", 
          omit.stat = c("ser"), 
          add.lines = list(c("Industry fixed effects", rep("Yes", times = 6)), 
                           c("Year fixed effects", rep("Yes", times = 6))),
          out = "DiD H2 results_no_prop.html")

parallel_trend1 <- did_regression_raw %>% 
  mutate(treated = if_else(treated == 1, "treated", "control")) %>% 
  group_by(treated, quarter_index) %>% 
  summarise(foreign_inst_percentage = mean(foreign_inst_percentage),
            foreign_shares_by_institutional_shares = mean(foreign_shares_by_institutional_shares),
            foreign_breadth2 = mean(foreign_breadth2),
            foreign_breadth = mean(foreign_breadth),
            domestic_inst_percentage = mean(domestic_inst_percentage),
            domestic_breadth = mean(domestic_breadth))  %>% 
  ungroup()

ggplot(parallel_trend1, aes(quarter_index, foreign_inst_percentage, group = treated, color = treated)) + 
  geom_line() +
  geom_vline(xintercept=0) +
  theme_classic()

ggsave("parallel_trend_no_prop.png", last_plot())

parallel_trend2 <- did_regression_raw %>% 
  mutate(treated = if_else(treated == 1, "treated", "control")) %>% 
  group_by(treated, quarter_index, event_date) %>% 
  summarise(foreign_inst_percentage = mean(foreign_inst_percentage),
            foreign_shares_by_institutional_shares = mean(foreign_shares_by_institutional_shares),
            foreign_breadth2 = mean(foreign_breadth2),
            foreign_breadth = mean(foreign_breadth),
            domestic_inst_percentage = mean(domestic_inst_percentage),
            domestic_breadth = mean(domestic_breadth)) %>% 
  ungroup()

ggplot(parallel_trend2, aes(quarter_index, foreign_breadth2, group = treated, color = treated)) + 
  geom_line() +
  geom_vline(xintercept=0) +
  theme_classic() + facet_wrap(. ~ event_date)

ggsave("parallel_trend_facet_no_prop.png", last_plot())


## DiD H3 results

did_regression_h3 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-12:12)) %>% 
  filter(analyst_coverage < 3) %>% 
  group_by(permno, event_date, year, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

did_model1 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3)

did_model2 <- felm(domestic_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3)

did_model3 <- felm(foreign_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3)

did_model4 <- felm(domestic_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3)

did_list <- list(did_model1, did_model2, did_model3, did_model4)

stargazer(did_list, title = "Difference-in-differences regression results (H3)", 
          omit.stat = c("ser"), 
          add.lines = list(c("Industry fixed effects", rep("Yes", times = 4)), 
                           c("Year fixed effects", rep("Yes", times = 4))),
          out = "DiD H3 results.html")
