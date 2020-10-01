library(tidyverse)
library(lubridate)
library(lfe)
library(stargazer)
library(ggplot2)
library(MatchIt)
library(starpolishr)
library(AER)

# baseline regressions

baseline_regression_raw <- read_rds("data/baseline_regression_raw.rds") %>% 
  select(permno:sic_code, bid_ask_spread, everything()) %>% 
  select(-debt) %>% 
  rename(FOR_OWN = foreign_own, FOR_TO_INST_OWN = 
           foreign_own2, FOR_BREADTH = foreign_breadth,  FOR_TO_INST_BREADTH = foreign_breadth2,
         INST_OWN = institutional_own, INST_BREADTH = institutional_breadth, BA_SPREAD = bid_ask_spread,
         PIN_DY = pin_dy, PIN_DY_ADJ = pin_dy_adj, PSOS_DY = psos_dy, PIN_EHO = pin_eho,
         PIN_BHL = pin_bhl, PIN_BH = pin_bh, MIA = mia, VCV_USD = vcv1, VCV_MKT = vcv2, VCV_TO = vcv3,
         LOG_MKT_CAP = log_market_cap, BM_RATIO = book_to_market, LEVERAGE = leverage, ROA = roa) %>% 
  select(-spread)

write_rds(baseline_regression_raw, "results/baseline_regression_raw.rds")

cor_raw <- baseline_regression_raw %>% 
  select(year, FOR_OWN:FOR_TO_INST_OWN, FOR_BREADTH:FOR_TO_INST_BREADTH, INST_OWN, INST_BREADTH,
         LOG_MKT_CAP:ROA, BA_SPREAD, PIN_DY:VCV_TO) %>%
  mutate(year = as.integer(year))

years <- cor_raw %>% transmute(year = as.integer(year)) %>% distinct() %>% flatten_chr()

filter1 <- function (df, years){
  df %>% 
    filter(year == years) %>% 
    select(-year)
}

cor_dfs_raw <- map(years, ~filter1(cor_raw, .x))

map_na_function <- function(df){
  df %>% 
  map(~.x) %>% 
    discard(~all(is.na(.x))) %>%
    map_df(~.x)
}

cor_dfs <- map(cor_dfs_raw, ~map_na_function(.x))

cor_matrices <- map(cor_dfs_raw, ~cor(.x, use = "pairwise.complete.obs"))

arr <- array(unlist(cor_matrices), c(dim(cor_matrices[[1]]), length(cor_matrices)))

cor_matrices_mean <- round(rowMeans(x = arr, dims = 2, na.rm = T), 2)

cols <- cor_raw %>% select(-year) %>% colnames()

rownames(cor_matrices_mean) <- cols

colnames(cor_matrices_mean) <- cols

cor_matrices_mean[upper.tri(cor_matrices_mean)] <- ""

cor_matrix <- cor_matrices_mean

write_rds(cor_matrix, "results/cor_matrix.rds")

## difference-in-differences regressions (did)

did_regression_raw <- read_rds("data/did_regression_raw.rds") %>% 
  mutate(year = year(event_date),
         report_year = year(report_date)) %>% 
  group_by(permno, event_date, brokerage_name, treated) %>%
  mutate(n = n_distinct(after)) %>%
  ungroup() %>%
  filter(n > 1) %>%  # require for every permno to have before and after value for each event
  arrange(event_date, quarter_index, treated) %>% 
  rename(FOR_OWN = foreign_own, FOR_TO_INST_OWN = 
           foreign_own2, FOR_BREADTH = foreign_breadth,  FOR_TO_INST_BREADTH = foreign_breadth2,
         INST_OWN = institutional_own, INST_BREADTH = institutional_breadth,
         LOG_MKT_CAP = log_market_cap, BM_RATIO = book_to_market, LEVERAGE = leverage, ROA = roa,
         ANALYST_COVERAGE = analyst_coverage) %>% 
  mutate(FOR_OWN_QUASI =  foreign_own_quasi,
         FOR_OWN_NONQUASI = FOR_OWN - foreign_own_quasi,
         FOR_OWN_DEDICATED = foreign_own_dedicated,
         FOR_OWN_TRANSIENT = foreign_own_transient,
         FOR_BREADTH_QUASI = foreign_breadth_quasi,
         FOR_BREADTH_NONQUASI = FOR_BREADTH - foreign_breadth_quasi,
         FOR_BREADTH_DEDICATED = foreign_breadth_dedicated,
         FOR_BREADTH_TRANSIENT = foreign_breadth_transient,
         FOR_TO_INST_OWN_QUASI = foreign_own_quasi/INST_OWN,
         FOR_TO_INST_OWN_NONQUASI = (FOR_OWN - foreign_own_quasi)/INST_OWN,
         FOR_TO_INST_OWN_DEDICATED = foreign_own_dedicated/INST_OWN,
         FOR_TO_INST_OWN_TRANSIENT = foreign_own_transient/INST_OWN,
         FOR_TO_INST_BREADTH_QUASI = foreign_breadth_quasi/INST_BREADTH,
         FOR_TO_INST_BREADTH_NONQUASI = (FOR_BREADTH - foreign_breadth_quasi)/INST_BREADTH,
         FOR_TO_INST_BREADTH_DEDICATED = foreign_breadth_dedicated/INST_BREADTH,
         FOR_TO_INST_BREADTH_TRANSIENT = foreign_breadth_transient/INST_BREADTH)

# stargazer(cor_matrix, title = "correlation matrix", out = "correlation_matrix_did.html")

columns_for_summarise <- c("FOR_OWN", "FOR_TO_INST_OWN", "FOR_BREADTH", 
                           "FOR_TO_INST_BREADTH","INST_OWN", "INST_BREADTH", 
                           "FOR_OWN_QUASI", "FOR_OWN_NONQUASI", "FOR_OWN_DEDICATED",
                           "FOR_OWN_TRANSIENT", "FOR_BREADTH_QUASI", "FOR_BREADTH_NONQUASI",
                           "FOR_BREADTH_DEDICATED", "FOR_BREADTH_TRANSIENT", "FOR_TO_INST_OWN_QUASI",
                           "FOR_TO_INST_OWN_NONQUASI", "FOR_TO_INST_OWN_DEDICATED", 
                           "FOR_TO_INST_OWN_TRANSIENT", "FOR_TO_INST_BREADTH_QUASI", 
                           "FOR_TO_INST_BREADTH_NONQUASI", "FOR_TO_INST_BREADTH_DEDICATED", 
                           "FOR_TO_INST_BREADTH_TRANSIENT", "LOG_MKT_CAP", "BM_RATIO", "LEVERAGE", 
                           "ROA", "ANALYST_COVERAGE")

did_regression <- did_regression_raw %>% 
  filter(quarter_index %in% c(-12:12)) %>% 
  group_by(permno, event_date, brokerage_name, year, treated, after) %>% 
  mutate(sic_code = last(sic_code)) %>%
  ungroup() %>% 
  group_by(permno, event_date, year, brokerage_name, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

did_regression_nonmatched1 <- did_regression_raw %>% 
  rename(AFTER = after, TREATED = treated) %>% 
  filter(quarter_index %in% c(-4:4)) %>% 
  group_by(permno, event_date, brokerage_name, TREATED, AFTER) %>%
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_nonmatched1, "results/did_regression_nonmatched1.rds")

did_regression_nonmatched2 <- did_regression_raw %>% 
  rename(AFTER = after, TREATED = treated) %>% 
  filter(quarter_index %in% c(-8:8)) %>% 
  group_by(permno, event_date, brokerage_name, TREATED, AFTER) %>%
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_nonmatched2, "results/did_regression_nonmatched2.rds")

did_regression_nonmatched3 <- did_regression_raw %>% 
  rename(AFTER = after, TREATED = treated) %>% 
  group_by(permno, event_date, brokerage_name, TREATED, AFTER) %>%
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_nonmatched3, "results/did_regression_nonmatched3.rds")

did_regression_matched_temp1 <- did_regression %>% 
  filter(after == 0) %>% # match only based on before values during [-3;0] years
  group_by(permno, event_date, brokerage_name, treated) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

events <- did_regression_raw %>% select(event_date, brokerage_name) %>% 
  distinct() %>% mutate(event_brokerage = paste(event_date, brokerage_name, sep = " ")) %>% 
  select(-event_date, -brokerage_name)

filter1 <- function(df, events) {
  df %>% 
    filter(paste(event_date, brokerage_name, sep = " ") == events)
}

propensity_match <- function(df) {
  did_match <- matchit(treated ~ LOG_MKT_CAP + BM_RATIO + ANALYST_COVERAGE,
          method = "nearest", distance = "logit", data = df, ratio = 3)
  df <- match.data(did_match)
  df
}

# produces propensity scores for permno-event_date-treated combinations (based on means)

did_regression_matched_temp2 <- map(events$event_brokerage, ~filter1(did_regression_matched_temp1, .x)) %>%  
  map_df(~propensity_match(.x))

# now to join with before and after values

did_regression_matched_raw <- did_regression_matched_temp2 %>% 
  as_tibble() %>% 
  select(permno, event_date, brokerage_name, treated) %>% 
  left_join(did_regression_raw) %>% 
  rename(AFTER = after, TREATED = treated) %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)
  
write_rds(did_regression_matched_raw, "results/did_regression_matched_raw.rds")

did_regression_matched05 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-2:2)) %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_matched05, "results/did_regression_matched05.rds")

did_regression_matched1 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-4:4)) %>% 
  group_by(permno, event_date, brokerage_name, TREATED, AFTER) %>%
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_matched1, "results/did_regression_matched1.rds")

did_regression_matched2 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-8:8)) %>% 
  group_by(permno, event_date, brokerage_name, TREATED, AFTER) %>%
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_matched2, "results/did_regression_matched2.rds")

did_regression_matched3 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-12:12)) %>% 
  group_by(permno, event_date, brokerage_name, TREATED, AFTER) %>%
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_matched3, "results/did_regression_matched3.rds")

## difference-in-differences iv

did_regression_raw <- read_rds("data/did_regression_raw_iv.rds") %>% 
  mutate(year = year(event_date)) %>% 
  group_by(permno, event_date, brokerage_name, treated) %>%
  mutate(n = n_distinct(after)) %>%
  ungroup() %>%
  filter(n > 1) %>%  # require for every permno to have before and after value for each event
  arrange(event_date, quarter_index, treated) %>% 
  rename(FOR_OWN = foreign_own, FOR_TO_INST_OWN = 
           foreign_own2, FOR_BREADTH = foreign_breadth,  FOR_TO_INST_BREADTH = foreign_breadth2,
         INST_OWN = institutional_own, INST_BREADTH = institutional_breadth,
         LOG_MKT_CAP = log_market_cap, BM_RATIO = book_to_market, LEVERAGE = leverage, ROA = roa,
         ANALYST_COVERAGE = analyst_coverage)


columns_for_summarise <- c("FOR_OWN", "FOR_TO_INST_OWN", "FOR_BREADTH", 
                           "FOR_TO_INST_BREADTH","INST_OWN", "INST_BREADTH", 
                           "LOG_MKT_CAP", "BM_RATIO", "LEVERAGE", 
                           "ROA", "ANALYST_COVERAGE", "vcv1", "vcv2", "vcv3", "bid_ask_spread")

did_regression <- did_regression_raw %>% 
  filter(quarter_index %in% c(-12:12)) %>% 
  group_by(permno, event_date, brokerage_name, treated, after) %>% 
  mutate(sic_code = last(sic_code)) %>%
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

did_regression_matched_temp1 <- did_regression %>% 
  filter(after == 0) %>% # match only based on before values during [-3;0] years
  group_by(permno, event_date, brokerage_name, treated) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

events <- did_regression_raw %>% select(event_date, brokerage_name) %>% 
  distinct() %>% mutate(event_brokerage = paste(event_date, brokerage_name, sep = " ")) %>% 
  select(-event_date, -brokerage_name)

filter1 <- function(df, events) {
  df %>% 
    filter(paste(event_date, brokerage_name, sep = " ") == events)
}

propensity_match <- function(df) {
  did_match <- matchit(treated ~ LOG_MKT_CAP + BM_RATIO + ANALYST_COVERAGE,
                       method = "nearest", distance = "logit", data = df, ratio = 3)
  df <- match.data(did_match)
  df
}

# produces propensity scores for permno-event_date-treated combinations (based on means)

did_regression_matched_temp2 <- map(events$event_brokerage, ~filter1(did_regression_matched_temp1, .x)) %>%  
  map_df(~propensity_match(.x))

# now to join with before and after values

did_regression_matched_raw <- did_regression_matched_temp2 %>% 
  as_tibble() %>% 
  select(permno, event_date, brokerage_name, treated) %>% 
  left_join(did_regression_raw) %>% 
  rename(AFTER = after, TREATED = treated) %>% 
  mutate(report_year = year(report_date)) %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_matched_raw, "results/did_regression_matched_raw_iv.rds")

did_regression_matched05 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-2:2)) %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_matched05, "results/did_regression_matched05_iv.rds")

did_regression_matched1 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-4:4)) %>% 
  group_by(permno, event_date, brokerage_name, TREATED, AFTER) %>%
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, brokerage_name, TREATED) %>%
  mutate(n = n_distinct(AFTER)) %>%
  ungroup() %>%
  filter(n > 1)

write_rds(did_regression_matched1, "results/did_regression_matched1_iv.rds")

did_data <- did_regression_matched1 %>% 
  mutate(AFTER_TREATED = AFTER*TREATED,
         sic_code_lyhyt = as.character(str_sub(sic_code, 1, 2)))

did_model1 <- felm(FOR_OWN ~ BM_RATIO + LEVERAGE + ROA|
                     year + sic_code_lyhyt|
                     (vcv3 ~ TREATED + AFTER + AFTER_TREATED + LOG_MKT_CAP) |
                     year + sic_code_lyhyt,
                   data = did_data) %>% 
  summary()

did_model1

did_model1 <- ivreg(FOR_OWN ~ bid_ask_spread + BM_RATIO + LEVERAGE + ROA |
                      .-bid_ask_spread + TREATED + AFTER + AFTER_TREATED, data = did_data) %>% 
  summary()

did_model1

j <- felm(FOR_OWN ~ bid_ask_spread + BM_RATIO + LEVERAGE + ROA |
            report_year + sic_code | 0 |report_year + sic_code, data = did_data) %>% 
  summary()

j

did_model2 <- felm(FOR_TO_INST_OWN ~ AFTER + TREATED + AFTER_TREATED + LOG_MKT_CAP +
                     BM_RATIO + LEVERAGE + ROA |
                     report_year + sic_code_lyhyt |
                     0 | 
                     report_year + sic_code_lyhyt, 
                   data = did_data)

summary(did_model2)

did_data$vcv1_hats <- fitted(did_model2)

did_model3 <- felm(FOR_OWN ~ vcv1_hats + BM_RATIO + 
                     LEVERAGE + ROA | report_year + sic_code | 0 | report_year + sic_code,
                   data = did_data)

summary(did_model3)

did_model5 <- felm(vcv3 ~ TREATED + AFTER + AFTER_TREATED + LOG_MKT_CAP + BM_RATIO +
                     LEVERAGE + ROA | year | 0 | year,
                   data = did_data)

summary(did_model5)
