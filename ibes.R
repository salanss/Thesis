library(tidyverse)
library(readr)
library(lubridate)

# reading ibes detail histotry data -> entire database, us file, fy1, q1, q2, q3 and q4, only eps and from 1997-01 to 2011-01

detail_raw <- read_tsv("data/ibes_data_detail_history_detail.txt", col_types = cols(.default = "c"))

# renaming, formatting and selecting the variables

detail_temp1 <- detail_raw %>%
  transmute(ibes_ticker = TICKER,
            cusip = CUSIP,
            firm = CNAME,
            brokerage_code = ESTIMATOR,
            analyst = ANALYS,
            forecast_period_id = FPI,
            measure = MEASURE,
            eps_value = parse_double(VALUE),
            forecast_period_end_date = ymd(FPEDATS),
            announce_date = ymd(ANNDATS)) %>% # date when forecast was reported
  select(-measure, -forecast_period_id) # dropped, as not of particular interest


# closure events from Kelly and Ljungqvist (2012) Appendix list

# brokerages and codes that closed mapped by ibes_names.csv

closures_raw <- read_tsv("data/closure_events.txt", col_names = T, col_types = cols(.default = "c"))
closures <- closures_raw %>% 
  transmute(brokerage_code = brokerage_code,
            brokerage_name_ibes = `brokerage_name (from ibes_names)`,
            brokerage_name = `brokerage_name (from Appendix list)`,
            event_date = ymd(event_date))

brokerage_codes_list <- list(closures$brokerage_code) %>% 
  flatten_chr()

# read stopped_estimates from ibes to filter out analysts that stopped before the closure_event_date

stopped_raw <- read_tsv("data/ibes_data_detail_stopped_estimate.txt",  col_types = cols(.default = "c"))

stopped <- stopped_raw %>%
  transmute(ibes_ticker = TICKER,
            firm = CNAME,
            brokerage_code = ESTIMATOR,
            announce_stop_date = ymd(ASTPDATS), # date when forecast stopped
            forecast_period_end_date = ymd(FPEDATS)) %>% 
  select(-firm) %>% 
  distinct() %>% 
  arrange(ibes_ticker, brokerage_code, announce_stop_date)

# left join detail data and stopped analysts data

detail_temp2 <- left_join(detail_temp1, stopped, by = c("ibes_ticker", "brokerage_code", "forecast_period_end_date"))

# left join detail data and closure_dates

detail_temp3 <- left_join(detail_temp2, closures, by = c("brokerage_code")) %>% 
  select(-brokerage_name, -brokerage_name_ibes)

# filter brokerages that are in the closed_brokerages list (i.e. treatment group)

detail_temp4 <- detail_temp3 %>% 
  mutate(in_brokerage_list = brokerage_code %in% brokerage_codes_list) %>% 
  filter(in_brokerage_list == T) %>% # have to be in closed brokerages list
  mutate(yearbefore = announce_date %within% interval(event_date - years(1), event_date)) %>% 
  filter(yearbefore == T) %>%  # filter only analysts that "covers" the firm, see Derrien and Keckses (2013) p. 1411
  mutate(stopped_before = announce_stop_date %within% interval(announce_date, event_date %m-% months(3))) %>%  
  filter(stopped_before == F) %>%  # filter only firms of which analysts have not stopped before event_date (relax 3 months)
  filter(!is.na(cusip))

treated_firms_temp1 <- detail_temp4 %>% 
  group_by(cusip, event_date) %>% 
  summarise(treated = 1,
            after= 0) %>% 
  ungroup()

treated_firms_temp2 <- detail_temp4 %>% 
  group_by(cusip, event_date) %>% 
  summarise(treated = 1,
            after= 1) %>% 
  ungroup()

treated_firms_temp3 <- bind_rows(treated_firms_temp1, treated_firms_temp2) %>% 
  mutate(event_year = year(event_date)) %>% 
  arrange(cusip, event_date)

treated_firms_distinct <- treated_firms_temp3 %>% 
  select(cusip) %>% 
  distinct()

treated_firms_list <- list(treated_firms_distinct$cusip) %>% 
  flatten_chr()

analyst_coverage_temp1 <- detail_temp3 %>% 
  mutate(announce_year = year(announce_date),
         in_treated_list = cusip %in% treated_firms_list) %>% 
  filter(in_treated_list == T)

analyst_coverage <- analyst_coverage_temp1 %>% 
  group_by(cusip, announce_year) %>% 
  summarise(analyst_coverage = n_distinct(analyst)) %>% 
  ungroup()

treated_firms <- left_join(treated_firms_temp3, analyst_coverage, by = c("cusip", "event_year" = "announce_year"))

write_rds(treated_firms, "data/treated_firms_ibes.rds")

# control group

all_firms_temp1 <- detail_temp3 %>% 
  group_by(cusip) %>% 
  summarise(k = 1) %>% 
  ungroup()

closures_temp1 <- closures %>%
  select(event_date) %>% 
  mutate(k = 1)

all_firms_temp2 <- inner_join(all_firms_temp1, closures_temp1, by = 'k') %>% 
  select(-k) %>% 
  distinct()

control_firms_temp1 <- anti_join(all_firms_temp2, treated_firms, by = c("cusip", "event_date"))

control_firms_temp2 <- control_firms_temp1 %>%
  mutate(treated = 0,
         after = 0)

control_firms_temp3 <- control_firms_temp2 %>%
  mutate(treated = 0,
         after = 1)
  
control_firms <- bind_rows(control_firms_temp2, control_firms_temp3) %>% 
  arrange(cusip, event_date)

control_firms %>%
  group_by(cusip) %>% 
  tally()

ibes_firms <- bind_rows(treated_firms, control_firms) %>% 
  arrange(cusip, event_date)
