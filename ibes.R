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
  select(-measure, -forecast_period_id) %>%  # dropped, as not of particular interest
  filter(!is.na(cusip)) %>% # filter NA Cusips away, since can not be linked to other datasources
  distinct()


# closure events from Kelly and Ljungqvist (2012) Appendix list

# brokerages and codes that closed mapped by ibes_names.csv

closures_raw <- read_tsv("data/closure_events.txt", col_names = T, col_types = cols(.default = "c"))
closures <- closures_raw %>% 
  transmute(brokerage_code = brokerage_code,
            brokerage_name_ibes = `brokerage_name (from ibes_names)`,
            brokerage_name = `brokerage_name (from Appendix list)`,
            event_date = ymd(event_date),
            event_date_temp1 = event_date %m-% months(12))

brokerage_codes_list <- list(closures$brokerage_code) %>% 
  flatten_chr()

# read stopped_estimates from ibes to filter out analysts that stopped before the closure_event_date

stopped_raw <- read_tsv("data/ibes_data_detail_stopped_estimate.txt",  col_types = cols(.default = "c"))

stopped <- stopped_raw %>%
  transmute(ibes_ticker = TICKER,
            brokerage_code = ESTIMATOR,
            announce_stop_date = ymd(ASTPDATS), # date when forecast stopped
            forecast_period_end_date = ymd(FPEDATS)) %>% 
  distinct() %>% 
  arrange(ibes_ticker, brokerage_code, announce_stop_date)

# left join detail data and stopped analysts data

detail_temp2 <- detail_temp1 %>% 
  left_join(stopped, by = c("ibes_ticker", "brokerage_code", "forecast_period_end_date"))

# left join detail data and closure_dates

detail_temp3 <- detail_temp2 %>% 
  left_join(closures, by = c("brokerage_code")) %>% 
  select(-brokerage_name, -brokerage_name_ibes)

# filter brokerages that are in the closed_brokerages list (i.e. treatment group)

detail_temp4 <- detail_temp3 %>% 
  filter(!is.na(event_date)) %>% # have to have a closure date to be in treatment group
  mutate(yearbefore = announce_date %within% interval(event_date - years(1), event_date)) %>% 
  filter(yearbefore == T) %>%  # filter only analysts that actively "covers" the firm, see Derrien and Keckses (2013) p. 1411
  mutate(announce_stop_date = if_else(is.na(announce_stop_date), event_date - years(1), announce_stop_date)) %>% 
  mutate(stopped_before = announce_stop_date %within% interval(announce_date, event_date %m-% months(3))) %>%  
  filter(stopped_before == F)  # filter only firms of which analysts have not stopped before event_date (relax 3 months)
                                    # otherwise endogenous "stoppings", i.e. decided to stop covering

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

treated_firms <- bind_rows(treated_firms_temp1, treated_firms_temp2) %>% 
  arrange(cusip, event_date)


# control group

yearbefore_list <- map2(closures$event_date_temp1, closures$event_date,
                        ~seq(.x, .y, "day") %>% as.character) %>% 
  flatten_chr() %>% 
  ymd()

all_firms_temp1 <- detail_temp3 %>% 
  mutate(inyearbeforelist = announce_date %in% yearbefore_list) %>% 
  filter(inyearbeforelist == T) %>% # require control to be "actively" covered, i.e. year before
  group_by(cusip, announce_date) %>% 
  summarise(k = 1) %>% 
  ungroup()

closures_temp1 <- closures %>%
  select(event_date) %>% 
  mutate(k = 1)

all_firms <- inner_join(all_firms_temp1, closures_temp1, by = 'k') %>% 
  select(-k) %>%
  distinct()

control_firms_temp1 <- all_firms %>% 
  mutate(yearbefore = announce_date %within% interval(event_date %m-% months(12), event_date)) %>%
  filter(yearbefore == T) %>% # potential controls have to be similarly actively covered year before
  anti_join(treated_firms, by = c("cusip", "event_date"))

control_firms_temp2 <- control_firms_temp1 %>%
  select(-announce_date, -yearbefore) %>% 
  distinct() %>% 
  mutate(treated = 0,
         after = 0)

control_firms_temp3 <- control_firms_temp2 %>%
  mutate(treated = 0,
         after = 1)
  
control_firms <- bind_rows(control_firms_temp2, control_firms_temp3) %>% 
  arrange(cusip, event_date)


## identified firms to either treatment group (treated = 1) or control group (treated = 0) for each event date

ibes_did_raw <- bind_rows(treated_firms, control_firms) %>% 
  arrange(cusip, event_date)

## events and corresponding intervals [-15;-3] and [+3;+15] months

events <- closures %>% 
  select(event_date) %>% 
  distinct() %>% 
  mutate(before_event_start = event_date %m-% months(3) %m-% months(12),
         before_event_end = event_date %m-% months(3),
         after_event_start = event_date %m+% months(3),
         after_event_end = event_date %m+% months(3) %m+% months(12),
         before_interval = interval(before_event_start, before_event_end),
         after_interval = interval(after_event_start, after_event_end))

write_rds(events, "data/events.rds")

before_intervals <- events$before_interval

## total analyst coverage, used for calculating number of distinct analysts

analyst_coverage <- detail_temp3 %>%
  select(cusip, announce_date, analyst) %>% 
  arrange(cusip, announce_date, analyst) %>% 
  distinct()

#### calculating distinct analysts during the given events-intervals, for each event and for each stock (cusip)

## manual way (first event example)

analysts_before <- analyst_coverage %>% 
  filter(announce_date %within% interval(events$before_event_start[1], events$before_event_end[1])) %>% 
  mutate(event_date = events$event_date[1],
         after = 0) %>% 
  group_by(cusip, event_date, after) %>% 
  summarise(distinct_analysts = n_distinct(analyst))

analysts_after <- analyst_coverage %>% 
  filter(announce_date %within% interval(events$after_event_start[1], events$after_event_end[1])) %>% 
  mutate(event_date = events$event_date[1],
         after = 1) %>% 
  group_by(cusip, event_date, after) %>% 
  summarise(distinct_analysts = n_distinct(analyst))

analysts <- bind_rows(ibes_before, ibes_after) %>% 
  arrange(cusip, event_date)

## elegant way

filter1 <- function (df, interval){
  filter(df, announce_date %within% interval)
}

summarise1 <- function (df) {
  df %>% 
    group_by(cusip, event_date) %>% 
    summarise(result = n_distinct(analyst)) %>% 
    ungroup()
}

summarise2 <- function (df, fun) {
  df %>% 
    group_by(cusip, event_date) %>% 
    summarise(result = mean(analyst)) %>% 
    ungroup()
}

before_val <- map2(events$before_interval,
                   events$event_date,
                   ~filter1(analyst_coverage, .x) %>% mutate(event_date = .y)) %>% 
  map_df(~summarise1(.x)) %>% 
  rename(analyst_coverage = result) %>% 
  mutate(after = 0)

after_val <- map2(events$after_interval,
                   events$event_date,
                   ~filter1(analyst_coverage, .x) %>% mutate(event_date = .y)) %>% 
  map_df(~summarise1(.x)) %>% 
  rename(analyst_coverage = result) %>% 
  mutate(after = 1)

#vals <- full_join(before_val, after_val, by = c("cusip", "event_date"))

vals <- bind_rows(before_val, after_val)

ibes_did <- ibes_did_raw %>% 
  left_join(vals, by = c("cusip", "event_date", "after")) %>% 
  filter(!is.na(analyst_coverage))

write_rds(ibes_did, "data/ibes_did")
