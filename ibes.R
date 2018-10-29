library(tidyverse)
library(readr)
library(lubridate)

# reading ibes detail histotry data -> entire database, us file, fy1, q1, q2, q3 and q4, only eps and from 1997-01 to 2011-01

detail_raw <- read_tsv("data/ibes_data_detail_history_detail.txt", col_types = cols(.default = "c"))

# renaming, formatting and selecting the variables

detail_temp1 <- detail_raw %>%
  transmute(ibes_ticker = TICKER,
            official_ticker = OFTIC,
            cusip = CUSIP,
            firm = CNAME,
            brokerage = ESTIMATOR,
            analyst = ANALYS,
            forecast_period_id = FPI,
            measure = MEASURE,
            eps_value = as.numeric(VALUE),
            announce_date = ymd(ANNDATS)) %>% # date when forecast was reported
  select(-measure, -forecast_period_id, -official_ticker) # dropped, as not of particular interest

rm(detail_raw)

# closure events from Kelly and Ljungqvist (2012) Appendix list

# brokerages and codes that closed mapped by ibes_names.csv

closures_raw <- read_tsv("data/closure_events.txt", col_names = T, col_types = cols(.default = "c"))
closures <- closures_raw %>% 
  transmute(brokerage_code = brokerage_code,
            brokerage_name = `brokerage_name (from Appendix list)`,
            event_date = ymd(event_date))

brokerage_codes_list <- list(closures$brokerage_code) %>% 
  flatten_chr()

closures_temp1 <- closures %>% 
  select(event_date) %>% 
  distinct()

event_dates <- closures_temp1 %>% 
  mutate(event_date_temp1 = event_date - years(1),
         event_date_temp2 = event_date + years(1),
         event_date_temp3 = event_date - years(2),
         event_date_temp4 = event_date + years(2),
         event_date_temp5 = event_date - years(3),
         event_date_temp6 = event_date + years(3),
         event_date_temp7 = event_date %m-%  months(3))

rm(closures_raw, closures_temp1)

# generate all the dates that are in the before_interval (i.e. [-1,0]-year)

yearbefore_list <- map2(event_dates$event_date_temp1, event_dates$event_date,
                     ~seq(.x, .y, "day") %>% as.character) %>% 
  flatten_chr() %>% 
  ymd()

yearbefore_list2 <- map2(event_dates$event_date_temp1, event_dates$event_date_temp7,
                        ~seq(.x, .y, "day") %>% as.character) %>% 
  flatten_chr() %>% 
  ymd()

# # read stopped_estimates from ibes to filter out analysts that stopped before the closure_event_date

stopped_raw <- read_tsv("data/ibes_data_detail_stopped_estimate.txt",  col_types = cols(.default = "c"))

stopped <- stopped_raw %>%
  transmute(ibes_ticker = TICKER,
            official_ticker = OFTIC,
            firm = CNAME,
            brokerage = ESTIMATOR,
            announce_stop_date = ymd(ASTPDATS), # date when forecast stopped
            forecast_period = ymd(FPEDATS)) %>% 
  select(-official_ticker, -firm, -forecast_period) %>% 
  distinct() %>% 
  arrange(ibes_ticker, brokerage, announce_stop_date)

rm(stopped_raw)

## left join detail data and stopped analysts data

detail_temp2 <- left_join(detail_temp1, stopped, by = c("ibes_ticker", "brokerage"))

## left join detail data and closure_dates

detail_temp3 <- left_join(detail_temp2, closures, by = c("brokerage" = "brokerage_code"))

rm(closures, stopped, detail_temp1, detail_temp2)

detail_temp3

# filter brokerages that are in the closed_brokerages list (i.e. treatment group)

detail_temp4 <- detail_temp3 %>% 
  mutate(yearbefore = announce_date >= (event_date - years(1))) %>% 
  filter(yearbefore == T) %>%  # filter only analysts that "covers" the firm, see Derrien and Keckses (2013) p. 1411
  mutate(in_brokerage_list = brokerage %in% brokerage_codes_list) %>% 
  filter(in_brokerage_list == T) %>% # have to be in closed brokerages list
  mutate(estimates_after = (announce_date > (event_date %m-% months(3)))) %>% 
  filter(estimates_after == F) %>% # there cannot be estimates after the event_date (relaxing for 3 months)
  mutate(stopped_before = (announce_stop_date <= (event_date %m-% months(3)))) %>%  
  filter(stopped_before == F)  # filter only firms of which analysts have not stopped before event_date (relax 3 months)

treated_firms_temp1 <- detail_temp4 %>% 
  group_by(cusip, event_date) %>% 
  summarise(treated = 1,
            after= 0,
            analysts = n_distinct(analyst)) %>% 
  ungroup()

treated_firms_temp2 <- detail_temp4 %>% 
  group_by(cusip, event_date) %>% 
  summarise(treated = 1,
            after= 1,
            analysts = n_distinct(analyst)) %>% 
  ungroup()

treated_firms <- bind_rows(treated_firms_temp1, treated_firms_temp2) %>% 
  arrange(cusip, event_date)

treated_firms %>% 
  group_by(cusip) %>% 
  tally()

rm(treated_firms_temp1, treated_firms_temp2)

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
