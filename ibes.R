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

# filter estimates that are during the year before the disappearance date in order to find out whether an analyst "covers" a firm
# see Derrien and Kecskes (2013) p. 1411

# 
# detail_temp2 <- detail_temp1 %>% 
#   mutate(in_year_before_interval = announce_date %in% yearbefore_list) %>% 
#   filter(in_year_before_interval == T, !is.na(eps_value))  %>% 
#   select(-in_year_before_interval)

# # read stopped_estimates from ibes to filter out analysts that stopped before the closure_event_date

stopped_raw <- read_tsv("data/ibes_data_detail_stopped_estimate.txt",  col_types = cols(.default = "c"))

stopped <- stopped_raw %>%
  transmute(ibes_ticker = TICKER,
            official_ticker = OFTIC,
            firm = CNAME,
            brokerage = ESTIMATOR,
            announce_stop_date = ymd(ASTPDATS), # date when forecast stopped
            forecast_period = ymd(FPEDATS)) %>% 
  select(-official_ticker, -firm, -forecast_period)

rm(stopped_raw)

## left join detail data and stopped analysts data

detail_temp2 <- left_join(detail_temp1, stopped, by = c("ibes_ticker", "brokerage"))

## left join detail data and closure_dates

detail_temp3 <- left_join(detail_temp2, closures, by = c("brokerage" = "brokerage_code"))

rm(closures, stopped, detail_temp1, detail_temp2)

detail_temp3

# filter brokerages that are in the closed_brokerages list (i.e. treatment group)

detail_temp4 <- detail_temp3 %>% 
  mutate(in_brokerage_list = brokerage %in% brokerage_codes_list) %>% 
  filter(in_brokerage_list == T) %>% # have to be in closed brokerages list
  mutate(estimates_before = (announce_date <= (event_date) %m+% months(3))) %>% 
  filter(estimates_before == T) %>% # there cannot be estimates after the event_date (relaxing for 3 months)
  mutate(stopped_before = (announce_stop_date <= (event_date %m-% months(3)))) %>%  
  filter(stopped_before == F) %>%  # filter only firms of which analysts have not stopped before event_date (relax 3 months)
  mutate(yearbefore = announce_date >= (event_date - years(1))) %>% 
  filter(yearbefore == T) %>% # filter only analysts that "covers" the firm, see Derrien and Keckses (2013) p. 1411
  filter(!is.na(eps_value))

treated_firms_temp1 <- detail_temp4 %>% 
  group_by(ibes_ticker, cusip, firm, event_date) %>% 
  summarise(treated = 1,
            after= 0,
            analysts = n_distinct(analyst))

treated_firms_temp2 <- detail_temp4 %>% 
  group_by(ibes_ticker, cusip, firm, event_date) %>% 
  summarise(treated = 1,
            after= 1,
            analysts = n_distinct(analyst))

treated_firms <- bind_rows(treated_firms_temp1, treated_firms_temp2) %>% 
  arrange(ibes_ticker, cusip, event_date)

treated_firms %>% 
  group_by(cusip) %>% 
  tally()

treated_firms_temp3 <- treated_firms %>% 
  ungroup() %>% 
  select(ibes_ticker) %>% 
  distinct()

treated_firms_list <- list(treated_firms_temp3$ibes_ticker) %>% 
  flatten_chr()

summary(treated_firms)

rm(treated_firms_temp1, treated_firms_temp2)

# control group

detail_temp5 <- detail_temp3 %>% 
  mutate(in_brokerage_list = brokerage %in% brokerage_codes_list) %>% 
  filter(in_brokerage_list == F) %>% 
  mutate(in_yearbefore = announce_date %in% yearbefore_list) %>% 
  filter(in_yearbefore == T) %>% 
  mutate(stopped_before = announce_stop_date %in% yearbefore_list2) %>% 
  filter(stopped_before == F)

detail_temp6 <- detail_temp3 %>% 
  mutate(in_brokerage_list = brokerage %in% brokerage_codes_list) %>% 
  filter(in_brokerage_list == T) %>% 
  

detail_temp5
  
control_firms_temp1 <- detail_temp5 %>% 
  group_by(cusip, firm, event_date) %>% 
  summarise(treated = 0,
            after= 0,
            analysts = n_distinct(analyst))

control_firms_temp2 <- detail_temp5 %>% 
  group_by(cusip, firm, event_date) %>% 
  summarise(treated = 0,
            after= 1,
            analysts = n_distinct(analyst))

control_firms <- bind_rows(control_firms_temp1, control_firms_temp2) %>% 
  arrange(cusip, event_date)

control_firms %>% 
  group_by(cusip) %>% 
  tally()
