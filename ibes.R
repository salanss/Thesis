library(tidyverse)
library(readr)
library(lubridate)

# reading ibes detail histotry data -> entire database, us file, fy1, q1, q2, q3 and q4, only eps and from 1999-01 to 2010-01

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
            activation_date = ymd(ACTDATS), # date when forecast was recorded by Thomson Reuters
            activation_date_actual = ymd(ACTDATS_ACT), # date when actual was recorded by Thomson Reuters
            announce_date = ymd(ANNDATS), # date when forecast was reported
            announce_date_actual = ymd(ANNDATS_ACT), # date when actual was reported
            forecast_period_end_date = ymd(FPEDATS)) %>% 
  select(-activation_date_actual, -activation_date, -measure) # dropped, as not of particular interest

# closure events from Kelly and Ljungqvist (2012) Appendix list

# brokerages and codes that closed

closures_raw <- read_tsv("data/brokerage_codes.txt", col_names = T, col_types = cols(.default = "c"))
closures <- closures_raw %>% 
  transmute(brokerage_code = brokerage_code,
            brokerage_name = `brokerage_name (from Appendix list)`,
            event_date = ymd(event_date))

brokerage_codes_list <- list(closures$brokerage_code) %>% 
  flatten_chr()

brokerage_event_dates_temp <- closures %>% 
  select(event_date) %>% 
  distinct()

event_dates <- closures %>% 
  mutate(event_date_temp1 = event_date - years(1),
         event_date_temp2 = event_date + years(1),
         event_date_temp3 = event_date - years(2),
         event_date_temp4 = event_date + years(2),
         event_date_temp5 = event_date - years(3),
         event_date_temp6 = event_date + years(3))

# generate all the dates that are in the before_interval (i.e. [-1,0]-year)

yearbefore_list <- map2(event_dates$event_date_temp1, event_dates$event_date,
                     ~seq(.x, .y, "day") %>% as.character) %>% 
  flatten_chr() %>% 
  ymd()

# filter estimates that are during the year before the disappearance date in order to find out whether an analyst "covers" a firm
# see Derrien and Kecskes (2013) p. 1411


detail_temp2 <- detail_temp1 %>% 
  mutate(in_before_interval = announce_date %in% yearbefore_list)
         
detail_temp3 <- detail_temp2 %>% 
  filter(in_before_interval == T, !is.na(eps_value))  


detail_temp4 <- detail_temp3 %>% 
  mutate(in_brokerage_list = brokerage %in% brokerage_codes_list)

# filter brokerages that are in the closed_brokerages list (i.e. treatment group)

detail_temp5 <- detail_temp4 %>% 
  filter(in_brokerage_list == T)

# left join to data the information of closure_dates and brokerege_names

detail_temp6 <- left_join(detail_temp5, closed_brokerages, by = c("brokerage" = "brokerage_code"))

treatment_firms <- detail_temp6 %>% 
  group_by(ibes_ticker, cusip, firm) %>% 
  summarise(event_date = max(event_date))

treatment_firms_list <- list(treatment_firms$ibes_ticker) %>% 
  flatten_chr()

# filter firms that are not in the treatment_firms_list (i.e. control group)

control_firms <- detail_temp3 %>% 
  mutate(in_treatment_list = ibes_ticker %in% treatment_firms_list) %>% 
  filter(in_treatment_list == F) %>% 
  group_by(ibes_ticker, cusip, firm) %>% 
  summarise(max(announce_date))


# # read stopped_estimates from ibes to filter out analysts that stopped before the closure_event_date (three months lag?)
# 
# stopped_raw <- read_tsv("data/ibes_data_detail_stopped_estimate.txt",  col_types = cols(.default = "c"))
# 
# stopped <- stopped_raw %>%
#   transmute(ibes_ticker = TICKER,
#             official_ticker = OFTIC,
#             firm = CNAME,
#             brokerage = ESTIMATOR,
#             announce_stop_date = ymd(ASTPDATS), # date when forecast stopped
#             forecast_period = ymd(FPEDATS))
# 
# stopped_temp1 <- stopped %>%
#   mutate(in_before_interval = announce_stop_date %in% yearbefore_list)
# 
# detail_temp4 <-  left_join(detail_temp3, stopped_temp1, by = c("ibes_ticker" = "ibes_ticker", "brokerage" = "brokerage")) %>% 
#   select(-eps_value, -official_ticker, -announce_date_actual, -forecast_period_end_date, -forecast_period_id) # %>%
# #filter(stopped_temp1, in_before_interval == F)
# 
# detail_temp4
