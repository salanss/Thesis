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

write_rds(closures, "data/closures.rds")

events <- closures %>%
  select(event_date, event_date_temp1) %>% 
  distinct()

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

yearbefore_list <- map2(events$event_date_temp1, events$event_date,
                        ~seq(.x, .y, "day") %>% as.character) %>% 
  flatten_chr() %>% 
  ymd()

all_firms_temp1 <- detail_temp3 %>% 
  mutate(inyearbeforelist = announce_date %in% yearbefore_list) %>% 
  filter(inyearbeforelist == T) %>% # require control to be "actively" covered, i.e. year before
  group_by(cusip, announce_date) %>% 
  summarise(k = 1) %>% 
  ungroup()

closures_temp1 <- events %>%
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


## total analyst coverage, used for calculating number of distinct analysts

analyst_coverage <- detail_temp3 %>%
  select(cusip, announce_date, analyst) %>% 
  arrange(cusip, announce_date, analyst) %>% 
  distinct()

#### calculating distinct analysts during the given events-intervals, for each event and for each stock (cusip)

## elegant way

quarter_index <- c(1:12) # 12 quarters = 3 years

before_interval_fun <- function (event_date, quarter_index) {
  i <- 3 + (quarter_index - 1) * 3
  j <- 3 + (quarter_index) * 3
  df <- tibble(event_date = event_date,
               interval = interval(floor_date(event_date %m-% months(j) + days(1), unit = "quarter") - days(1),
                                   floor_date(event_date %m-% months(i) + days(1), unit = "quarter") - days(1)),
               quarter_index = quarter_index)
  df
}

after_interval_fun <- function (event_date, quarter_index) {
  i <- 3 + (quarter_index - 1) * 3
  j <- 3 + (quarter_index) * 3
  df <- tibble(event_date = event_date,
               interval = interval(ceiling_date(event_date %m+% months(i), unit = "quarter") - days(1),
                                   ceiling_date(event_date %m+% months(j), unit = "quarter") - days(1)),
               quarter_index = quarter_index)
  df
}

filter1 <- function (df_measures, df_events){
  interval <- df_events$interval  
  filter(df_measures, announce_date %within% interval)
}
 
summarise1 <- function (df) {
  df %>% 
    group_by(cusip, event_date, quarter_index) %>% 
    summarise(result = n_distinct(analyst)) %>% 
    ungroup()
}

# map every before_interval (12) to every distinct event_date (20) and flatten to list of 12*20
# do the filtering for list of data frames based on intervals and add columns event_date and year_index
# do the summarising for the measure of interest (number of distinct analysts in this case)
# and set corresponding after value (before = 0, after = 1)

before_val <- map(quarter_index, ~map(events$event_date, ~before_interval_fun(.x, .y), .y = .x)) %>%
  flatten() %>% 
  map(~filter1(analyst_coverage, .x) %>%
        mutate(event_date = .x$event_date,
               quarter_index = .x$quarter_index)) %>% 
  map_df(~summarise1(.x)) %>% 
  rename(analyst_coverage = result) %>% 
  mutate(after = 0,
         quarter_index = (-1)*quarter_index) %>% 
  arrange(cusip, event_date, quarter_index)

after_val <- map(quarter_index, ~map(events$event_date, 
                                  ~after_interval_fun(.x, .y), .y = .x)) %>% 
  flatten() %>% 
  map(~filter1(analyst_coverage, .x) %>%
        mutate(event_date = .x$event_date,
               quarter_index = .x$quarter_index)) %>% 
  map_df(~summarise1(.x)) %>% 
  rename(analyst_coverage = result) %>% 
  mutate(after = 1) %>% 
  arrange(cusip, event_date, quarter_index)

#vals <- full_join(before_val, after_val, by = c("cusip", "event_date"))

vals <- bind_rows(before_val, after_val)

ibes_did <- ibes_did_raw %>% 
  left_join(vals, by = c("cusip", "event_date", "after")) %>% 
  filter(!is.na(analyst_coverage)) %>% 
  distinct()

write_rds(ibes_did, "data/ibes_did")
