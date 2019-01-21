library(tidyverse)
library(readr)
library(lubridate)
library(robustHD)
library(DescTools)

# baseline linking

thirteenf <- read_rds("data/13f_output.rds") 
thirteenf_institutionals_quarterly <- read_rds("data/13f_institutionals_quarterly.rds") %>% 
  transmute(report_date,
            total_institutions = institutional_numbers)

thirteenf_merged <- thirteenf %>%
  left_join(thirteenf_institutionals_quarterly, by = "report_date") %>% 
  mutate(report_date = ceiling_date(report_date, unit = "month") - days(1))

crsp_quarter_stock <- read_rds("data/crsp_quarter_stock.rds")

compustat_quarter <- read_rds("data/compustat_quarter_final.rds") %>% 
  mutate(datadate = ceiling_date(datadate, unit = "month") - days(1)) # sanity check

information_asymmetry_measures <- read_rds("data/information_asymmetry_measures.rds")

thirteenf_crsp_merged <- thirteenf_merged %>%
  left_join(crsp_quarter_stock, by = c("permno", "report_date" = "quarter_date"))

thirteenf_crsp_not_merged <- crsp_quarter_stock %>% 
  anti_join(thirteenf_merged, by = c("permno")) %>% 
  filter(quarter_date >= ymd(19971231)) %>% 
  mutate(report_date = quarter_date,
         institutional_ownership_shares = 0,
         foreign_institutional_ownership_shares = 0,
         domestic_institutional_ownership_shares = 0,
         institutional_numbers = 0,
         foreign_institutional_numbers = 0,
         domestic_institutional_numbers = 0) %>% 
  select(-quarter_date) %>% 
  left_join(thirteenf_institutionals_quarterly, by = "report_date")

thirteenf_crsp <- bind_rows(thirteenf_crsp_merged, thirteenf_crsp_not_merged) %>% 
  mutate(inst_ownership_percentage = if_else(shares_outstanding_adjusted > 0, 
                                             institutional_ownership_shares / shares_outstanding_adjusted, NA_real_),
         foreign_ownership_percentage = if_else(shares_outstanding_adjusted > 0, 
                                                foreign_institutional_ownership_shares / shares_outstanding_adjusted, NA_real_),
         domestic_ownership_percentage = if_else(shares_outstanding_adjusted > 0, 
                                                 domestic_institutional_ownership_shares / shares_outstanding_adjusted, NA_real_),
         inst_breadth = institutional_numbers / total_institutions,
         foreign_breadth = foreign_institutional_numbers / total_institutions,
         domestic_breadth = domestic_institutional_numbers / total_institutions) %>% 
  filter(!is.na(foreign_ownership_percentage))

# thirteenf_crsp2 <- thirteenf_crsp %>% 
#   mutate(foreign_ownership_percentage = Winsorize(foreign_ownership_percentage, probs = c(0.01, 0.99))
# 
# summary(thirteenf_crsp)
# 
# summary(thirteenf_crsp2)

thirteenf_crsp_compustat_temp <- thirteenf_crsp %>% 
  inner_join(compustat_quarter, by = c("permno", "report_date" = "datadate")) %>% 
  transmute(report_date = report_date,
            year = year(report_date),
            permno = permno,
            sic_code = if_else(sic_code.x == "0" | is.na(sic_code.x) == T, NA_character_, sic_code.x),
            shares_outstanding_adjusted = shares_outstanding_adjusted, #crsp 
                                              # coalesce(shares_outstanding.x, shares_outstanding.y, shares_outstanding)
            inst_percentage = inst_ownership_percentage,
            foreign_inst_percentage = foreign_ownership_percentage,
            domestic_inst_percentage = domestic_ownership_percentage,
            inst_breadth = inst_breadth,
            foreign_breadth = foreign_breadth,
            domestic_breadth = domestic_breadth,
            price = price_adjusted, # coalesce(price, price_close_calendar)
            market_cap = market_cap.x, 
            log_market_cap = log(market_cap.x),
            book_equity = book_equity,
            book_to_market = book_to_market,
            leverage = leverage,
            roa = roa,
            tobin_q = tobin_q,
            bid_ask_spread = bid_ask_spread) %>% 
  arrange(report_date, permno)

summary(thirteenf_crsp_compustat_temp)

thirteenf_crsp_compustat <- thirteenf_crsp_compustat_temp %>% 
  select(permno, year, sic_code, inst_percentage, foreign_inst_percentage, domestic_inst_percentage,
         inst_breadth, foreign_breadth, domestic_breadth, market_cap, log_market_cap,
         book_to_market, leverage, roa, tobin_q, bid_ask_spread) %>% 
  filter_all(all_vars(!is.na(.))) %>% # filter all NAs away, since going to be used in regression 
  mutate(inst_percentage = Winsorize(inst_percentage, probs = c(0.01, 0.99)), # winsorize all variables to mitigate outliers
         foreign_inst_percentage = Winsorize(foreign_inst_percentage, probs = c(0.01, 0.99)),
         domestic_inst_percentage = Winsorize(domestic_inst_percentage, probs = c(0.01, 0.99)),
         inst_breadth = Winsorize(inst_breadth, probs = c(0.01, 0.99)),
         foreign_breadth = Winsorize(foreign_breadth, probs = c(0.01, 0.99)),
         domestic_breadth = Winsorize(domestic_breadth, probs = c(0.01, 0.99)),
         market_cap = Winsorize(market_cap, probs = c(0.01, 0.99)),
         book_to_market = Winsorize(book_to_market, probs = c(0.01, 0.99)),
         leverage = Winsorize(leverage, probs = c(0.01, 0.99)),
         roa = Winsorize(roa, probs = c(0.01, 0.99)),
         tobin_q = Winsorize(tobin_q, probs = c(0.01, 0.99)),
         bid_ask_spread = Winsorize(bid_ask_spread, probs = c(0.01, 0.99))) %>% 
  group_by(permno, year) %>% 
  summarise(sic_code = last(sic_code),
            inst_percentage = mean(inst_percentage),
            inst_breadth = mean(inst_breadth),
            foreign_inst_percentage = mean(foreign_inst_percentage),
            foreign_breadth = mean(foreign_breadth),
            domestic_inst_percentage = mean(domestic_inst_percentage),
            domestic_breadth = mean(domestic_breadth),
            log_market_cap = log(mean(market_cap)),
            book_to_market = mean(book_to_market),
            leverage = mean(leverage),
            roa = mean(roa),
            tobin_q = mean(tobin_q),
            bid_ask_spread = mean(bid_ask_spread)) %>% 
  ungroup()

thirteenf_crsp_compustat_ia <- thirteenf_crsp_compustat %>% 
  inner_join(information_asymmetry_measures, by = c("permno", "year"))

write_rds(thirteenf_crsp_compustat_ia_merged, "data/baseline_regression_raw.rds")

## did linking

# first by id:s cusip (and filter NA permnos)

ibes_did <- read_rds("data/ibes_did")

crsp_compustat_keys <- crsp_quarter_stock %>% 
  select(permno, ncusip) %>% 
  distinct()

did_temp1 <- ibes_did %>% 
  left_join(crsp_compustat_keys, by = c("cusip" = "ncusip")) %>% 
  filter(!is.na(permno))

# retrieve data from thirteenf_crsp_compustat-data and do the interval filtering [-15;-3] and [3;15] for all measures
# and link that to ibes firms (treated, control, before and after events)

closures <- read_rds("data/closures.rds")

columns_for_summarise <- c("inst_percentage", "foreign_inst_percentage", "domestic_inst_percentage", 
                           "market_cap", "log_market_cap", "book_to_market", "leverage", "roa", "tobin_q")

columns_for_summarise <- c("foreign_inst_percentage")

year_index <- c(1, 2, 3, 4, 5)

#year_index <- c(1)

before_interval_fun <- function (event_date, year_index) {
  i <- 3 + (year_index - 1) * 12
  j <- 3 + (year_index) * 12
  df <- tibble(event_date = event_date,
               interval = interval(event_date %m-% months(j),event_date %m-% months(i)),
               year_index = year_index)
  df
}

after_interval_fun <- function (event_date, year_index) {
  i <- 3 + (year_index - 1) * 12
  j <- 3 + (year_index) * 12
  df <- tibble(event_date = event_date,
               interval = interval(event_date %m+% months(i),event_date %m+% months(j)),
               year_index = year_index)
  df
}

filter1 <- function (df_measures, df_events){
  interval <- df_events$interval  
  filter(df_measures, report_date %within% interval)
}

summarise1 <- function (df) {
  df %>% 
    group_by(permno, event_date, year_index) %>% 
    summarise_at(columns_for_summarise, funs(mean(., na.rm = TRUE))) %>% 
    ungroup()
}

# map every before_interval (5) to every distinct event_date (20) and flatten to list of 100
# do the filtering for list of data frames based on intervals and add columns event_date and year_index
# do the summarising for the measure of interest (number of distinct analysts in this case)
# and set corresponding after value (before = 0, after = 1)

before_val <- map(year_index, ~map(closures$event_date, ~before_interval_fun(.x, .y), .y = .x)) %>%
  flatten() %>% 
  map(~filter1(thirteenf_crsp_compustat_merged, .x) %>%
        mutate(event_date = .x$event_date,
               year_index = .x$year_index)) %>% 
  map_df(~summarise1(.x)) %>% 
  mutate(after = 0,
         year_index = (-1)*year_index) %>% 
  arrange(permno, event_date, year_index)

after_val <- map(year_index, ~map(closures$event_date, 
                                  ~after_interval_fun(.x, .y), .y = .x)) %>% 
  flatten() %>% 
  map(~filter1(thirteenf_crsp_compustat_merged, .x) %>%
        mutate(event_date = .x$event_date,
               year_index = .x$year_index)) %>% 
  map_df(~summarise1(.x)) %>% 
  mutate(after = 1) %>% 
  arrange(permno, event_date, year_index)


# before_val <- map2(events$before_interval,
#                    events$event_date,
#                    ~filter1(thirteenf_crsp_compustat_merged, .x) %>% mutate(event_date = .y)) %>% 
#   map_df(~summarise1(.x)) %>% 
#   mutate(after = 0)
# 
# after_val <- map2(events$after_interval,
#                   events$event_date,
#                   ~filter1(thirteenf_crsp_compustat_merged, .x) %>% mutate(event_date = .y)) %>% 
#   map_df(~summarise2(.x)) %>% 
#   mutate(after = 1)

vals <- bind_rows(before_val, after_val) %>% 
  arrange(permno, event_date, after)

did <- did_temp1 %>% 
  inner_join(vals, by = c("permno", "event_date", "after", "year_index")) %>% 
  distinct()

write_rds(did, "data/did_regression_raw.rds")
