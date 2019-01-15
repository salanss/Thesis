library(tidyverse)
library(readr)
library(lubridate)

# baseline linking

thirteenf <- read_rds("data/13f_output.rds") %>% 
  mutate(report_date = ceiling_date(report_date, unit = "month") - days(1)) %>% 
  select(-institutional_ownership_percentage, -foreign_institutional_ownership_percentage, 
         -domestic_institutional_ownership_percentage) %>% 
  filter(!(is.na(foreign_institutional_ownership_shares))) %>% 
  filter(!(is.na(cusip))) %>% 
  distinct()

crsp_monthly_stock <- read_rds("data/crsp_monthly_stock.rds")

compustat_quarter <- read_rds("data/compustat_quarter_final.rds") %>% 
  mutate(datadate = ceiling_date(datadate, unit = "month") - days(1)) # sanity check

information_asymmetry_measures <- read_rds("data/information_asymmetry_measures.rds")

thirteenf_crsp_merged <- thirteenf %>%
  inner_join(crsp_monthly_stock, by = c("cusip" = "ncusip", "report_date" = "date"))

thirteenf_crsp_compustat_merged <- thirteenf_crsp_merged %>% 
  inner_join(compustat_quarter, by = c("permno", "report_date" = "datadate")) %>% 
  transmute(report_date = report_date,
            year = year(report_date),
            permno = permno,
            cusip = cusip,
            sic_code = if_else(sic_code.x == "0" | is.na(sic_code.x) == T, sic_code.y, sic_code.x),
            shares_outstanding = shares_outstanding.y, #crsp 
                                              # coalesce(shares_outstanding.x, shares_outstanding.y, shares_outstanding)
            institutional_ownership_shares = institutional_ownership_shares,
            foreign_institutional_ownership_shares = foreign_institutional_ownership_shares,
            domestic_institutional_ownership_shares = domestic_institutional_ownership_shares,
            inst_percentage = institutional_ownership_shares / shares_outstanding,
            foreign_inst_percentage = foreign_institutional_ownership_shares / shares_outstanding,
            domestic_inst_percentage = domestic_institutional_ownership_shares / shares_outstanding,
            price = price, # coalesce(price, price_close_calendar)
            market_cap = market_cap.x/1000000, # crsp to same units as compustat
            log_market_cap = log(market_cap),
            book_equity = book_equity,
            book_to_market = book_to_market,
            leverage = leverage,
            roa = roa,
            tobin_q = tobin_q) %>% 
  arrange(report_date, permno)

thirteenf_crsp_compustat_ia_merged <- thirteenf_crsp_compustat_merged %>% 
  select(permno, year, sic_code, inst_percentage, foreign_inst_percentage, domestic_inst_percentage,
         market_cap, log_market_cap, book_equity, book_to_market, leverage, roa, tobin_q) %>% 
  filter_all(all_vars(!is.na(.))) %>% # filter all NAs away, since going to be used in regression 
  group_by(permno, year, sic_code) %>% 
  summarise(inst_percentage = mean(inst_percentage),
            foreign_inst_percentage = mean(foreign_inst_percentage),
            domestic_inst_percentage = mean(domestic_inst_percentage),
            log_market_cap = log(mean(market_cap)),
            book_to_market = mean(book_to_market),
            leverage = mean(leverage),
            roa = mean(roa),
            tobin_q = mean(tobin_q)) %>% 
  ungroup() %>% 
  inner_join(information_asymmetry_measures, by = c("permno", "year"))

write_rds(thirteenf_crsp_compustat_ia_merged, "data/baseline_regression_raw.rds")

## did linking

# first by id:s cusip (and filter NA permnos)

ibes_did <- read_rds("data/ibes_did")

crsp_compustat_keys <- thirteenf_crsp_compustat_merged %>% 
  select(permno, cusip, sic_code) %>% 
  distinct()

did_temp1 <- ibes_did %>% 
  left_join(crsp_compustat_keys, by = "cusip") %>% 
  filter(!is.na(permno))

# retrieve data from thirteenf_crsp_compustat-data and do the interval filtering [-15;-3] and [3;15] for all measures
# and link that to ibes firms (treated, control, before and after events)

events <- read_rds("data/events.rds")

columns_for_summarise <- c("inst_percentage", "foreign_inst_percentage", "domestic_inst_percentage", 
                           "market_cap", "log_market_cap", "book_to_market", "leverage", "roa", "tobin_q")

filter1 <- function (df, interval){
  filter(df, report_date %within% interval)
}

summarise_mean <- function (df) {
  df %>% 
    group_by(permno, event_date) %>% 
    summarise_at(columns_for_summarise, mean) %>% 
    ungroup()
}

before_val <- map2(events$before_interval,
                   events$event_date,
                   ~filter1(thirteenf_crsp_compustat_merged, .x) %>% mutate(event_date = .y)) %>% 
  map_df(~summarise_mean(.x)) %>% 
  mutate(after = 0)

after_val <- map2(events$after_interval,
                  events$event_date,
                  ~filter1(thirteenf_crsp_compustat_merged, .x) %>% mutate(event_date = .y)) %>% 
  map_df(~summarise_mean(.x)) %>% 
  mutate(after = 1)

vals <- bind_rows(before_val, after_val) %>% 
  arrange(permno, event_date, after)

did <- did_temp1 %>% 
  inner_join(vals, by = c("permno", "event_date", "after"))

write_rds(did, "data/did_regression_raw.rds")
