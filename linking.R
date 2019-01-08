library(tidyverse)
library(readr)
library(lubridate)

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

treated_firms_ibes <- read_rds("data/treated_firms_ibes.rds")

thirteenf_crsp_merged <- thirteenf %>%
  inner_join(crsp_monthly_stock, by = c("cusip" = "ncusip", "report_date" = "date"))

thirteenf_crsp_compustat_quarter_merged <- thirteenf_crsp_merged %>% 
  inner_join(compustat_quarter, by = c("permno", "report_date" = "datadate")) %>% 
  transmute(report_date = report_date,
            permno = permno,
            cusip = cusip,
            shares_outstanding = shares_outstanding.y, #coalesce(shares_outstanding.x, shares_outstanding.y, shares_outstanding)
            institutional_ownership_shares = institutional_ownership_shares,
            foreign_institutional_ownership_shares = foreign_institutional_ownership_shares,
            domestic_institutional_ownership_shares = domestic_institutional_ownership_shares,
            inst_percentage = institutional_ownership_shares / shares_outstanding,
            foreign_inst_percentage = foreign_institutional_ownership_shares / shares_outstanding,
            sic_code = if_else(sic_code.x == "0" | is.na(sic_code.x) == T, sic_code.y, sic_code.x),
            price = price, # coalesce(price, price_close_calendar)
            market_cap = shares_outstanding * price,
            log_market_cap = log(market_cap),
            book_equity = book_equity,
            book_to_market = book_to_market,
            leverage = leverage,
            roa = roa,
            tobin_q = tobin_q) %>% 
  arrange(report_date, permno)

thirteenf_crsp_merged <- read_rds("data/thirteenf_crsp_merged.rds") %>% 
  transmute(report_date = report_date, 
            year = year(report_date),
            permno = permno, 
            cusip = cusip,
            inst_percentage = institutional_ownership_shares/shares_outstanding,
            foreign_inst_percentage = foreign_institutional_ownership_shares/shares_outstanding,
            domestic_inst_percentage = domestic_institutional_ownership_shares/shares_outstanding) %>% 
  arrange(report_date)

information_asymmetry_measures <- read_rds("data/information_asymmetry_measures.rds")

thirteenf_crsp_ia_merged <- thirteenf_crsp_merged %>% 
  group_by(permno, year) %>% 
  summarise(inst_percentage = mean(inst_percentage),
            foreign_inst_percentage = mean(foreign_inst_percentage),
            domestic_inst_percentage = mean(domestic_inst_percentage)) %>% 
  ungroup() %>% 
  inner_join(information_asymmetry_measures, by = c("permno", "year"))

write_rds(thirteenf_crsp_ia_merged, "data/baseline_regression_raw.rds")

##

ibes_crsp_link1 <- left_join(treated_firms_ibes, crsp_monthly_stock, by = c("cusip" = "ncusip")) %>% 
  select(-date, -price, -shares_outstanding, -return, -trading_volume) %>%
  distinct()

ibes_crsp_link1 %>% 
  group_by(permno) %>% 
  tally()

# link for 13F (only event_date and permno)

ibes_crsp_link2 <- ibes_crsp_link1 %>% 
  select(permno, event_date) %>% 
  distinct()

thirteenf_ibes_link1 <- left_join(thirteenf_temp1, ibes_crsp_link2, by = "permno") %>% 
  select(permno, report_date, foreign_institutional_ownership_percentage, event_date) %>%
  filter(!is.na(event_date)) %>% 
  mutate(yearbefore = report_date %within% interval(event_date %m-% months(15), event_date %m-% months(3) + days(3)),
         yearafter = report_date %within% interval(event_date %m+% months(3) - days(3), event_date %m+% months(15))) %>% 
  arrange(permno, report_date, event_date)

thirteenf_ibes_link2 <- thirteenf_ibes_link1 %>% 
  filter(yearbefore == T) %>% 
  group_by(permno, event_date) %>% 
  summarise(foreign_holdings = mean(foreign_institutional_ownership_percentage)) %>% 
  ungroup()

thirteenf_ibes_link3 <- thirteenf_ibes_link1 %>% 
  filter(yearafter == T) %>% 
  group_by(permno, event_date) %>% 
  summarise(foreign_holdings = mean(foreign_institutional_ownership_percentage)) %>% 
  ungroup()

crsp_monthly_stock %>% 
  group_by(permno) %>% 
  tally()
