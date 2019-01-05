library(tidyverse)
library(readr)
library(lubridate)

thirteenf <- read_rds("data/13f_output.rds") %>% 
  mutate(report_date = ceiling_date(report_date, unit = "month") - days(1)) %>% 
  select(-shares_outstanding, -institutional_ownership_percentage, -foreign_institutional_ownership_percentage, 
         -domestic_institutional_ownership_percentage) %>% 
  distinct() %>% 
  filter(!(is.na(foreign_institutional_ownership_shares))) %>% 
  filter(!(is.na(cusip)))

# crsp_monthly_stock <- read_rds("data/crsp_monthly_stock.rds") %>% 
#   select(-return, -trading_volume, -ncusip) %>% 
#   distinct() %>% 
#   group_by(permno) %>% 
#   fill(shares_outstanding, .direction = "up") %>% 
#   mutate(date = ceiling_date(date, unit = "month") - days(1)) %>% 
#   ungroup()
# 
# write_rds(crsp_monthly_stock, "data/crsp_monthly_stock_final.rds")

crsp_monthly_stock_final <- read_rds("data/crsp_monthly_stock_final.rds")

# crsp_monthly_stock_linking <- read_rds("data/crsp_monthly_stock.rds") %>% 
#   select(date, ncusip, permno) %>% 
#   distinct() %>% 
#   group_by(permno) %>% 
#   fill(ncusip, .direction = "up") %>% 
#   ungroup() %>% 
#   mutate(date = ceiling_date(date, unit = "month") - days(1))

# write_rds(crsp_monthly_stock_linking, "data/crsp_monthly_stock_linking_final.rds")

crsp_monthly_stock_linking_final <- read_rds("data/crsp_monthly_stock_linking_final.rds")

treated_firms_ibes <- read_rds("data/treated_firms_ibes.rds")

# thirteenf_temp1 <- thirteenf %>%
#   left_join(crsp_monthly_stock_linking_final, by = c("cusip" = "ncusip", "report_date" = "date")) %>%
#   group_by(cusip) %>%
#   fill(permno, .direction = "up") %>%
#   ungroup() %>%
#   filter(!(is.na(permno))) %>%
#   left_join(crsp_monthly_stock_final, by = c("permno", "report_date" = "date")) %>%
#   select(-price) %>%
#   arrange(report_date, permno) %>% 
#   group_by(permno) %>% 
#   fill(shares_outstanding, .direction = "up") %>% 
#   ungroup() %>% 
#   filter(!(is.na(shares_outstanding)))

# write_rds(thirteenf_temp1, "data/thirteenf_crsp_merged.rds")

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
