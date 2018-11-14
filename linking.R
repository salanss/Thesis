library(tidyverse)
library(readr)
library(lubridate)

thirteenf <- read_rds("data/13f_output.rds")
crsp_monthly_stock <- read_rds("data/crsp_monthly_stock.rds")
treated_firms_ibes <- read_rds("data/treated_firms_ibes.rds")


thirteenf_temp1 <- left_join(thirteenf, crsp_monthly_stock, by = c("cusip" = "ncusip", "report_date" = "date")) %>% 
  filter(!is.na(permno))

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
