library(tidyverse)
library(readr)
library(lubridate)

# reading CRSP monthly stock file

crsp_monthly_stock_raw <- read_tsv("data/crsp_monthly_stock_file.txt", col_types = cols(.default = "c"))

crsp_monthly_stock <- crsp_monthly_stock_raw %>% 
  transmute(permno = PERMNO,
            date = ymd(date),
            ncusip = NCUSIP,
            official_ticker = TICKER,
            firm = COMNAM,
            cusip = CUSIP,
            price = abs(parse_double(PRC)),
            shares_outstanding = parse_double(SHROUT)*1000, # in 1000s
            return = parse_double(RET), # e.g. value "C" is converted to NA
            trading_volume = parse_double(VOL)) %>% 
  select(-cusip, -official_ticker, -firm)

write_rds(crsp_monthly_stock, "data/crsp_monthly_stock.rds")

# Reading CRSP daily stock file

crsp_daily_stock_raw <- read_tsv("crsp_daily_stock_file.txt", col_types = cols(.default = "c"))

crsp_daily_stock_raw

crsp_daily_stock <- crsp_daily_stock_raw %>% 
  transmute(permno = PERMNO,
            date = ymd(date),
            ncusip = NCUSIP,
            official_ticker = TICKER,
            firm_crsp = COMNAM,
            cusip = CUSIP,
            price = abs(as.numeric(PRC)),
            shares_outstanding = as.numeric(SHROUT), # in 1000s
            return = as.numeric(RET), # e.g. value "C" is converted to NA
            trading_volume = as.numeric(VOL))

crsp_daily_stock

crsp_monthly_stock %>% 
  group_by(permno) %>% 
  tally()

write_rds(crsp_daily_stock, "data/crsp_daily_stock.rds")
