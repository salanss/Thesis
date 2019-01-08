library(tidyverse)
library(readr)
library(lubridate)

# reading CRSP monthly stock file

crsp_monthly_stock_raw <- read_tsv("data/crsp_monthly_stock_file.txt", col_types = cols(.default = "c"))

crsp_monthly_stock <- crsp_monthly_stock_raw %>% 
  transmute(permno = PERMNO,
            date = ceiling_date(ymd(date), unit = "month") - days(1),
            ncusip = NCUSIP,
            official_ticker = TICKER,
            firm = COMNAM,
            sic_code = SICCD, # historical
            exchange_code = EXCHCD,
            price = if_else(is.na(PRC), abs(parse_double(DLPRC)),
                            abs(parse_double(PRC))),
            price_delisting = abs(parse_double(DLPRC)),
            bid = abs(parse_double(BIDLO)),
            ask = abs(parse_double(ASKHI)),
            bid_ask_spread = (ask - bid)/price,
            shares_outstanding = parse_double(SHROUT)*1000, # in 1000s
            return = if_else(is.na(RET), parse_double(DLRET),
                             parse_double(RET)), # e.g. value "C" is converted to NA
            return_delisting = parse_double(DLRET),
            trading_volume = parse_double(VOL)) %>% 
  select(-official_ticker, -firm) %>% 
  filter(exchange_code %in% c("1", "2", "3")) %>% # keeping only NYSE, AMEX and Nasdaq
  filter(!is.na(ncusip)) %>% 
  distinct()

write_rds(crsp_monthly_stock, "data/crsp_monthly_stock.rds")

# Reading CRSP daily stock file

crsp_daily_stock_raw <- read_tsv("crsp_daily_stock_file.txt", col_types = cols(.default = "c"))

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
