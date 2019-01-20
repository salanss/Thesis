library(tidyverse)
library(readr)
library(lubridate)

# reading CRSP monthly stock file

crsp_monthly_stock_raw <- read_tsv("data/crsp_monthly_stock_file.txt", col_types = cols(.default = "c"))

crsp_monthly_stock <- crsp_monthly_stock_raw %>% 
  transmute(permno = PERMNO,
            date = ceiling_date(ymd(date), unit = "month") - days(1),
            ncusip = NCUSIP,
            firm = COMNAM,
            sic_code = SICCD, # historical
            exchange_code = EXCHCD,
            cumulative_price_factor = if_else(parse_double(CFACPR) == 0,
                                              NA_real_, parse_double(CFACPR)),
            price = if_else(is.na(PRC), abs(parse_double(DLPRC)),
                            abs(parse_double(PRC))),
            price_adjusted = if_else(is.na(cumulative_price_factor),
                                     price, price / cumulative_price_factor),
            price_delisting = abs(parse_double(DLPRC)),
            bid = abs(parse_double(BIDLO)),
            ask = abs(parse_double(ASKHI)),
            bid_ask_spread = if_else(price == 0, NA_real_, (ask - bid)/price),
            cumulative_shares_factor = parse_double(CFACSHR),
            shares_outstanding = parse_double(SHROUT)*1000, # in 1000s
            shares_outstanding_adjusted = if_else(is.na(cumulative_shares_factor),
                                                  shares_outstanding, shares_outstanding * cumulative_shares_factor),
            market_cap = price_adjusted * shares_outstanding_adjusted / 1000000,
            return = if_else(is.na(RET), parse_double(DLRET),
                             parse_double(RET)), # e.g. value "C" is converted to NA
            return_delisting = parse_double(DLRET),
            trading_volume = parse_double(VOL)) %>% 
  select(-firm) %>% 
  filter(exchange_code %in% c("1", "2", "3")) %>% # keeping only NYSE, AMEX and Nasdaq
  filter(!is.na(ncusip)) %>% 
  distinct()

crsp_quarter_stock <- crsp_monthly_stock %>% 
  mutate(quarter_date = ceiling_date(date, "quarter") - days(1)) %>% 
  group_by(permno, quarter_date, ncusip) %>% 
  summarise_all(last) %>% 
  ungroup() %>% 
  select(-date) %>% 
  distinct()

write_rds(crsp_quarter_stock, "data/crsp_quarter_stock.rds")

# Reading CRSP daily stock file

crsp_daily_stock_raw <- read_tsv("data/crsp_daily_stock_file", col_types = cols(.default = "c"))

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
