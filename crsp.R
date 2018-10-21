library(tidyverse)
library(readr)
library(lubridate)

thirteenf <- read_rds("13f_output.rds")

crsp_monthly_stock_raw <- read_tsv("crsp_monthly_stock_file.txt", col_types = cols(.default = "c"))
crsp_monthly_stock <- crsp_monthly_stock_raw %>% 
  transmute(permno = PERMNO,
            date = ymd(date),
            ncusip = NCUSIP,
            official_ticker = TICKER,
            firm = COMNAM,
            cusip = CUSIP,
            price = abs(as.numeric(PRC)),
            shares_outstanding = as.numeric(SHROUT), # in 1000s
            #return = if_else(as.numeric(RET) == c(-66.0, -77.0, -88.0, -99.0), NA, as.numeric(RET)),
            trading_volume = case_when(VOL == "-99" ~ NA,
                                       TRUE ~ as.numeric(VOL)))

crsp_monthly_stock_raw %>% 
  group_by(RET) %>% 
  tally()
