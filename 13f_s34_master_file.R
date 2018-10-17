library(tidyverse)
library(readr)
library(lubridate)

# reading 13f institutional holdings, stock ownership summary file, entire database, 1999-01, 2010-01


df_raw <- read_tsv("13f_s34_master_file.txt", col_types = cols(.default = "c"))

df_raw

df_testi <- df_raw %>% 
  filter(country != "UNITED STATES")

df_testi %>% 
  group_by(rdate) %>% 
  tally()

df_temp1 <- df_raw2 %>% 
  transmute(cusip = cusip,
            official_ticker = ticker,
            firm = stkname,
            institutional = mgrno,
            institutional_country = country,
            institutional_type = typecode,
            report_date = ymd(rdate),
            shareholdings_end_qtr = as.numeric(shares),
            shares_outstanding_millions = as.numeric(shrout1),
            shares_outstanding_thousands = as.numeric(shrout2),
            market_price = as.numeric(prc))

df_testi <- df_temp1 %>% 
  filter(country != "UNITED STATES")

df_testi %>% 
  group_by(rdate) %>% 
  tally()

