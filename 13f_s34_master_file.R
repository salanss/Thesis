library(tidyverse)
library(readr)
library(lubridate)

# reading 13f institutional holdings, stock ownership summary file, entire database, 1999-01, 2010-01


df_raw2 <- read_tsv("13f_s34_master_file.txt", col_types = cols(.default = "c"))

df_testi <- df_raw2 %>% 
  filter(country != "UNITED STATES")

df_testi %>% 
  group_by(rdate) %>% 
  tally()

df_temp1 <- df_raw2 %>% 
  transmute(cusip = cusip,
            official_ticker = ticker,
            

