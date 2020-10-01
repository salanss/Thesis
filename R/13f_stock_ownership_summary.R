library(tidyverse)
library(readr)
library(lubridate)

# reading 13f institutional holdings, stock ownership summary file, entire database, 1999-01, 2010-01

df_raw <- read_tsv("data/13f_stock_ownership_summary.txt", col_types = cols(.default = "c"))
# renaming, formatting and selecting the variables

df_temp1 <- df_raw %>% 
  transmute(report_date = ymd(rdate),
            cusip = cusip,
            official_ticker = ticker,
            firm = stkname,
            shares_outstanding_qtr = parse_double(shrout), # as in 1000s
            top5_institutionals = parse_double(Top5InstOwn),
            top10_institutional = parse_double(Top10InstOwn),
            number_of_institutional_blockholders = parse_double(NumInstBlockOwners), # blockholder means ownership above 5% threshold
            ownership_of_institutional_blockholders = parse_double(InstBlockOwn),
            number_of_institutionals = parse_double(NumInstOwners),
            ownership_of_institutionals = parse_double(InstOwn),
            ownership_of_largest_institutional = parse_double(MaxInstOwn),
            index_of_institutional_ownership = parse_double(InstOwn_HHI),
            ownership_of_institutionals_percentage = parse_double(InstOwn_Perc)) # as a share of shares outstanding

df_temp2 <- df_temp1 %>% 
  filter(!is.na(ownership_of_institutionals_percentage))
