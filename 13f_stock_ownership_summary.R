library(tidyverse)
library(readr)
library(lubridate)

# reading 13f institutional holdings, stock ownership summary file, entire database, 1999-01, 2010-01

df_raw <- read_tsv("13f.txt", col_types = cols(.default = "c"))
# renaming, formatting and selecting the variables

df_temp1 <- df_raw %>% 
  transmute(report_date = ymd(rdate),
            cusip = cusip,
            official_ticker = ticker,
            firm = stkname,
            shares_outstanding_qtr = as.numeric(shrout), # as in 1000s
            top5_institutionals = as.numeric(Top5InstOwn),
            top10_institutional = as.numeric(Top10InstOwn),
            number_of_institutional_blockholders = as.numeric(NumInstBlockOwners), # blockholder means ownership above 5% threshold
            ownership_of_institutional_blockholders = as.numeric(InstBlockOwn),
            number_of_institutionals = as.numeric(NumInstOwners),
            ownership_of_institutionals = as.numeric(InstOwn),
            ownership_of_largest_institutional = as.numeric(MaxInstOwn),
            index_of_institutional_ownership = as.numeric(InstOwn_HHI),
            ownership_of_institutionals_percentage = as.numeric(InstOwn_Perc)) # as a share of shares outstanding

summary(df_temp1)
