library(tidyverse)
library(readr)
library(lubridate)
library(feather)

# reading 13f institutional holdings, stock ownership summary file, entire database, 1980-01, 2017-012


# df_institutions_raw <- read_tsv("data/13f_institutions.txt", col_types = cols(.default = "c"))
# 
# df_institutions <- df_institutions_raw %>% 
#   group_by(mgrno) %>% 
#   fill(country, .direction = "up") %>% 
#   fill(country, .direction = "down") %>% 
#   ungroup() %>% 
#   select(-mgrname, -permkey)
# 
# write_feather(df_institutions, "data/13_institutions.feather")
# 
# 
# df_raw <- read_tsv("data/13f_s34_master_file.txt", col_types = cols(.default = "c")) %>% 
#   select(-country)
# 
# write_feather(df_raw, "data/13_raw_master.feather")

df_institutions <- read_feather("data/13_institutions.feather") %>% 
  distinct() %>% 
  arrange(rdate)

df <- read_feather("data/13_raw_master.feather") %>% 
  distinct()

df_temp1 <- df %>% 
  left_join(df_institutions)

write_feather(df_temp1, "data/13f_masterfile_merged.feather")

df_temp2 <- read_feather("data/13f_masterfile_merged.feather") %>% 
  transmute(cusip = cusip,
            official_ticker = ticker,
            firm_13f = stkname,
            institutional = mgrno,
            institutional_country = country,
            institutional_type = typecode,
            report_date = ymd(rdate),
            shareholdings_end_qtr = parse_double(shares),
            shares_outstanding_millions = parse_double(shrout1),
            shares_outstanding_thousands = parse_double(shrout2),
            market_price = parse_double(prc))

df_temp3 <- df_temp2 %>% 
  group_by(report_date, cusip) %>% 
  summarise(shares_outstanding = sum(shares_outstanding_thousands)*1000,
            institutional_ownership_shares = sum(shareholdings_end_qtr),
            institutional_ownership_percentage = sum(shareholdings_end_qtr)/(sum(shares_outstanding_thousands)*1000),
            foreign_institutional_ownership_shares = sum(shareholdings_end_qtr[institutional_country != "UNITED STATES"]),
            foreign_institutional_ownership_percentage = sum(shareholdings_end_qtr[institutional_country != "UNITED STATES"])/(sum(shares_outstanding_thousands)*1000),
            domestic_institutional_ownership_shares = sum(shareholdings_end_qtr[institutional_country == "UNITED STATES"]),
            domestic_institutional_ownership_percentage = sum(shareholdings_end_qtr[institutional_country == "UNITED STATES"])/(sum(shares_outstanding_thousands)*1000)) %>% 
  ungroup()  

# think whether to calculate ownership with market prices (values) (CRSP), since lot of NA values in shares outstanding 


write_feather(df_temp3, "data/13f_output.feather")
write_rds(df_temp3, "data/13f_output.rds")

