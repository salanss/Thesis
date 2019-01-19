library(tidyverse)
library(readr)
library(lubridate)
library(feather)

# reading 13f institutional holdings, stock ownership summary file, entire database, 1980-01, 2017-012


df_institutions_raw <- read_tsv("data/13f_institutions.txt", col_types = cols(.default = "c"))

df_institutions <- df_institutions_raw %>% 
  transmute(file_date = ymd(fdate),
            report_date = ymd(rdate),
            country = country,
            mgrno = mgrno,
            manager = mgrname) %>% 
  filter(report_date >= ymd(19971231)) %>% 
  group_by(mgrno) %>% 
  fill(country, .direction = "down") %>% 
  fill(country, .direction = "up") %>% 
  ungroup() %>% 
  filter(!is.na(country))

df_institutions_final <- df_institutions %>% 
  group_by(mgrno, report_date, country) %>% 
  summarise(file_date = min(file_date)) %>% 
  ungroup()

write_rds(df_institutions_final, "data/13f_institutions.rds")

df_raw <- read_tsv("data/13f_holdings.txt", col_types = cols(.default = "c"))

df_holdings <- df_raw %>% 
  transmute(file_date = ymd(fdate),
            cusip = cusip,
            mgrno = mgrno,
            shareholdings = parse_double(shares))


write_feather(df_holdings, "data/13f_holdings.feather")


df_institutions <- read_rds("data/13f_institutions.rds") %>% 
  distinct()

df <- read_feather("data/13f_holdings.feather") %>% 
  distinct()

df_temp1 <- df %>% 
  left_join(df_institutions, by = c("mgrno", "file_date"))

write_feather(df_temp1, "data/13f_masterfile_merged.feather")

df_temp2 <- read_feather("data/13f_masterfile_merged.feather") %>% 
  transmute(cusip = cusip,
            report_date = report_date,
            file_date = file_date,
            institutional = mgrno,
            institutional_country = country,
            shareholdings_end_qtr = shareholdings)

## crsp link

crsp_quarter_stock <- read_rds("data/crsp_quarter_stock.rds") %>% 
  select(permno, ncusip) %>% 
  distinct()

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

