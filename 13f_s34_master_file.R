library(tidyverse)
library(readr)
library(lubridate)

# reading 13f institutional holdings, stock ownership summary file, entire database, 1980-01, 2017-012


df_raw <- read_tsv("data/13f_s34_master_file.txt", col_types = cols(.default = "c"))
df_institutions_raw <- read_tsv("data/13f_institutions.txt", col_types = cols(.default = "c"))

df_institutions <- df_institutions_raw %>% 
  group_by(mgrno) %>% 
  fill(country, .direction = "up") %>% 
  fill(country, .direction = "down") %>% 
  ungroup()
  
testi <- df_institutions %>%
  filter(!(is.na(country))) %>%
  group_by(mgrno) %>% 
  tally()

df_institutions_temp2 <- df_institutions %>% 
  select(mgrno, country) %>% 
  filter(!is.na(country)) %>% 
  filter(!duplicated(mgrno))

df_institutions_temp3 <- df_institutions %>% 
  filter(!is.na(country)) %>% 
  group_by(mgrno, country) %>% 
  summarise(max(rdate)) %>% 
  ungroup() %>% 
  filter(duplicated(mgrno))

duplicated_list <- list(df_institutions_temp3$mgrno) %>% 
  flatten_chr()

df_institutions_temp4 <- df_institutions %>% 
  select(mgrno, country, rdate) %>% 
  mutate(in_list = mgrno %in% duplicated_list) %>% 
  filter(in_list == T) %>% 
  filter(!is.na(country)) %>% 
  distinct() %>% 
  arrange(mgrno) %>% 
  group_by(mgrno, rdate) %>% 
  summarise(country = last(country)) %>% 
  ungroup()

df_institutions_temp5 <- df_institutions %>%
  select(-country)

df_institutions_temp5 <- df_institutions %>%
  select(-country) %>% 
  left_join(df_institutions_temp2)

df_institutions_temp6 <- df_institutions_temp5 %>% 
  left_join(df_institutions_temp4)

df_institutions_temp7 <- df_institutions_temp6 %>% 
  filter(is.na(country))

df_institutions_temp8 <- df_institutions_temp4 %>% 
  group_by(mgrno) %>% 
  summarise(country = first(country)) %>% 
  ungroup()

df_institutions_temp9 <- df_institutions_temp7 %>% 
  select(-country) %>% 
  left_join(df_institutions_temp8) %>% 
  filter(!is.na(country))

df_temp1 <- df_raw %>% 
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

testi <- df_temp1 %>% group_by(institutional_country) %>% 
  tally()

df_temp2 <- df_temp1 %>% 
  group_by(report_date, cusip) %>% 
  summarise(shares_outstanding = sum(shares_outstanding_thousands)*1000,
            institutional_ownership_shares = sum(shareholdings_end_qtr),
            institutional_ownership_percentage = sum(shareholdings_end_qtr)/(sum(shares_outstanding_thousands)*1000),
            foreign_institutional_ownership_shares = sum(shareholdings_end_qtr[institutional_country != "UNITED STATES"]),
            foreign_institutional_ownership_percentage = sum(shareholdings_end_qtr[institutional_country != "UNITED STATES"])/(sum(shares_outstanding_thousands)*1000),
            domestic_institutional_ownership_shares = sum(shareholdings_end_qtr[institutional_country == "UNITED STATES"]),
            domestic_institutional_ownership_percentage = sum(shareholdings_end_qtr[institutional_country == "UNITED STATES"])/(sum(shares_outstanding_thousands)*1000))
  
# think whether to calculate ownership with market prices (values) (CRSP), since lot of NA values in shares outstanding 

df_temp3 <- df_temp1 %>% 
  top_n(100)

write_rds(df_temp2, "data/13f_output.rds")

write_rds(df_temp3, "data/13f_raw_filtered.rds")

