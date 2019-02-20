library(tidyverse)
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
  filter(file_date >= ymd(19970630)) %>% 
  group_by(mgrno) %>% 
  fill(country, .direction = "down") %>% 
  fill(country, .direction = "up") %>% 
  ungroup() %>% 
  filter(!is.na(country))

institutions_types <- read_rds("data/investors_types.rds")

df_institutions_final <- df_institutions %>% 
  group_by(mgrno, report_date, country) %>% 
  summarise(file_date = min(file_date)) %>% 
  ungroup() %>% 
  mutate(report_year = year(report_date)) %>% 
  left_join(institutions_types, by = c("report_year" = "year", "mgrno" = "institutional_code"))

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
  filter(file_date >= ymd(19970630)) %>% 
  distinct()

df_temp1 <- df %>% 
  right_join(df_institutions, by = c("mgrno", "file_date"))

write_feather(df_temp1, "data/13f_masterfile_merged.feather")

df_temp2 <- read_feather("data/13f_masterfile_merged.feather") %>% 
  transmute(cusip = cusip,
            report_date = report_date,
            file_date = file_date,
            institutional = mgrno,
            institutional_country = country,
            investor_type = investor_type,
            shareholdings_end_qtr = shareholdings)

## crsp link

crsp_quarter_stock_link <- read_rds("data/crsp_quarter_stock.rds") %>% 
  select(permno, ncusip) %>% 
  distinct()

crsp_quarter_stock_link2 <- read_rds("data/crsp_quarter_stock.rds") %>% 
  select(permno, quarter_date, cumulative_shares_factor) %>% 
  distinct()

df_temp3 <- df_temp2 %>% 
  inner_join(crsp_quarter_stock_link, by = c("cusip" = "ncusip"))

df_temp4 <- df_temp3 %>% 
  inner_join(crsp_quarter_stock_link2, by = c("permno", "file_date" = "quarter_date"))

write_rds(df_temp4, "data/13f_crsp_merged.rds")

df_temp5 <- read_rds("data/13f_crsp_merged.rds") %>% 
  mutate(shareholdings_adjusted = if_else(is.na(cumulative_shares_factor), 
                                          shareholdings_end_qtr, shareholdings_end_qtr * cumulative_shares_factor))

write_rds(df_temp5, "data/13f_crsp_merged_final.rds")

df_temp6 <- read_rds("data/13f_crsp_merged_final.rds") %>% 
  group_by(permno, report_date) %>% 
  summarise(institutional_ownership_shares = sum(shareholdings_adjusted),
            institutional_shares_nonquasi = sum(shareholdings_adjusted[investor_type != "QIX"]),
            institutional_shares_dedicated = sum(shareholdings_adjusted[investor_type == "DED"]),
            foreign_institutional_ownership_shares = sum(shareholdings_adjusted[institutional_country != "UNITED STATES"]),
            foreign_shares_nonquasi = sum(shareholdings_adjusted[institutional_country != "UNITED STATES" & investor_type != "QIX"]),
            foreign_shares_dedicated = sum(shareholdings_adjusted[institutional_country != "UNITED STATES" & investor_type == "DED"]),
            domestic_institutional_ownership_shares = sum(shareholdings_adjusted[institutional_country == "UNITED STATES"]),
            domestic_shares_nonquasi = sum(shareholdings_adjusted[institutional_country == "UNITED STATES" & investor_type != "QIX"]),
            domestic_shares_dedicated = sum(shareholdings_adjusted[institutional_country == "UNITED STATES" & investor_type == "DED"]),
            institutional_numbers = n_distinct(institutional),
            institutional_numbers_nonquasi = n_distinct(institutional[investor_type != "QIX"]),
            institutional_numbers_dedicated = n_distinct(institutional[investor_type == "DED"]),
            foreign_institutional_numbers = n_distinct(institutional[institutional_country != "UNITED STATES"]),
            foreign_numbers_nonquasi = n_distinct(institutional[institutional_country != "UNITED STATES" & investor_type != "QIX"]),
            foreign_numbers_dedicated = n_distinct(institutional[institutional_country != "UNITED STATES" & investor_type == "DED"]),
            domestic_institutional_numbers = n_distinct(institutional[institutional_country == "UNITED STATES"]),
            domestic_numbers_nonquasi = n_distinct(institutional[institutional_country == "UNITED STATES" & investor_type != "QIX"]),
            domestic_numbers_dedicated = n_distinct(institutional[institutional_country == "UNITED STATES" & investor_type == "DED"])) %>% 
  ungroup()

df_temp7 <- read_rds("data/13f_crsp_merged_final.rds") %>% 
  group_by(report_date) %>% 
  summarise(institutional_numbers = n_distinct(institutional),
            institutional_numbers_nonquasi = n_distinct(institutional[investor_type != "QIX"]),
            institutional_numbers_dedicated = n_distinct(institutional[investor_type == "DED"])) %>% 
  ungroup()

# think whether to calculate ownership with market prices (values) (CRSP), since lot of NA values in shares outstanding 

write_feather(df_temp6, "data/13f_output.feather")
write_rds(df_temp6, "data/13f_output.rds")

write_rds(df_temp7, "data/13f_institutionals_quarterly.rds")

