library(tidyverse)
library(readr)
library(lubridate)

# annual data

compustat_annual_raw <- read_tsv("data/crsp_compustat_merged_annual.txt", col_types = cols(.default = "c"))

compustat_annual <- compustat_annual_raw %>% 
  transmute(gvkey = GVKEY,
            permno = LPERMNO,
            firm = conm,
            datadate = ymd(datadate),
            fiscal_year = parse_integer(fyear),
            fiscal_year_end_month = parse_integer(fyr),
            total_assets = parse_double(at),
            bookvaluepershare = parse_double(bkvlps),
            common_equity = parse_double(ceq),
            common_equity_liquidation_value = parse_double(ceql),
            common_stock = parse_double(cstk),
            shares_outstanding = parse_double(csho),
            debt_in_current_liabilities = parse_double(dlc),
            debt_long_term = parse_double(dltt),
            debt_liabilities = parse_date(lt),
            net_income = parse_double(ni),
            preferred_stock = parse_double(pstk),
            preferred_stock_convertible = parse_double(pstkc),
            preferred_stock_liquidation_value = parse_double(pstkl),
            sales = parse_double(sale), # = turnover
            stockholders_equity = parse_double(seq), # common equity (ceq) + preferred stock (pstk), lot of NA
            price_close_calendar = parse_double(prcc_c),
            price_close_fiscal = parse_double(prcc_f),
            market_cap = parse_double(mkvalt), # lot of NA
            exchange_code = exchg,
            sic_code_historical = sich,
            sic_code = sic,
            gics_sector = gsector,
            gics_group = ggroup,
            gics_industry = gind,
            gics_subindustry = gsubind)

compustat_annual_final <- compustat_annual %>% 
  rowwise() %>% 
  mutate(market_cap = price_close_calendar * shares_outstanding,
         log_market_cap = log(market_cap),
         shareholders_equity = if_else(
           !is.na(stockholders_equity) == T, stockholders_equity,
           sum(common_equity + preferred_stock, na.rm = T)),
         book_equity = shareholders_equity - coalesce(preferred_stock_liquidation_value, preferred_stock, 0),
         book_to_market = book_equity / market_cap,
         leverage = (debt_in_current_liabilities + debt_long_term) / total_assets) %>% 
  ungroup()


# compustat quarter data

compustat_quarter_raw <- read_tsv("data/crsp_compustat_merged_quarter.txt", col_types = cols(.default = "c"))

compustat_quarter <- compustat_quarter_raw %>% 
  transmute(gvkey = GVKEY,
            permno = LPERMNO,
            firm = conm,
            datadate = ymd(datadate),
            fiscal_year = parse_integer(fyearq),
            fiscal_year_end_month = parse_integer(fyr),
            fiscal_quarter = parse_integer(fqtr),
            assets = parse_double(atq),
            #bookvaluepershare = parse_double(bkvlps),
            common_equity = parse_double(ceqq),
            #common_equity_liquidation_value = parse_double(ceql),
            common_stock = parse_double(cstkq),
            shares_outstanding = parse_double(cshoq),
            debt_in_current_liabilities = parse_double(dlcq),
            debt_long_term = parse_double(dlttq),
            debt_liabilities = parse_date(ltq),
            net_income = parse_double(niq),
            preferred_stock = parse_double(pstkq),
            #preferred_stock_convertible = parse_double(pstkc),
            #preferred_stock_liquidation_value = parse_double(pstklq),
            sales = parse_double(saleq), # = turnover
            stockholders_equity = parse_double(teqq), # common equity (ceq) + preferred stock (pstk), lot of NA
            price_close_calendar = parse_double(prccq),
            #price_close_fiscal = parse_double(prcc_fq),
            market_cap = parse_double(mkvaltq), # lot of NA
            exchange_code = exchg,
            #sic_code_historical = sich,
            sic_code = sic,
            gics_sector = gsector,
            gics_group = ggroup,
            gics_industry = gind,
            gics_subindustry = gsubind)
