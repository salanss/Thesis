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
            assets = parse_double(at),
            bookvaluepershare = parse_double(bkvlps),
            common_equity = parse_double(ceq),
            common_equity_liquidation_value = parse_double(ceql),
            common_stock = parse_double(cstk),
            shares_outstanding = parse_double(csho),
            debt_in_current_liabilities = parse_double(dlc),
            debt_long_term = parse_double(dltt),
            debt_liabilities = parse_double(lt),
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
         leverage = (debt_in_current_liabilities + debt_long_term) / assets,
         roa = net_income / assets,
         sales = sales,
         tobin_q = (assets + market_cap - book_equity) / assets) %>% 
  ungroup()

write_rds(compustat_annual_final, "data/compustat_annual_final.rds")

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
            debt_liabilities = parse_double(ltq),
            net_income = parse_double(niq),
            preferred_stock = parse_double(pstkq),
            #preferred_stock_convertible = parse_double(pstkc),
            #preferred_stock_liquidation_value = parse_double(pstklq),
            sales = parse_double(saleq), # = turnover
            stockholders_equity1 = parse_double(seqq),
            stockholders_equity2 = parse_double(teqq), # common equity (ceq) + preferred stock (pstk), lot of NA
            price_close_calendar = parse_double(prccq),
            deferred_taxes_inv_credit = parse_double(txditcq),
            #price_close_fiscal = parse_double(prcc_fq),
            market_cap = parse_double(mkvaltq), # lot of NA
            exchange_code = exchg,
            #sic_code_historical = sich,
            sic_code = sic,
            gics_sector = gsector,
            gics_group = ggroup,
            gics_industry = gind,
            gics_subindustry = gsubind)
  

compustat_quarter_final <- compustat_quarter %>% 
  mutate(market_cap = if_else(price_close_calendar * shares_outstanding > 0,
                              price_close_calendar * shares_outstanding, NA_real_),
         shareholders_equity = if_else(
           !is.na(stockholders_equity1) == T, stockholders_equity1,
           coalesce(common_equity, 0) + coalesce(preferred_stock, 0)),
         book_equity_temp = if_else(stockholders_equity1 > 0,
                               shareholders_equity + coalesce(deferred_taxes_inv_credit, 0)
                               - coalesce(preferred_stock, 0), NA_real_),
         book_equity = if_else(book_equity_temp < 0, NA_real_, book_equity_temp),
         book_to_market = book_equity / market_cap,
         debt = coalesce(debt_in_current_liabilities, 0) + coalesce(debt_long_term, 0),
         leverage = if_else(assets <= 0, NA_real_, debt_liabilities / assets),
         roa = if_else(assets <= 0, NA_real_, net_income / assets),
         sales = if_else(sales <= 0, NA_real_, sales),
         tobin_q = if_else(assets <= 0, NA_real_, (assets + market_cap - book_equity) / assets))


write_rds(compustat_quarter_final, "data/compustat_quarter_final.rds")
