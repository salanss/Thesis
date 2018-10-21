thirteenf <- read_rds("13f_output.rds")
crsp_daily_stock <- read_rds("crsp_daily_stock.rds")

thirteenf
crsp_daily_stock

thirteenf_temp1 <-
  left_join(thirteenf, crsp_daily_stock, by = c("cusip" = "ncusip", "report_date" = "date"))
