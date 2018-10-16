# old but maybe useful codes

detail_temp2 <- detail_temp1 %>%
  arrange(brokerage, ibes_ticker, announce_date)

aa0g <- detail_temp1 %>% 
  #filter(ibes_ticker == "AA0G") %>% 
  filter(announce_date > as.Date("2004-05-05"))

aa0g %>% filter(brokerage == "00003")