library(tidyverse)
library(readr)
library(lubridate)

# PINs by Duarte and Young (2006 or 2009)

pin_dy_raw <- read_csv("pin_dy_duarte_young.csv", col_types = cols(.default = "c"))
pin_dy <- pin_dy_raw %>% 
  transmute(permno = permno,
            year = as.numeric(year),
            pin = as.numeric(pin),
            exchange = as.numeric(exchange), # NYSE or AMEX probably, 1=NYSE?, 2=AMEX?
            adj_pin = as.numeric(adjpin),
            psos = as.numeric(psos))

pin_dy %>% 
  group_by(permno) %>% 
  tally()


pin_eho_raw <- read_delim("pin_eho_easley_hvidkjaer_o'hara.dat", delim = " ", col_types = cols(.default = "c"))
pin_eho <- pin_eho_raw %>% 
  transmute(permno = permn,
            year = as.numeric(year),
            pin = as.numeric(pin))

pin_eho %>%
  group_by(permno) %>% 
  tally()


