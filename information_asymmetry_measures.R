library(tidyverse)
library(readr)
library(lubridate)

# PINs by Duarte and Young (2006 or 2009), PIN_DY, PIN_Adjusted and PSOS

pin_dy_raw <- read_csv("pin_dy_duarte_young.csv", col_types = cols(.default = "c"))
pin_dy <- pin_dy_raw %>% 
  transmute(permno = permno,
            year = as.numeric(year),
            exchange = as.numeric(exchange), # NYSE or AMEX probably, 1=NYSE?, 2=AMEX?
            pin_dy = as.numeric(pin),
            pin_dy_adj = as.numeric(adjpin),
            psos_dy = as.numeric(psos)) %>% 
  select(-exchange)

pin_dy %>% 
  group_by(permno) %>% 
  tally()

# PINs by Easley, Hvidkjaer and O'Hara (2010), PIN_EHO

pin_eho_raw <- read_delim("pin_eho_easley_hvidkjaer_o'hara.dat", delim = " ", col_types = cols(.default = "c"))
pin_eho <- pin_eho_raw %>% 
  transmute(permno = permn,
            year = as.numeric(year),
            pin_eho = as.numeric(pin))

pin_eho %>%
  group_by(permno) %>% 
  tally()

# PINs by Brown, Hillegeist and Lo (2004), PIN_BHL

pin_bhl_raw <- read_table("pin_bhl_brown_hillegeist_lo.asc_", col_names = T, col_types = cols(.default = "c"))

pin_bhl <- pin_bhl_raw %>% 
  transmute(permno = permno,
            year = as.numeric(year),
            pin_bhl = as.numeric(pinesas))

# PINs by Brown and Hillegeist (2007), PIN_BH

pin_bh_raw <- read_table("pin_bh_brown_hillegeist.asc_", col_names = T, col_types = cols(.default = "c"))

pin_bh <- pin_bh_raw %>% 
  transmute(permno = permno,
            year = as.numeric(year),
            pin_bh = as.numeric(pin))

# MIAs by Johnson and So (2017)

mia_raw <- read_csv("mia_johnson_so.csv", col_types = cols(.default = "c"))

mia <- mia_raw %>% 
  transmute(permno = PERMNO,
            date = mdy(DATE),
            mia = as.numeric(MIA),
            year = year(mdy(DATE)))


information_asymmetry_measures_temp1 <- full_join(pin_dy, pin_eho)

information_asymmetry_measures_temp2 <- full_join(information_asymmetry_measures_temp1, pin_bhl)

information_asymmetry_measures_temp3 <- full_join(information_asymmetry_measures_temp2, pin_bh)

information_asymmetry_measures_temp4 <- full_join(information_asymmetry_measures_temp3, mia)
