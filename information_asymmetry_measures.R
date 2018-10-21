library(tidyverse)
library(readr)
library(lubridate)

# PINs by Duarte and Young (2006 or 2009), PIN_DY, PIN_Adjusted and PSOS

pin_dy_raw <- read_csv("pin_dy_duarte_young.csv", col_types = cols(.default = "c"))
pin_dy <- pin_dy_raw %>% 
  transmute(permno = permno,
            year = as.numeric(year),
            exchange = as.numeric(exchange), # NYSE or AMEX probably, 1=NYSE?, 2=AMEX?
            pin = as.numeric(pin),
            adj_pin = as.numeric(adjpin),
            psos = as.numeric(psos))

pin_dy %>% 
  group_by(permno) %>% 
  tally()

# PINs by Easley, Hvidkjaer and O'Hara (2010), PIN_EHO

pin_eho_raw <- read_delim("pin_eho_easley_hvidkjaer_o'hara.dat", delim = " ", col_types = cols(.default = "c"))
pin_eho <- pin_eho_raw %>% 
  transmute(permno = permn,
            year = as.numeric(year),
            pin = as.numeric(pin))

pin_eho %>%
  group_by(permno) %>% 
  tally()

# PINs by Brown, Hillegeist and Lo (2004), PIN_BHL

pin_bhl_raw <- read_delim("pin_bhl_brown_hillegeist_lo.asc_", delim = " ")


# PINs by Brown and Hillegeist (2007), PIN_BH

pin_bh_raw <- read_delim("pin_bh_brown_hillegeist.asc_", delim = " ", col_types = cols(.default = "c"))

# MIAs by Johnson and So (2017)

mia_raw <- read_csv("mia_johnson_so.csv", col_types = cols(.default = "c"))

mia <- mia_raw %>% 
  transmute(permno = PERMNO,
            date = mdy(DATE),
            mia = as.numeric(MIA))
