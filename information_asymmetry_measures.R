library(tidyverse)
library(lubridate)

# PINs by Duarte and Young (2006 or 2009), PIN_DY, PIN_Adjusted and PSOS
# link http://www.owlnet.rice.edu/~jd10/publications.html

pin_dy_raw <- read_csv("data/pin_dy_duarte_young.csv", col_types = cols(.default = "c"))
pin_dy <- pin_dy_raw %>% 
  transmute(permno = permno,
            year = as.numeric(year),
            exchange = as.numeric(exchange), # NYSE or AMEX probably, 1=NYSE?, 2=AMEX?
            pin_dy = as.numeric(pin),
            pin_dy_adj = as.numeric(adjpin),
            psos_dy = as.numeric(psos)) %>% 
  select(-exchange)

e <- pin_dy %>% 
  group_by(year) %>% 
  tally()

# PINs by Easley, Hvidkjaer and O'Hara (2010), PIN_EHO
# link https://sites.google.com/site/hvidkjaer/data

pin_eho_raw <- read_delim("data/pin_eho_easley_hvidkjaer_o'hara.dat", delim = " ", col_types = cols(.default = "c"))
pin_eho <- pin_eho_raw %>% 
  transmute(permno = permn,
            year = as.numeric(year),
            pin_eho = as.numeric(pin))

f <- pin_eho %>%
  group_by(year) %>% 
  tally()

# PINs by Brown, Hillegeist and Lo (2004), PIN_BHL
# link http://scholar.rhsmith.umd.edu/sbrown/probability-informed-trade-easley-et-al-model

pin_bhl_raw <- read_table("data/pin_bhl_brown_hillegeist_lo.asc_", col_names = T, col_types = cols(.default = "c"))

pin_bhl <- pin_bhl_raw %>% 
  transmute(permno = permno,
            year = as.numeric(year),
            pin_bhl = as.numeric(pinesas))

g <- pin_bhl %>% group_by(year) %>% 
  tally()

# PINs by Brown and Hillegeist (2007), PIN_BH
# link http://scholar.rhsmith.umd.edu/sbrown/pin-data

pin_bh_raw <- read_table("data/pin_bh_brown_hillegeist.asc_", col_names = T, col_types = cols(.default = "c"))

pin_bh <- pin_bh_raw %>% 
  transmute(permno = permno,
            year = as.numeric(year),
            pin_bh = as.numeric(pin))

h <- pin_bh %>%
  group_by(year) %>% 
  tally()

# MIAs by Johnson and So (2017)
# link http://travislakejohnson.com/data.html

mia_raw <- read_csv("data/mia_johnson_so.csv", col_types = cols(.default = "c"))

mia <- mia_raw %>% 
  transmute(permno = PERMNO,
            date = mdy(DATE),
            mia_daily = as.numeric(MIA),
            year = year(mdy(DATE))) %>% 
  group_by(permno, year) %>% 
  summarise(mia = mean(mia_daily)) %>% 
  ungroup()

mia_quarter <- mia_raw %>%
  transmute(permno = PERMNO, 
            date = mdy(DATE),
            quarter_date = ceiling_date(date, "quarter") - days(1),
            mia_daily = as.numeric(MIA)) %>% 
  group_by(permno, quarter_date) %>% 
  summarise(mia = mean(mia_daily)) %>% 
  ungroup()

vcv_raw <- read_rds("data/vcv.rds")

vcv <- vcv_raw %>% 
  mutate(permno = as.character(permno)) %>% 
  mutate_at(vars(vcv1:vcv3),
            funs(Winsorize(., probs = c(0.01, 0.99), na.rm = T)))

ba_spread_raw <- read_rds("data/ba_spread.rds")

ba_spread <- ba_spread_raw %>% 
  mutate(permno = as.character(permno),
         bid_ask_spread = Winsorize(spread, probs = c(0.01, 0.99), na.rm = T))

information_asymmetry_measures <- pin_dy %>% 
  full_join(pin_eho, by = c("permno", "year")) %>% 
  full_join(pin_bhl, by = c("permno", "year")) %>% 
  full_join(pin_bh, by = c("permno", "year")) %>% 
  full_join(mia, by = c("permno", "year")) %>% 
  full_join(vcv, by = c("permno", "year")) %>% 
  full_join(ba_spread, by = c("permno", "year"))

write_rds(information_asymmetry_measures, "data/information_asymmetry_measures.rds")
