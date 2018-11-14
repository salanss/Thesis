library(tidyverse)
library(readr)
library(lubridate)


testi1 <- read_tsv("data/testi1.txt", col_types = cols(.default = "c"))
testi2 <- read_tsv("data/testi2.txt", col_types = cols(.default = "c"))
