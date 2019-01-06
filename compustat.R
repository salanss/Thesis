library(tidyverse)
library(readr)
library(lubridate)

compustat_raw <- read_tsv("data/crsp_compustat_merged_annual.txt", col_types = cols(.default = "c"))
