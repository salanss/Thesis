library(tidyverse)
library(lubridate)

investor_types_raw <- read_delim("data/bushee.txt", delim = " ", col_names = F, col_types = cols(.default = "c"))

investor_types <- investor_types_raw %>% 
  transmute(institutional_code = X1,
            year = parse_double(X4),
            investor_type = case_when(X6 == "  ." ~ NA_character_,
                                      TRUE ~ X6))

write_rds(investor_types, "data/investors_types.rds")
