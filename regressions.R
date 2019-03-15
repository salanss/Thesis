library(tidyverse)
library(lubridate)
library(lfe)
library(stargazer)
library(ggplot2)
library(MatchIt)
library(starpolishr)

# baseline regressions

baseline_regression_raw <- read_rds("data/baseline_regression_raw.rds") %>% 
  select(permno:sic_code, bid_ask_spread, everything()) %>% 
  select(-debt) %>% 
  rename(FOR_OWN = foreign_own, FOR_TO_INST_OWN = 
           foreign_own2, FOR_BREADTH = foreign_breadth,  FOR_TO_INST_BREADTH = foreign_breadth2,
         INST_OWN = institutional_own, INST_BREADTH = institutional_breadth, BA_SPREAD = bid_ask_spread,
         PIN_DY = pin_dy, PIN_DY_ADJ = pin_dy_adj, PSOS_DY = psos_dy, PIN_EHO = pin_eho,
         PIN_BHL = pin_bhl, PIN_BH = pin_bh, MIA = mia, VCV_USD = vcv1, VCV_MKT = vcv2, VCV_TO = vcv3,
         LOG_MKT_CAP = log_market_cap, BM_RATIO = book_to_market, LEVERAGE = leverage, ROA = roa) %>% 
  select(-spread)

write_rds(baseline_regression_raw, "results/baseline_regression_raw.rds")

cor_raw <- baseline_regression_raw %>% 
  select(year, FOR_OWN:FOR_TO_INST_OWN, FOR_BREADTH:FOR_TO_INST_BREADTH, INST_OWN, INST_BREADTH,
         LOG_MKT_CAP:ROA, BA_SPREAD, PIN_DY:VCV_TO) %>%
  mutate(year = as.integer(year))

years <- cor_raw %>% transmute(year = as.integer(year)) %>% distinct() %>% flatten_chr()

filter1 <- function (df, years){
  df %>% 
    filter(year == years) %>% 
    select(-year)
}

cor_dfs_raw <- map(years, ~filter1(cor_raw, .x))

map_na_function <- function(df){
  df %>% 
  map(~.x) %>% 
    discard(~all(is.na(.x))) %>%
    map_df(~.x)
}

cor_dfs <- map(cor_dfs_raw, ~map_na_function(.x))

cor_matrices <- map(cor_dfs_raw, ~cor(.x, use = "pairwise.complete.obs"))

arr <- array(unlist(cor_matrices), c(dim(cor_matrices[[1]]), length(cor_matrices)))

cor_matrices_mean <- round(rowMeans(x = arr, dims = 2, na.rm = T), 2)

cols <- cor_raw %>% select(-year) %>% colnames()

rownames(cor_matrices_mean) <- cols

colnames(cor_matrices_mean) <- cols

cor_matrices_mean[upper.tri(cor_matrices_mean)] <- ""

cor_matrix <- cor_matrices_mean

write_rds(cor_matrix, "results/cor_matrix.rds")

# resizebox.stargazer(
#   stargazer(cor_matrix, header=FALSE, title = "Correlation matrix",
#             float.env = "sidewaystable", align = T, no.space = T, font.size = "tiny",
#             column.sep.width = "1pt", type = "html", out = "correlation_matrix.html"),
#   tab.width = "\\linewidth")

# gather ia_measures for single column, create nested data frames and apply functional programming for lm model ->

baseline_regression <- baseline_regression_raw %>% 
  gather(ia_measure_name, ia_measure, BA_SPREAD:VCV_TO) %>% 
  gather(dep_measure_name, dep_measure, FOR_OWN) %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  group_by(dep_measure_name, ia_measure_name) %>% 
  nest()

baseline_regression2 <- baseline_regression_raw %>% 
  gather(ia_measure_name, ia_measure, BA_SPREAD:VCV_TO) %>% 
  gather(dep_measure_name, dep_measure, FOR_TO_INST_OWN) %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  group_by(dep_measure_name, ia_measure_name) %>% 
  nest()

baseline_regression3 <- read_rds("data/baseline_regression_raw.rds") %>% 
  gather(ia_measure_name, ia_measure, pin_dy:bid_ask_spread) %>% 
  gather(dep_measure_name, dep_measure, foreign_own:foreign_own_transient) %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  group_by(dep_measure_name, ia_measure_name) %>% 
  nest()

rnm <- function(df, ia_meas, dep_meas) {
  rnm_list <- set_names(c("ia_measure", "dep_measure"), syms(c(ia_meas, dep_meas)))
  rename(df, !!!rnm_list)
}

model_function <- function(df, ia_meas, dep_meas) {
  f <- dep_measure ~ ia_measure + BM_RATIO + LEVERAGE + 
    ROA |year + sic_code|0|year + sic_code
  f[[2]] <- sym(dep_meas)
  f[[3]][[2]][[2]][[2]][[2]][[2]][[2]] <- sym(ia_meas)
  felm(f, data = df)
}

model_function2 <- function(df, ia_meas, dep_meas) {
  f <- dep_measure ~ ia_measure + book_to_market + leverage + 
    roa |year + sic_code|0|year + sic_code
  f[[2]] <- sym(dep_meas)
  f[[3]][[2]][[2]][[2]][[2]][[2]][[2]] <- sym(ia_meas)
  felm(f, data = df)
}

baseline_names <- mutate(baseline_regression,
                         data_named = pmap(list(data, ia_measure_name, dep_measure_name), rnm),
                         model = pmap(list(data_named, ia_measure_name, dep_measure_name), model_function))

baseline_names2 <- mutate(baseline_regression2,
                          data_named = pmap(list(data, ia_measure_name, dep_measure_name), rnm),
                          model = pmap(list(data_named, ia_measure_name, dep_measure_name), model_function))

baseline_names3 <- mutate(baseline_regression3,
                          data_named = pmap(list(data, ia_measure_name, dep_measure_name), rnm),
                          model = pmap(list(data_named, ia_measure_name, dep_measure_name), model_function2))

# write_rds(baseline_names, "results/baseline_names.rds")

# stargazer(baseline_names$model, column.labels =c("Foreign ownership", "Foreign ownership2",
#                                                  "Foreign breadth", "Foreign breadth2"),
#           dep.var.labels.include = F,
#           column.separate = c(11, 11, 11, 11),
#           omit.stat = c("ser"),
#           add.lines = list(c("Industry fixed effects", rep("Yes", times = 44)), 
#                            c("Year fixed effects", rep("Yes", times = 44))),
#           title = "Baseline regression results (H1)", out = "Baseline_results.html")

stargazer(baseline_names$model, 
          header = F,
          label = "table_h1_for_breadth",
          notes.label = "",
          notes.append = F,
          notes = "",
          column.labels =c("FOR\\_OWN"),
          dep.var.labels.include = F,
          omit.stat = c("ser", "rsq"),
          #float.env = "sidewaystable",
          font.size = "tiny",
          no.space = T,
          add.lines = list(c("Industry fixed effects", rep("Yes", times = 11)),
                           c("Year fixed effects", rep("Yes", times = 11))),
          title = "Panel regression analysis of foreign breadth", type = "latex")

star1 <- stargazer(baseline_names$model,
                             header = F,
                             column.labels =c("FOR\\_OWN"),
                             label = "table_h1",
                             dep.var.labels.include = F,
                             notes.label = "",
                             notes.append = F,
                             notes = "",
                             column.separate = c(11),
                             omit.stat = c("ser", "rsq"),
                             font.size = "tiny",
                             no.space = T,
                             add.lines = list(c("Industry fixed effects", rep("Yes", times = 11)),
                                              c("Year fixed effects", rep("Yes", times = 11))),
                             title = "Panel regression analysis of foreign ownership
                                      and foreign breadth", 
                             type = "latex")

star2 <- stargazer(baseline_names2$model,
                             header = F,
                             #label = "table_h1_for_breadth",
                             notes.label = "",
                             notes.append = F,
                             notes = "",
                             column.labels =c("FOR OWN2"),
                             dep.var.labels.include = F,
                             omit.stat = c("ser", "rsq"),
                             font.size = "tiny",
                             no.space = T,
                             add.lines = list(c("Industry fixed effects", rep("Yes", times = 11)),
                                              c("Year fixed effects", rep("Yes", times = 11))),
                             type = "latex",
                   out = "testi.html")
                             #title = "Panel regression analysis of foreign breadth", type = "latex")

star3 <- stargazer(baseline_names3$model,
                   header = F,
                   out = "testiuusi.html")

star_out <- star_panel(star1, star2, same.summary.stats = F,
           panel.names = c("Foreign ownership", "Foreign breadth"))

star_tex_write(star_out, file = "my_tex_file.tex", headers = TRUE)


baseline_regression_summary <- baseline_regression_raw %>% 
  select(-permno, -year, -sic_code) %>% 
  as.data.frame()

write_rds(baseline_regression_summary, "results/baseline_regression_summary.rds")

# stargazer(baseline_regression_summary, title = "Baseline summary statistics", out = "Baseline_summary.html")

baseline_development <- baseline_regression_raw %>% 
  group_by(year) %>% 
  summarise(FOR_OWN = mean(FOR_OWN),
            INST_OWN = mean(INST_OWN)) %>% 
  ungroup()

write_rds(baseline_development, "results/baseline_development.rds")

ggplot(baseline_development, aes(year)) +
  #geom_line(aes(y = INST_OWN, colour = "INST_OWN")) +
  geom_line(aes(y = FOR_OWN, colour = "FOR_OWN")) +
  theme_classic()

## difference-in-differences regressions (did)

did_regression_raw <- read_rds("data/did_regression_raw.rds") %>% 
  mutate(year = year(event_date)) %>% 
  # group_by(permno, event_date, treated) %>%
  # mutate(n = n_distinct(after)) %>%
  # ungroup() %>% 
  # filter(n > 1) %>%  # require for every permno to have before and after value for each event
  arrange(event_date, quarter_index, treated) %>% 
  rename(FOR_OWN = foreign_own, FOR_TO_INST_OWN = 
           foreign_own2, FOR_BREADTH = foreign_breadth,  FOR_TO_INST_BREADTH = foreign_breadth2,
         INST_OWN = institutional_own, INST_BREADTH = institutional_breadth,
         LOG_MKT_CAP = log_market_cap, BM_RATIO = book_to_market, LEVERAGE = leverage, ROA = roa,
         ANALYST_COVERAGE = analyst_coverage)

# stargazer(cor_matrix, title = "correlation matrix", out = "correlation_matrix_did.html")

columns_for_summarise <- c("FOR_OWN", "FOR_TO_INST_OWN", "FOR_BREADTH", 
                           "FOR_TO_INST_BREADTH","INST_OWN", "INST_BREADTH", 
                           "LOG_MKT_CAP", "BM_RATIO", "LEVERAGE", 
                           "ROA", "ANALYST_COVERAGE")

did_regression <- did_regression_raw %>% 
  filter(quarter_index %in% c(-12:12)) %>% 
  group_by(permno, event_date, year, treated, after) %>% 
  mutate(sic_code = last(sic_code)) %>%
  ungroup() %>% 
  group_by(permno, event_date, year, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

did_regression_matched_temp1 <- did_regression %>% 
  filter(after == 0) %>% # match only based on before values during [-3;0] years
  group_by(permno, event_date, treated) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

events <- did_regression_raw %>% select(event_date) %>% distinct() 

filter1 <- function(df, events) {
  df %>% 
    filter(event_date == events)
}

propensity_match <- function(df) {
  did_match <- matchit(treated ~ LOG_MKT_CAP + BM_RATIO + ANALYST_COVERAGE,
          method = "nearest", distance = "logit", data = df, ratio = 3)
  df <- match.data(did_match)
  df
}

# produces propensity scores for permno-event_date-treated combinations (based on means)

did_regression_matched_temp2 <- map(events$event_date, ~filter1(did_regression_matched_temp1, .x)) %>%  
  map_df(~propensity_match(.x))

# now to join with before and after values

did_regression_matched_raw <- did_regression_matched_temp2 %>% 
  as_tibble() %>% 
  select(permno, event_date, treated) %>% 
  left_join(did_regression_raw) %>% 
  rename(AFTER = after, TREATED = treated) %>% 
  group_by(permno, event_date, TREATED) %>% 
  mutate(n = n_distinct(quarter_index[quarter_index <= 12 & quarter_index >= -12])) %>% 
  ungroup() %>% 
  filter(n == 24) 

write_rds(did_regression_matched_raw, "results/did_regression_matched_raw.rds")

did_regression_matched1 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-4:4)) %>% 
  group_by(permno, event_date, TREATED, AFTER) %>% 
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

write_rds(did_regression_matched1, "results/did_regression_matched1.rds")

did_regression_matched2 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-8:8)) %>% 
  # group_by(permno, event_date, TREATED) %>% 
  # mutate(n = n_distinct(quarter_index[quarter_index <= 4 & quarter_index >= -4])) %>% 
  # ungroup() %>% 
  # filter(n == 8) %>%
  group_by(permno, event_date, TREATED, AFTER) %>% 
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

write_rds(did_regression_matched2, "results/did_regression_matched2.rds")

did_regression_matched3 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-12:12)) %>% 
  # group_by(permno, event_date, TREATED) %>% 
  # mutate(n = n_distinct(quarter_index[quarter_index <= 4 & quarter_index >= -4])) %>% 
  # ungroup() %>% 
  # filter(n == 8) %>%
  group_by(permno, event_date, TREATED, AFTER) %>% 
  mutate(sic_code = last(sic_code)) %>% 
  ungroup() %>% 
  group_by(permno, event_date, year, TREATED, AFTER, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

write_rds(did_regression_matched3, "results/did_regression_matched3.rds")

did_regression_summary <- did_regression_matched1 %>% 
  select(-permno, -year, -sic_code) %>% 
  as.data.frame()

write_rds(did_regression_summary, "results/did_regression_summary.rds")

# stargazer(did_regression_summary, title = "DiD summary statistics", out = "DiD_summary.html")
  
did_model1 <- felm(ANALYST_COVERAGE ~ AFTER + TREATED + TREATED * AFTER|year + sic_code|0|year + sic_code,
                   data = did_regression_matched1)

did_model2 <- felm(FOR_OWN ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched2)

did_model3 <- felm(FOR_OWN ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched3)

did_model4 <- felm(FOR_TO_INST_OWN ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched1)

did_model5 <- felm(FOR_TO_INST_OWN ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched2)

did_model6 <- felm(FOR_TO_INST_OWN ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched3)

did_model7 <- felm(FOR_BREADTH ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched1)

did_model8 <- felm(FOR_BREADTH ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched2)

did_model9 <- felm(FOR_BREADTH ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched3)

did_model10 <- felm(FOR_TO_INST_BREADTH ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched1)

did_model11 <- felm(FOR_TO_INST_BREADTH ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched2)

did_model12 <- felm(FOR_TO_INST_BREADTH ~ AFTER + TREATED + TREATED * AFTER + BM_RATIO + 
                     LEVERAGE + ROA|year + sic_code|0|year + sic_code, data = did_regression_matched3)

did_list <- list(did_model1, did_model2, did_model3, did_model4, did_model5, did_model6, did_model7, did_model8,
                 did_model9, did_model10, did_model11, did_model12)

write_rds(did_list, "results/did_list.rds")

# stargazer(did_list, title = "Difference-in-differences regression results with propensity score matching (H2)",
#           dep.var.labels =c("Foreign ownership", "Foreign ownership2",
#                            "Foreign breadth", "Foreign breadth2"),
#           column.labels = rep(c("[-1;1] years", "[-2;2] years", "[-3;3] years"), times = 4),
#           omit.stat = c("ser"),
#           add.lines = list(c("Industry fixed effects", rep("Yes", times = 12)),
#                            c("Year fixed effects", rep("Yes", times = 12))),
#           out = "DiD H2 results.html")

parallel_trend1 <- did_regression_matched_raw %>% 
  mutate(TREATED = if_else(TREATED == 1, "treated", "control")) %>% 
  group_by(TREATED, quarter_index) %>% 
  summarise(ANALYST_COVERAGE = mean(ANALYST_COVERAGE)) %>% 
  ungroup()

`%nin%` = Negate(`%in%`)

parallel_trend1 <- did_regression_matched_raw %>% 
  mutate(foreign_own_nonquasi = FOR_OWN - foreign_own_quasi,
         foreign_breadth_nonquasi = FOR_BREADTH - foreign_breadth_quasi,
         domestic_own_nonquasi = domestic_own - domestic_own_quasi,
         domestic_breadth_nonquasi = domestic_breadth - domestic_breadth_quasi,
         inst_own_nonquasi = INST_OWN - institutional_own_quasi,
         inst_breadth_nonquasi = INST_BREADTH - institutional_breadth_quasi) %>% 
  mutate(treated = if_else(treated == 1, "treated", "control")) %>% 
  group_by(treated, quarter_index) %>% 
  summarise(FOR_OWN = mean(foreign_breadth_nonquasi),
            FOR_BREADTH = mean(foreign_breadth_nonquasi),
            FOR_TO_INST_OWN = mean(FOR_TO_INST_OWN),
            FOR_TO_INST_BREADTH = mean(FOR_TO_INST_BREADTH),
            foreign_own_nonquasi = mean(foreign_own_nonquasi),
            foreign_breadth_nonquasi = mean(foreign_breadth_nonquasi),
            INST_OWN = mean(INST_OWN),
            INST_BREADTH = mean(INST_BREADTH),
            inst_own_nonquasi = mean(inst_own_nonquasi),
            inst_breadth_nonquasi = mean(inst_breadth_nonquasi),
            domestic_own = mean(domestic_own),
            domestic_breadth = mean(domestic_breadth),
            domestic_own_nonquasi = mean(domestic_own_nonquasi),
            domestic_breadth_nonquasi = mean(domestic_breadth_nonquasi)) %>% 
  ungroup()

parallel_trend2 <- parallel_trend1 %>% 
  group_by(treated, quarter_index) %>% 
  summarise_all(.funs = funs(mean)) %>% 
  ungroup()

ggplot(parallel_trend1, aes(quarter_index, ANALYST_COVERAGE, group = TREATED, color = TREATED)) +
  geom_line() +
  labs(x = "Event quarter") +
  geom_vline(xintercept=0) +
  theme_classic() +
  theme(plot.title = element_text(size=9, face="italic")) +
  scale_color_grey(start = 0.55, end = 0)

write_rds(parallel_trend1, "results/parallel_trend1.rds")

# ggplot(parallel_trend1, aes(quarter_index, measure, group = treated, color = treated)) + 
#   scale_colour_grey() + 
#   geom_line() +
#   geom_vline(xintercept=0) +
#   theme_classic() +
#   facet_wrap(. ~ measure_name, scales = "free_y")
# 
# ggsave("parallel_trend.png", last_plot())

parallel_trend2 <- did_regression_matched_raw %>% 
  mutate(treated = if_else(treated == 1, "treated", "control")) %>% 
  group_by(treated, quarter_index, event_date) %>% 
  summarise(foreign_inst_percentage = mean(foreign_inst_percentage),
            foreign_inst_percentage2 = mean(foreign_inst_percentage2),
            foreign_breadth = mean(foreign_breadth),
            foreign_breadth2 = mean(foreign_breadth2),
            domestic_inst_percentage = mean(domestic_inst_percentage),
            domestic_breadth = mean(domestic_breadth)) %>% 
  ungroup()

write_rds(parallel_trend2, "results/parallel_trend2.rds")

# ggplot(parallel_trend2, aes(quarter_index, foreign_inst_percentage, group = treated, color = treated)) + 
#   geom_line() +
#   geom_vline(xintercept=0) +
#   theme_classic() + facet_wrap(. ~ event_date)
# 
# ggsave("parallel_trend_facet.png", last_plot())


## H2 DiD without propensity score matching (all the rest)

did_regression_summary_no_prop <- did_regression %>% 
  select(-permno, -year, -sic_code) %>% 
  as.data.frame()

write_rds(did_regression_summary_no_prop, "results/did_regression_summary_no_prop.rds")

# stargazer(did_regression_summary, title = "DiD summary statistics", out = "DiD_summary_no_prop.html")

did_regression1 <- did_regression_raw %>% 
  filter(quarter_index %in% c(-4:4)) %>% 
  group_by(permno, event_date, year, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, treated, after) %>% 
  mutate(sic_code = last(sic_code)) %>% 
  ungroup()

did_regression2 <- did_regression_raw %>% 
  filter(quarter_index %in% c(-8:8)) %>% 
  group_by(permno, event_date, year, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup() %>% 
  group_by(permno, event_date, treated, after) %>% 
  mutate(sic_code = last(sic_code)) %>% 
  ungroup()

did_regression3 <- did_regression

did_model1 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression1)

did_model2 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression2)

did_model3 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression3)

did_model4 <- felm(foreign_inst_percentage2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression1)

did_model5 <- felm(foreign_inst_percentage2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression2)

did_model6 <- felm(foreign_inst_percentage2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression3)

did_model7 <- felm(foreign_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression1)

did_model8 <- felm(foreign_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression2)

did_model9 <- felm(foreign_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression3)

did_model10 <- felm(foreign_breadth2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                      leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression1)

did_model11 <- felm(foreign_breadth2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                      leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression2)

did_model12 <- felm(foreign_breadth2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                      leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression3)

did_list_no_prop <- list(did_model1, did_model2, did_model3, did_model4, did_model5, did_model6, did_model7, did_model8,
                 did_model9, did_model10, did_model11, did_model12)

write_rds(did_list_no_prop, "results/did_list_no_prop.rds")

# stargazer(did_list, title = "Difference-in-differences regression results without propensity score matching (H2)", 
#           dep.var.labels =c("Foreign ownership", "Foreign ownership2",
#                             "Foreign breadth", "Foreign breadth2"),
#           column.labels = rep(c("[-1;1] years", "[-2;2] years", "[-3;3] years"), times = 4),
#           omit.stat = c("ser"), 
#           add.lines = list(c("Industry fixed effects", rep("Yes", times = 12)), 
#                            c("Year fixed effects", rep("Yes", times = 12))),
#           out = "DiD H2 results_no_prop.html")


parallel_trend1 <- did_regression_raw %>% 
  mutate(treated = if_else(treated == 1, "treated", "control")) %>% 
  group_by(treated, quarter_index) %>% 
  summarise(`Foreign ownership` = mean(foreign_inst_percentage),
            `Foreign ownership2` = mean(foreign_inst_percentage2),
            `Foreign breadth` = mean(foreign_breadth),
            `Foreign breadth2` = mean(foreign_breadth2)) %>% 
  ungroup() %>% 
  gather(measure_name, measure, `Foreign ownership`:`Foreign breadth2`)

# ggplot(parallel_trend1, aes(quarter_index, measure, group = treated, color = treated)) + 
#   geom_line() +
#   geom_vline(xintercept=0) +
#   theme_classic() +
#   facet_wrap(. ~ measure_name, scales = "free_y")
# 
# ggsave("parallel_trend_no_prop.png", last_plot())

parallel_trend2 <- did_regression_raw %>% 
  mutate(treated = if_else(treated == 1, "treated", "control")) %>% 
  group_by(treated, quarter_index, event_date) %>% 
  summarise(foreign_inst_percentage = mean(foreign_inst_percentage),
            foreign_shares_by_institutional_shares = mean(foreign_shares_by_institutional_shares),
            foreign_breadth2 = mean(foreign_breadth2),
            foreign_breadth = mean(foreign_breadth),
            domestic_inst_percentage = mean(domestic_inst_percentage),
            domestic_breadth = mean(domestic_breadth)) %>% 
  ungroup()

# ggplot(parallel_trend2, aes(quarter_index, foreign_breadth2, group = treated, color = treated)) + 
#   geom_line() +
#   geom_vline(xintercept=0) +
#   theme_classic() + facet_wrap(. ~ event_date)
# 
# ggsave("parallel_trend_facet_no_prop.png", last_plot())


## DiD H3 results

did_regression_h3 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-12:12)) %>% 
  filter(analyst_coverage < 3) %>% 
  group_by(permno, event_date, year, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

did_model1 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3)

did_model2 <- felm(foreign_inst_percentage2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3)

did_model3 <- felm(foreign_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3)

did_model4 <- felm(foreign_breadth2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3)

did_list <- list(did_model1, did_model2, did_model3, did_model4)

# stargazer(did_list, title = "Difference-in-differences regression results (H3) Analyst coverage < 3", 
#           omit.stat = c("ser"), 
#           add.lines = list(c("Industry fixed effects", rep("Yes", times = 4)), 
#                            c("Year fixed effects", rep("Yes", times = 4))),
#           out = "DiD H3 results_low_analyst_coverage.html")

did_regression_h3_2 <- did_regression_matched_raw %>% 
  filter(quarter_index %in% c(-12:12)) %>% 
  filter(inst_percentage < 0.30) %>% 
  group_by(permno, event_date, year, treated, after, sic_code) %>% 
  summarise_at(columns_for_summarise, mean) %>% 
  ungroup()

did_model1 <- felm(foreign_inst_percentage ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3_2)

did_model2 <- felm(foreign_inst_percentage2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3_2)

did_model3 <- felm(foreign_breadth ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3_2)

did_model4 <- felm(foreign_breadth2 ~ treated + after + treated * after + log_market_cap + book_to_market + 
                     leverage + roa + tobin_q |year + sic_code|0|year + sic_code, data = did_regression_h3_2)

did_list <- list(did_model1, did_model2, did_model3, did_model4)

# stargazer(did_list, title = "Difference-in-differences regression results (H3) Inst. ownership < 0.30", 
#           omit.stat = c("ser"), 
#           add.lines = list(c("Industry fixed effects", rep("Yes", times = 4)), 
#                            c("Year fixed effects", rep("Yes", times = 4))),
#           out = "DiD H3 results_low_institutional_ownership.html")
