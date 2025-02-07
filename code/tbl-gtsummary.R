library(gtsummary)

df_corr_tbl <- df_seasonal_joins_3 |>
  select(Station.name, Season, Seasonal_avg.pm2.5.x, Seasonal_avg.pm2.5.y, Seasonal_avg.r.y, Seasonal_avg.r) |>
  rename("winter pm2.5" = Seasonal_avg.pm2.5.x,
         "spring pm2.5" = Seasonal_avg.pm2.5.y,
         "spring r" = Seasonal_avg.r.y,
         "summer r" = Seasonal_avg.r) |>
 # filter(Station.name == "UB" | Station.name == "DZ") |>
  drop_na()

# summarize the data with our package
table1_UB <-
  df_combined %>%
  select(Station.name, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) %>%
 filter(Station.name == "UB" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Average_Type,
   ~tbl_summary(.x,
               # type = Station.name ~ "continuous",
                # label = c(Station.name ~ "UB", 
                #           Seasonal_avg.r ~ "r", 
                #           Seasonal_avg.pm2.5 ~ "PM2.5 (mug/m3)", Seasonal_avg.pm10 ~ "PM10 (mug/m3)", 
                #           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
          by = Season, missing = "no",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
         # digits = 2
              ) 
           ) |>
  remove_row_type(Station.name, type = "level") 
table1_UB 

table2_UB <-
  df_combined %>%
  filter(Station.name == "UB" & Average_Type == "Annual") %>%
  select(Station.name, Average_Type, Annual_avg.r, Annual_avg.pm2.5, Annual_avg.pm10, Annual_avg.WS, Annual_avg.WD,  Annual_avg.VIS) %>%
  rename(Seasonal_avg.r = Annual_avg.r,  Seasonal_avg.pm2.5 = Annual_avg.pm2.5, Seasonal_avg.pm10 = Annual_avg.pm10, 
         Seasonal_avg.WS = Annual_avg.WS, Seasonal_avg.WD = Annual_avg.WD, Seasonal_avg.VIS = Annual_avg.VIS) %>%
  tbl_strata(
    strata = Average_Type,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                 # label = c(Station.name ~ "UB", 
                 #           Annual_avg.r ~ "r", 
                 #            Annual_avg.pm2.5 ~ "PM2.5 (mug/m3)", Annual_avg.pm10 ~ "PM10 (mug/m3)", 
                 #            Annual_avg.WS ~ "WS (m/s)", Annual_avg.WD ~ "WD (deg)",  Annual_avg.VIS ~ "VIS (km)"),
               #  by = Season, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    ) 
  ) |>
  remove_row_type(Station.name, type = "level") 
table2_UB

tbl_merge(tbls = list(table1, table2) ) %>%
  modify_spanning_header(everything() ~ NA_character_)


table3a <-
  df_combined %>%
  filter(Station.name == "UB" & Average_Type == "Annual") %>%
  select(Station.name, Average_Type, starts_with("I_5")) %>%
  mutate("Changes" = "I") %>%
  rename(r = I_5y_mean.r,  pm2.5 = I_5y_mean.pm2.5, pm10 = I_5y_mean.pm10 , WS = I_5y_mean.WS, VIS = I_5y_mean.VIS)
table3b <-
  df_combined %>%
  filter(Station.name == "UB" & Average_Type == "Annual") %>%
  select(Station.name, Average_Type, starts_with("II_5")) %>%
 # mutate("Changes" = "II") %>%
  rename(r = II_5y_mean.r,  pm2.5 = II_5y_mean.pm2.5, pm10 = II_5y_mean.pm10 , WS = II_5y_mean.WS, VIS = II_5y_mean.VIS)
table3 <- bind_rows(table3a, table3b)
table3$Changes[is.na(table3$Changes)] <- "II"


table3_UB <-
  df_combined |>
  select(Station.name, Year, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) |>
  mutate(Year = case_when(Year <= 2013 ~ "I", Year > 2013 ~ "II")) |>
  filter(Station.name == "UB" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Season,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                 # label = c(Station.name ~ "UB", 
                 #           Seasonal_avg.r ~ "r", 
                 #           Seasonal_avg.pm2.5 ~ "PM2.5 (mug/m3)", Seasonal_avg.pm10 ~ "PM10 (mug/m3)", 
                 #           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
                 by = Year, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2

  )
  remove_row_type(Station.name, type = "level") |>
  remove_row_type(Average_Type, type = "all")) 
table3_UB 

modify_sp
tbl_UB <- tbl_merge(tbls = list(table1_UB, table2_UB, table3_UB) ,
                 tab_spanner = c("**Seasonal**", "**Annual**", "**pre-post [2014]**")) %>%
  modify_header(stat_2_1_1= paste("**Seasonal** Seasonal"), stat_0_1_2 = "Annual", stat_1_1_3 = "pre-[2014]")
tbl_UB
show_header_names(tbl)


# Example 1 ----------------------------------
df_combined |>
  select(Station.name, Season, Average_Type, Seasonal_avg.WD, Seasonal_avg.WS, Seasonal_avg.VIS, Seasonal_avg.pm10, Seasonal_avg.pm2.5, Seasonal_avg.r) %>%
  filter(Station.name == "UB") %>%
  tbl_strata(strata = Average_Type,
  ~tbl_summary(.x, by = Season, missing = "no") )|>
  remove_row_type(Station.name, type = "all") |>
  add_row(Station.name, before = Seasonal_avg.WD
)
remove_row

add_ro
table2 <-
  df_combined %>%
  filter(Station.name == "DZ") %>%
  select(Season, Seasonal_avg.WD, Seasonal_avg.WS, Seasonal_avg.VIS, Seasonal_avg.pm10, Seasonal_avg.pm2.5, Seasonal_avg.r) %>%
  tbl_summary(
    by = Station.name, missing = "no",
    statistic = list(all_continuous() ~ "{mean} ({sd})")) 
table2

tbl_stack(list(table1, table2))

tbl <- 
  mtcars %>%
  select(cyl) %>%
  mutate(fct_cyl = factor(cyl)) %>%
  tbl_summary(
    type = where(is.numeric) ~ "continuous",
    statistic = where(is.numeric) ~ "{mean} ({sd})",
    label = cyl ~ "No. Cylinders"
  ) 

# remove extra header row for factor variables
tbl$table_body <-
  tbl$table_body %>%
  filter(!(startsWith(variable, "fct_") & row_type == "label"))

# print table
tbl

----
  # \donttest{
  # Example 1 ----------------------------------
my_stats <- function(data, ...) {
  marker_sum = sum(data$marker, na.rm = TRUE)
  mean_age = mean(data$age, na.rm = TRUE)
  dplyr::tibble(
    marker_sum = marker_sum,
    mean_age = mean_age
  )
}

my_stats(trial)

tbl_custom_summary_ex1 <-
  trial %>%
  tbl_custom_summary(
    include = c("stage", "grade"),
    by = "trt",
    stat_fns = everything() ~ my_stats,
    statistic = everything() ~ "A: {mean_age} - S: {marker_sum}",
    digits = everything() ~ c(1, 0),
    overall_row = TRUE,
    overall_row_label = "All stages & grades"
  ) %>%
  add_overall(last = TRUE) %>%
  modify_footnote(
    update = all_stat_cols() ~ "A: mean age - S: sum of marker"
  ) %>%
  bold_labels()

# Example 2 ----------------------------------
# Use `data[[variable]]` to access the current variable
mean_ci <- function(data, variable, ...) {
  test <- t.test(data[[variable]])
  dplyr::tibble(
    mean = test$estimate,
    conf.low = test$conf.int[1],
    conf.high = test$conf.int[2]
  )
}

tbl_custom_summary_ex2 <-
  df_combined %>%
  tbl_custom_summary(
    include = c("Season", "Seasonal_avg.pm2.5"),
    by = c("Station.name","Season")
    stat_fns = ~ mean_ci,
    statistic = ~ "{mean} [{conf.low}; {conf.high}]"
  ) %>%
  add_overall(last = TRUE) %>%
#  modify_footnote(
    update = all_stat_cols() ~ "mean [95% CI]"
  )



# Example 3 ----------------------------------
# Use `full_data` to access the full datasets
# Returned statistic can also be a character
diff_to_great_mean <- function(data, full_data, ...) {
  mean <- mean(data$marker, na.rm = TRUE)
  great_mean <- mean(full_data$marker, na.rm = TRUE)
  diff <- mean - great_mean
  dplyr::tibble(
    mean = mean,
    great_mean = great_mean,
    diff = diff,
    level = ifelse(diff > 0, "high", "low")
  )
}

tbl_custom_summary_ex3 <-
  trial %>%
  tbl_custom_summary(
    include = c("grade", "stage"),
    by = "trt",
    stat_fns = ~ diff_to_great_mean,
    statistic = ~ "{mean} ({level}, diff: {diff})",
    overall_row = TRUE
  ) %>%
  bold_labels()
# }


# Example 4 ----------------------------------
# Use tbl_strata
df_combined %>%
  tbl_strata(
    strata = Station.name
    ~tbl_summary(.x, by = Season, missing = "no") %>%
      modify_header(all_stat_cols() ~ "**{level}**")
  )
