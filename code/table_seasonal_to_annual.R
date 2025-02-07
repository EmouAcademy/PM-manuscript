# summarize the data with our package
table1_DZ <-
  df_combined %>%
  select(Station.name, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) %>%
  filter(Station.name == "DZ" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Average_Type,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                  label = c(Station.name ~ "DZ"), 
                 #           Seasonal_avg.r ~ "r", 
                 #           Seasonal_avg.pm2.5 ~ "PM2.5 (mug/m3)", Seasonal_avg.pm10 ~ "PM10 (mug/m3)", 
                 #           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
                 by = Season, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    ) 
  ) |>
  remove_row_type(Station.name, type = "level") 
table1_DZ 

table2_DZ <-
  df_combined %>%
  filter(Station.name == "DZ" & Average_Type == "Annual") %>%
  select(Station.name, Average_Type, Annual_avg.r, Annual_avg.pm2.5, Annual_avg.pm10, Annual_avg.WS, Annual_avg.WD,  Annual_avg.VIS) %>%
  rename(Seasonal_avg.r = Annual_avg.r,  Seasonal_avg.pm2.5 = Annual_avg.pm2.5, Seasonal_avg.pm10 = Annual_avg.pm10, 
         Seasonal_avg.WS = Annual_avg.WS, Seasonal_avg.WD = Annual_avg.WD, Seasonal_avg.VIS = Annual_avg.VIS) %>%
  tbl_strata(
    strata = Average_Type,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                  label = c(Station.name ~ "DZ"), 
                 #           Annual_avg.r ~ "r", 
                 #            Annual_avg.pm2.5 ~ "PM2.5 (mug/m3)", Annual_avg.pm10 ~ "PM10 (mug/m3)", 
                 #            Annual_avg.WS ~ "WS (m/s)", Annual_avg.WD ~ "WD (deg)",  Annual_avg.VIS ~ "VIS (km)"),
                 #  by = Season, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    ) 
  ) |>
  remove_row_type(Station.name, type = "level") 
table2_DZ

tbl_DZ <- tbl_merge(tbls = list(table1_DZ, table2_DZ)) |>
          modify_spanning_header(everything() ~ NA_character_)
tbl_DZ
show_header_names(tbl)

table3_DZ <-
  df_combined |>
  select(Station.name, Year, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) |>
  mutate(Year = case_when(Year <= 2013 ~ "I", Year > 2013 ~ "II")) |>
  filter(Station.name == "DZ" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Season,
    ~tbl_summary(.x,
                 type = c(Seasonal_avg.pm2.5, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS) ~ "continuous",
                  label = c(Station.name ~ "DZ"), 
                 #           Seasonal_avg.r ~ "r", 
                 #           Seasonal_avg.pm2.5 ~ "PM2.5 (mug/m3)", Seasonal_avg.pm10 ~ "PM10 (mug/m3)", 
                 #           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
                 by = Year, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    )
    )  |>
  remove_row_type(Station.name, type = "level") |>
  remove_row_type(Average_Type, type = "level") 
table3_DZ

### change to tbl_summary
table3_DZb <-
  df_combined |>
  select(Station.name, Year, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) |>
  mutate(Year = case_when(Year <= 2013 ~ "I", Year > 2013 ~ "II")) |>
  filter(Station.name == "DZ" & Average_Type == "Seasonal") %>%
    tbl_summary(
                 type = c(Seasonal_avg.pm2.5, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS) ~ "continuous",
                 label = c(Station.name ~ "DZ"), 
                 #           Seasonal_avg.r ~ "r", 
                 #           Seasonal_avg.pm2.5 ~ "PM2.5 (mug/m3)", Seasonal_avg.pm10 ~ "PM10 (mug/m3)", 
                 #           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
                 by = list(Year, Season), missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    
  )  |>
  remove_row_type(Station.name, type = "level") |>
  remove_row_type(Average_Type, type = "level") 
table3_DZb





######################
