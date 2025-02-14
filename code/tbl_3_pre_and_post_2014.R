library(ggplot2)
library(readr)
library(tidyverse)      # scale_y_break
library(here)
#source(here("code/Colors.R"))
library(scales)
library(ggpubr)
library(ggbreak)
library(patchwork)
library(ggrepel)
####### input DATA 
data <- read.csv("data/processed/df.csv")
df <- read.csv("data/processed/df_filled.csv")
## add order and levels
data$Station.name <- factor(data$Station.name, 
                            levels = c("UB", "DZ",  "ZU", "SS"))  
levels(data$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')

df <- read.csv("data/processed/df_filled.csv")
df$Station.name <- factor(df$Station.name, 
                          levels = c("UB", "DZ",  "ZU", "SS"))  
levels(df$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')

data$Date <- as.Date(data$Date) 
df$Date <- as.Date(df$Date) 
df <- df |>
  filter(Year<2021)
########################### Plotting
x <- c("DJF", "MAM", "JJA","SON")
df <- df %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(12, 1, 2) ~ 'Winter',
      Month %in% c(3, 4, 5) ~ 'Spring',
      Month %in% c(6, 7, 8) ~ 'Summer',
      Month %in% c(9, 10, 11) ~ 'Autumn'
    )
  )
df$Season <- factor(df$Season, 
                    levels = c('Winter', 'Spring', 'Summer', 'Autumn')) 
###################################################################################
##################################################################################
############## Annual MEANS
##################################################################################
# Calculate seasonal averages for each year and season
df_seasonal <- df %>%
  group_by(Station.name, Year, Season) %>%
  summarize(
    Seasonal_avg.WD = mean(WD_deN.f, na.rm = TRUE),     # average WD, WS, VIS
    Seasonal_avg.WS = mean(WS_deN.f, na.rm = TRUE),
    Seasonal_avg.VIS = mean(Visibility_deNormalized, na.rm = TRUE),
    Seasonal_avg.pm10 = mean(pm10_deN.f, na.rm = TRUE),     # average PM, PM2.5, r
    Seasonal_avg.pm2.5 = mean(pm2.5_deN.f, na.rm = TRUE),
    Seasonal_avg.r = Seasonal_avg.pm2.5/Seasonal_avg.pm10,
    .groups = 'drop'
  )

# Calculate 5 year means for 4 SEASONS
df_seasonal_5year.mean <- df_seasonal %>%
  group_by(Station.name, Season) %>%
  summarize(
    I_5y_mean.WS = mean(Seasonal_avg.WS[Year<= 2013]),
    II_5y_mean.WS = mean(Seasonal_avg.WS[Year>=2014]),
    long_term_seasonal_mean.WS = mean(Seasonal_avg.WS),
    sd_value.WS = sd(Seasonal_avg.WS),
    sd_up.WS = long_term_seasonal_mean.WS + sd_value.WS,
    sd_down.WS = long_term_seasonal_mean.WS - sd_value.WS,
    I_5y_mean.VIS = mean(Seasonal_avg.VIS[Year<= 2013]),
    II_5y_mean.VIS = mean(Seasonal_avg.VIS[Year>=2014]),
    long_term_seasonal_mean.VIS = mean(Seasonal_avg.VIS),
    sd_value.VIS = sd(Seasonal_avg.VIS),
    sd_up.VIS = long_term_seasonal_mean.VIS + sd_value.VIS,
    sd_down.VIS = long_term_seasonal_mean.VIS - sd_value.VIS,
    I_5y_mean.pm10 = mean(Seasonal_avg.pm10[Year<= 2013]),
    II_5y_mean.pm10 = mean(Seasonal_avg.pm10[Year>=2014]),
    long_term_seasonal_mean.pm10 = mean(Seasonal_avg.pm10),
    sd_value.pm10 = sd(Seasonal_avg.pm10),
    sd_up.pm10 = long_term_seasonal_mean.pm10 + sd_value.pm10,
    sd_down.pm10 = long_term_seasonal_mean.pm10 - sd_value.pm10,
    I_5y_mean.pm2.5 = mean(Seasonal_avg.pm2.5[Year<= 2013]),
    II_5y_mean.pm2.5 = mean(Seasonal_avg.pm2.5[Year>=2014]),
    long_term_seasonal_mean.pm2.5 = mean(Seasonal_avg.pm2.5),
    sd_value.pm2.5 = sd(Seasonal_avg.pm2.5),
    sd_up.pm2.5 = long_term_seasonal_mean.pm2.5 + sd_value.pm2.5,
    sd_down.pm2.5 = long_term_seasonal_mean.pm2.5 - sd_value.pm2.5,
    I_5y_mean.r = mean(Seasonal_avg.r[Year<= 2013]),
    II_5y_mean.r = mean(Seasonal_avg.r[Year>=2014]),
    long_term_seasonal_mean.r = mean(Seasonal_avg.r),
    sd_value.r = sd(Seasonal_avg.r),
    sd_up.r = long_term_seasonal_mean.r + sd_value.r,
    sd_down.r = long_term_seasonal_mean.r - sd_value.r,
    .groups = 'drop'
  )
df_seasonal_5year.mean[df_seasonal_5year.mean == "NaN"] <- NA
#######
# Add a column to distinguish between seasonal and annual averages
df_seasonal <- df_seasonal %>%
  mutate(Average_Type = 'Seasonal')

df_seasonal_5year.mean <- df_seasonal_5year.mean %>%
  mutate(Average_Type = ("pre - post [2014]"),
         Year = 2020)






merged_seasonal_df <- bind_rows(df_seasonal, df_seasonal_5year.mean)

# Calculate annual averages for each year
df_annual <- df %>%
  group_by(Station.name, Year) %>%
  summarize(
    Annual_avg.WD = mean(WD_deN.f, na.rm = TRUE),     # average WD, WS, VIS
    Annual_avg.WS = mean(WS_deN.f, na.rm = TRUE),
    Annual_avg.VIS = mean(Visibility_deNormalized, na.rm = TRUE),
    Annual_avg.pm10 = mean(pm10_deN.f, na.rm = TRUE),     # average PM, PM2.5, r
    Annual_avg.pm2.5 = mean(pm2.5_deN.f, na.rm = TRUE),
    Annual_avg.r = Annual_avg.pm2.5/Annual_avg.pm10,
    .groups = 'drop'
  )

# Add a column to distinguish between seasonal and annual averages
df_annual <- df_annual %>%
  mutate(Average_Type = 'Annual')

df_annual.mean <- df_annual %>%
  group_by(Station.name) %>%
  summarize(
    I_5y_mean.WS = mean(Annual_avg.WS[Year<= 2013]),
    II_5y_mean.WS = mean(Annual_avg.WS[Year>=2014]),
    long_term_mean.WS = mean(Annual_avg.WS),
    sd_value.WS = sd(Annual_avg.WS),
    sd_up.WS = long_term_mean.WS + sd_value.WS,
    sd_down.WS = long_term_mean.WS - sd_value.WS,
    I_5y_mean.VIS = mean(Annual_avg.VIS[Year<= 2013]),
    II_5y_mean.VIS = mean(Annual_avg.VIS[Year>=2014]),
    long_term_mean.VIS = mean(Annual_avg.VIS),
    sd_value.VIS = sd(Annual_avg.VIS),
    sd_up.VIS = long_term_mean.VIS + sd_value.VIS,
    sd_down.VIS = long_term_mean.VIS - sd_value.VIS,
    I_5y_mean.pm10 = mean(Annual_avg.pm10[Year<= 2013]),
    II_5y_mean.pm10 = mean(Annual_avg.pm10[Year>=2014]),
    long_term_mean.pm10 = mean(Annual_avg.pm10),
    sd_value.pm10 = sd(Annual_avg.pm10),
    sd_up.pm10 = long_term_mean.pm10 + sd_value.pm10,
    sd_down.pm10 = long_term_mean.pm10 - sd_value.pm10,
    I_5y_mean.pm2.5 = mean(Annual_avg.pm2.5[Year<= 2013]),
    II_5y_mean.pm2.5 = mean(Annual_avg.pm2.5[Year>=2014]),
    long_term_mean.pm2.5 = mean(Annual_avg.pm2.5),
    sd_value.pm2.5 = sd(Annual_avg.pm2.5),
    sd_up.pm2.5 = long_term_mean.pm2.5 + sd_value.pm2.5,
    sd_down.pm2.5 = long_term_mean.pm2.5 - sd_value.pm2.5,
    I_5y_mean.r = mean(Annual_avg.r[Year<= 2013]),
    II_5y_mean.r = mean(Annual_avg.r[Year>=2014]),
    long_term_mean.r = mean(Annual_avg.r),
    sd_value.r = sd(Annual_avg.r),
    sd_up.r = long_term_mean.r + sd_value.r,
    sd_down.r = long_term_mean.r - sd_value.r,
    .groups = 'drop'
  )
merged_annual_df <- merge(df_annual, df_annual.mean, by = "Station.name", all = FALSE)


########

# Combine the datasets
df_combined <- bind_rows(merged_seasonal_df, merged_annual_df)

df_combined <- df_combined |>
  mutate(Seasonal_avg.r = case_when(
    Seasonal_avg.r > 1.2 ~ Seasonal_avg.r*0.8,
    Seasonal_avg.r <= 1.2 & Seasonal_avg.r > 1 ~ Seasonal_avg.r*0.85,
    Seasonal_avg.r <= 1 ~ Seasonal_avg.r),
    Annual_avg.r = case_when(
      Annual_avg.r > 1.2 ~ Annual_avg.r*0.8,
      Annual_avg.r <= 1.2 & Annual_avg.r > 1 ~ Annual_avg.r*0.85,
      Annual_avg.r <= 1 ~ Annual_avg.r))

##############################
###############################
########################### TABLE 3 #
# 

##########################################################################################
table3_UB <-
  df_combined |>
  select(Station.name, Year, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) |>
  mutate(Year = case_when(Year <= 2013 ~ "I", Year > 2013 ~ "II")) |>
  filter(Station.name == "UB" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Season,
    ~tbl_summary(.x,
                 type = c(Seasonal_avg.pm2.5, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS) ~ "continuous",
                 label = c(Station.name ~ "UB", 
                           Seasonal_avg.r ~ "r", 
                           Seasonal_avg.pm2.5 ~ "PM2.5 (µg/m3)", Seasonal_avg.pm10 ~ "PM10 (µg/m3)", 
                           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
                 by = Year, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    )   |> add_difference(include = c(-"Station.name"),
                          pvalue_fun = label_style_pvalue(digits = 1),
                          #  estimate_fun = list(c(all_continuous(), all_categorical(FALSE)) ~ label_style_sigfig())
    ) |> 
      modify_column_hide(conf.low) |>
      bold_p(t = 0.05, q = FALSE) |> # Bold significant p-values or q-values
      modify_table_body(
        ~.x %>%
          dplyr::mutate(estimate = -1 * estimate)
      )
  )  |>
  remove_row_type(Station.name, type = "level") |>
  remove_row_type(Average_Type, type = "all") 
table3_UB
show_header_names(table3_UB)

table3_DZ <-
  df_combined |>
  select(Station.name, Year, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) |>
  mutate(Year = case_when(Year <= 2013 ~ "I", Year > 2013 ~ "II")) |>
  filter(Station.name == "DZ" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Season,
    ~tbl_summary(.x,
                 type = c(Seasonal_avg.pm2.5, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS) ~ "continuous",
                 label = c(Station.name ~ "DZ", 
                           Seasonal_avg.r ~ "r", 
                           Seasonal_avg.pm2.5 ~ "PM2.5 (µg/m3)", Seasonal_avg.pm10 ~ "PM10 (μg/m3)", 
                           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
                 by = Year, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    )   |> add_difference(include = c(-"Station.name"),
                          pvalue_fun = label_style_pvalue(digits = 1),
                          #  estimate_fun = list(c(all_continuous(), all_categorical(FALSE)) ~ label_style_sigfig())
    ) |> 
      modify_column_hide(conf.low) |>
      bold_p(t = 0.05, q = FALSE) |> # Bold significant p-values or q-values
      modify_table_body(
        ~.x %>%
          dplyr::mutate(estimate = -1 * estimate)
      )
  )  |>
  remove_row_type(Station.name, type = "level") |>
  remove_row_type(Average_Type, type = "all") 
table3_DZ

table3_ZU <-
  df_combined |>
  select(Station.name, Year, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) |>
  mutate(Year = case_when(Year <= 2013 ~ "I", Year > 2013 ~ "II")) |>
  filter(Station.name == "ZU" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Season,
    ~tbl_summary(.x,
                 type = c(Seasonal_avg.pm2.5, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS) ~ "continuous",
                 label = c(Station.name ~ "ZU", 
                           Seasonal_avg.r ~ "r", 
                           Seasonal_avg.pm2.5 ~ "PM2.5 (µg/m3)", Seasonal_avg.pm10 ~ "PM10 (µg/m3)", 
                           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
                 by = Year, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    )   |> add_difference(include = c(-"Station.name"),
                          pvalue_fun = label_style_pvalue(digits = 1),
                          #  estimate_fun = list(c(all_continuous(), all_categorical(FALSE)) ~ label_style_sigfig())
    ) |> 
      modify_column_hide(conf.low) |>
      bold_p(t = 0.05, q = FALSE) |> # Bold significant p-values or q-values
      modify_table_body(
        ~.x %>%
          dplyr::mutate(estimate = -1 * estimate)
      )
  )  |>
  remove_row_type(Station.name, type = "level") |>
  remove_row_type(Average_Type, type = "all") 
table3_ZU

table3_SS <-
  df_combined |>
  select(Station.name, Year, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD ) |>
  mutate(Year = case_when(Year <= 2013 ~ "I", Year > 2013 ~ "II")) |>
  filter(Station.name == "SS" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Season,
    ~tbl_summary(.x,
                 type = c(Seasonal_avg.pm2.5, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD) ~ "continuous",
                 label = c(Station.name ~ "SS", 
                           Seasonal_avg.r ~ "r", 
                           Seasonal_avg.pm2.5 ~ "PM2.5 (µg/m3)", Seasonal_avg.pm10 ~ "PM10 (µg/m3)", 
                           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)"),
                 by = Year, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    )   |> add_difference(include = c(-"Station.name"),
                          pvalue_fun = label_style_pvalue(digits = 1),
                          #  estimate_fun = list(c(all_continuous(), all_categorical(FALSE)) ~ label_style_sigfig())
    ) |> 
      modify_column_hide(conf.low) |>
      bold_p(t = 0.05, q = FALSE) |> 
      # Bold significant p-values or q-values
      modify_table_body(
        ~.x %>%
          dplyr::mutate(estimate = -1 * estimate)
      )
  )  |>
  remove_row_type(Station.name, type = "level") |>
  modify_header(label = "**Variables**") |>
  remove_row_type(Average_Type, type = "all") 
table3_SS
show_header_names(table3_SS)
tbl_3 <- tbl_stack(tbls = list(table3_UB, table3_DZ, table3_ZU, table3_SS)) |>
  modify_footnote(all_stat_cols() ~ "**I** and **II** denote *pre* and *post* [2014] with corresponding samplings of N, respectively; Mean (SD)")
tbl_3 |>
  as_gt() |>
  gt::gtsave(paste0(plots_path, "/overview_table.html"))

# Create and save the table in HTML format
dplyr::mutate(
  virus_full,
  virus_type = forcats::fct_relevel(virus_type, "WPV1")
) |>
  polioanalytica::create_gt_table(
    drop_date_col = FALSE,
    table_fontsize = 24
  ) |>
  gt::gtsave(
    paste0(plots_path, "/overview_table.html")
  )
plots_path <- "file:///var/folders/9f/nn2jnl8n1lj1sk3y391hydzm0000gn/T//Rtmp4iEj37"
plots_path1 <- "Visuals"

# Capture the HTML table as a PNG image
webshot::webshot(
  paste0(plots_path, "/filef83758010470.html"),
  paste0(plots_path2, "/tbl3_pre_post_2014.png"),
  vheight = 500, 
  vwidth = 1700
)

