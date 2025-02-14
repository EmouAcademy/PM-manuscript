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
library(gtsummary)
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
############################ TABLE 2: Seasonal to Annual means  #
#   table1_site: seasonal
#.  table2_site: annual
#.  tbl_site: merge table1 and table2
#.   stack all tables for all
########################################################

# UB
table1_UB <-
  df_combined %>%
  select(Station.name, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) %>%
  rename(r = Seasonal_avg.r, pm2.5 = Seasonal_avg.pm2.5, pm10 = Seasonal_avg.pm10, WS = Seasonal_avg.WS, WD = Seasonal_avg.WD, VIS = Seasonal_avg.VIS) |>
  filter(Station.name == "UB" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Average_Type,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                  label = c(Station.name ~ "UB"), 
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
  rename(r = Annual_avg.r,  pm2.5 = Annual_avg.pm2.5, pm10 = Annual_avg.pm10, 
         WS = Annual_avg.WS, WD = Annual_avg.WD, VIS = Annual_avg.VIS) %>%
  tbl_strata(
    strata = Average_Type,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                  label = c(Station.name ~ "UB"), 
                 #           Annual_avg.r ~ "r", 
                 #            Annual_avg.pm2.5 ~ "PM2.5 (mug/m3)", Annual_avg.pm10 ~ "PM10 (mug/m3)", 
                 #            Annual_avg.WS ~ "WS (m/s)", Annual_avg.WD ~ "WD (deg)",  Annual_avg.VIS ~ "VIS (km)"),
                 #  by = Season, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    ) 
  ) |>
 # bold_labels(include = c(-Station.name))
  remove_row_type(Station.name, type = "level") 

table2_UB

tbl_UB <- tbl_merge(tbls = list(table1_UB, table2_UB),
          tab_spanner = c("**Seasonal**", "**Annual**")) |>
          modify_header(label = "**Variables**")
tbl_UB


##########################################################################################
# DZ
table1_DZ <-
  df_combined %>%
  select(Station.name, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) %>%
  rename(r = Seasonal_avg.r, pm2.5 = Seasonal_avg.pm2.5, pm10 = Seasonal_avg.pm10, WS = Seasonal_avg.WS, WD = Seasonal_avg.WD, VIS = Seasonal_avg.VIS) |>
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
  rename(r = Annual_avg.r,  pm2.5 = Annual_avg.pm2.5, pm10 = Annual_avg.pm10, 
         WS = Annual_avg.WS, WD = Annual_avg.WD, VIS = Annual_avg.VIS) %>%
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


##########################################################################################
# ZU 
table1_ZU <-
  df_combined %>%
  select(Station.name, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD,  Seasonal_avg.VIS ) %>%
  rename(r = Seasonal_avg.r, pm2.5 = Seasonal_avg.pm2.5, pm10 = Seasonal_avg.pm10, WS = Seasonal_avg.WS, WD = Seasonal_avg.WD, VIS = Seasonal_avg.VIS) |>
    filter(Station.name == "ZU" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Average_Type,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                 label = c(Station.name ~ "ZU"), 
                 #           Seasonal_avg.r ~ "r", 
                 #           Seasonal_avg.pm2.5 ~ "PM2.5 (mug/m3)", Seasonal_avg.pm10 ~ "PM10 (mug/m3)", 
                 #           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
                 by = Season, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    ) 
  ) |>
  remove_row_type(Station.name, type = "level") 
table1_ZU 

table2_ZU <-
  df_combined %>%
  filter(Station.name == "ZU" & Average_Type == "Annual") %>%
  select(Station.name, Average_Type, Annual_avg.r, Annual_avg.pm2.5, Annual_avg.pm10, Annual_avg.WS, Annual_avg.WD,  Annual_avg.VIS) %>%
  rename(r = Annual_avg.r,  pm2.5 = Annual_avg.pm2.5, pm10 = Annual_avg.pm10, 
         WS = Annual_avg.WS, WD = Annual_avg.WD, VIS = Annual_avg.VIS) %>%
  tbl_strata(
    strata = Average_Type,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                 label = c(Station.name ~ "ZU"), 
                 #           Annual_avg.r ~ "r", 
                 #            Annual_avg.pm2.5 ~ "PM2.5 (mug/m3)", Annual_avg.pm10 ~ "PM10 (mug/m3)", 
                 #            Annual_avg.WS ~ "WS (m/s)", Annual_avg.WD ~ "WD (deg)",  Annual_avg.VIS ~ "VIS (km)"),
                 #  by = Season, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    ) 
  ) |>
  remove_row_type(Station.name, type = "level") 
table2_ZU

tbl_ZU <- tbl_merge(tbls = list(table1_ZU, table2_ZU)) |>
  modify_spanning_header(everything() ~ NA_character_)
tbl_ZU


##########################################################################################
# SS
table1_SS <-
  df_combined %>%
  select(Station.name, Season, Average_Type, Seasonal_avg.r, Seasonal_avg.pm2.5, Seasonal_avg.pm10, Seasonal_avg.WS, Seasonal_avg.WD) %>%
  rename(r = Seasonal_avg.r, pm2.5 = Seasonal_avg.pm2.5, pm10 = Seasonal_avg.pm10, WS = Seasonal_avg.WS, WD = Seasonal_avg.WD) |>
  filter(Station.name == "SS" & Average_Type == "Seasonal") %>%
  tbl_strata(
    strata = Average_Type,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                 label = c(Station.name ~ "SS"), 
                 #           Seasonal_avg.r ~ "r", 
                 #           Seasonal_avg.pm2.5 ~ "PM2.5 (mug/m3)", Seasonal_avg.pm10 ~ "PM10 (mug/m3)", 
                 #           Seasonal_avg.WS ~ "WS (m/s)", Seasonal_avg.WD ~ "WD (deg)",  Seasonal_avg.VIS ~ "VIS (km)"),
                 by = Season, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    ) 
  ) |>
  remove_row_type(Station.name, type = "level") 
table1_SS 

table2_SS <-
  df_combined %>%
  filter(Station.name == "SS" & Average_Type == "Annual") %>%
  select(Station.name, Average_Type, Annual_avg.r, Annual_avg.pm2.5, Annual_avg.pm10, Annual_avg.WS, Annual_avg.WD) %>%
  rename(r = Annual_avg.r,  pm2.5 = Annual_avg.pm2.5, pm10 = Annual_avg.pm10, 
         WS = Annual_avg.WS, WD = Annual_avg.WD) %>%
  tbl_strata(
    strata = Average_Type,
    ~tbl_summary(.x,
                 # type = Station.name ~ "continuous",
                 label = c(Station.name ~ "SS"), 
                 #           Annual_avg.r ~ "r", 
                 #            Annual_avg.pm2.5 ~ "PM2.5 (mug/m3)", Annual_avg.pm10 ~ "PM10 (mug/m3)", 
                 #            Annual_avg.WS ~ "WS (m/s)", Annual_avg.WD ~ "WD (deg)",  Annual_avg.VIS ~ "VIS (km)"),
                 #  by = Season, missing = "no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 # digits = 2
    ) 
  ) |>
  remove_row_type(Station.name, type = "level") 
table2_SS

tbl_SS <- tbl_merge(tbls = list(table1_SS, table2_SS)) |>
  modify_spanning_header(everything() ~ NA_character_) 

##########################################################################################
tbl2_seasonal_to_annual <- tbl_stack(tbls = list(tbl_UB, tbl_DZ, tbl_ZU, tbl_SS)) |>
modify_footnote(all_stat_cols() ~ "N; Mean (SD)")
tbl2_seasonal_to_annual |>
  as_gt() |>
  gt::gtsave(
    ("Visuals/tbl2_seasonal_to_annual.png")
  )
plots_path <- "file:////var/folders/9f/nn2jnl8n1lj1sk3y391hydzm0000gn/T//Rtmp4iEj37"
plots_path1 <- "Visuals"
plots_path2 <- "submission/images"
# Capture the HTML table as a PNG image
webshot::webshot(
  paste0(plots_path, "/filef837ba5bcf5.html"),
  paste0(plots_path2, "/tbl2_seasonal_to_annual.png"),
  vheight = 400, 
  vwidth = 700
)
