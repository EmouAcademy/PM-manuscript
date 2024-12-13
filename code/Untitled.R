library(ggplot2)
library(readr)
library(tidyverse)      # scale_y_break
source(here("code/Colors.R"))
library(ggpubr)

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
########################### Plotting
threshold <- 0.05 # 50mkm/m3

df |>
  group_by(Station.name, Date) |>
  summarise(median = median(pm2.5_deN.f), 
            max = max(pm2.5_deN.f), 
            mean = mean(pm2.5_deN.f),
            uquartile = quantile(pm2.5_deN.f, prob = 0.75)) |>
  pivot_longer(-c(Station.name, Date)) |>
  ggplot(aes(x = Date, y = value), color= Station.name) +
  geom_line() +
  facet_wrap(Station.name~name, scales = 'free_y')

pol_df <- df |>
  mutate(year = year(Date),
         month = month(Date),
         ym =strftime(df$Date,format="%Y-%m")
  ) |>
  group_by(Station.name, ym, Date) |>
  summarise(#median = median(pm2.5_deN.f), 
          #  max = max(pm2.5_deN.f), 
           mean = mean(pm2.5_deN.f),
          #  uquartile = quantile(pm2.5_deN.f, prob = 0.75)
            ) |>
  pivot_longer(-c(Station.name, ym, Date)) |>
  filter(value > threshold) 


library(lubridate)  
pol_df  <- pol_df |>
 # filter(year != 2008) |>
  group_by(ym, Station.name) |>
  summarise(count = n()) |>
#  pivot_longer(-c(Station.name, ym)) |>
  


  pol_df |>
    pivot_longer(-c(Station.name, ym)) |>
  ggplot(aes(x = as.Date(as.numeric(ym)), y = value)) +
  geom_point() +
  facet_wrap(~Station.name, scales = 'free_y')
  
##################### annual changes of PMs, with trendline, correlation
library(ggbreak)
max_val <- c(200, 600, 600, 200)
## pm2.5
library(scales)
library(ggpubr)

pm10_by_year <- df |>
              filter(Year != 2008) |>
              group_by(Station.name, Year) |>
              summarise(median = median(pm10_deN.f), 
                        max = max(pm10_deN.f), 
                        mean = mean(pm10_deN.f),
                        uquartile = quantile(pm10_deN.f, prob = 0.75)) |>
  pivot_longer(-c(Station.name, Year)) |>
  ggplot(aes(x = Year, y = value), color= Station.name) +
    geom_line() +
    facet_wrap(Station.name~name, scales = 'free_y')

pm2.5_by_year <- df |>
  filter(Year != 2008, Month >10 | Month < 3 ) |>
  group_by(Station.name, Year) |>
  summarise(median = median(pm2.5_deN.f), 
            max = max(pm2.5_deN.f), 
            mean = mean(pm2.5_deN.f),
            uquartile = quantile(pm2.5_deN.f, prob = 0.75)) |>
  pivot_longer(-c(Station.name, Year)) |>
  ggplot(aes(x = Year, y = value), color= Station.name) +
  geom_line() +
  facet_wrap(Station.name~name, scales = 'free_y')

pm10_by_year <- df |>
  filter(Year != 2008) |>
  group_by(Station.name, Year) |>
  summarise(median = median(pm10_deN.f), 
            max = max(pm10_deN.f), 
            mean = mean(pm10_deN.f),
            uquartile = quantile(pm10_deN.f, prob = 0.75)) |>
  pivot_longer(-c(Station.name, Year)) |>
  ggplot(aes(x = Year, y = value), color= Station.name) +
  geom_line() +
  facet_wrap(Station.name~name)

pm2.5.f_by_year <- df |>
  filter(Year != 2008) |>
  group_by(Station.name, Year) |>
  summarise(median = median(pm2.5.f), 
            max = max(pm2.5.f), 
            mean = mean(pm2.5.f),
            uquartile = quantile(pm2.5.f, prob = 0.75)) |>
  pivot_longer(-c(Station.name, Year
                  )) |>
  ggplot(aes(x = Year, y = value), color= Station.name) +
  geom_point() +
  facet_wrap(Station.name~name, scales = 'free_y')

WS_by_year <- df |>
  filter(Year != 2008, Month >10 | Month < 3 ) |>
  group_by(Station.name, Year, Month) |>
  summarise(median = median(WS.f), 
            max = max(WS.f), 
            mean = mean(WS.f),
            uquartile = quantile(WS.f, prob = 0.75)) |>
  pivot_longer(-c(Station.name, Year, Month)) |>
  ggplot(aes(x = Year, y = value), color= Station.name) +
  geom_line() +
  facet_wrap(Station.name~name, scales = 'free_y')
