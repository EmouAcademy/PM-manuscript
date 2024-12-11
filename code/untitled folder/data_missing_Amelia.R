############### Cleaning and Handling MISSING data #########################
# I. Mining bad data
# a) explore the spikes
# b) check the spikes against the other data whether to keep it or delete it
# c) do iterative process with a) and b) for all data elements
# II. Remove the bad data
# a) replace with NA
# b) replace with Median
# c) replace with Mean
# III. Data gap filling, carefully choosing the correct strategy
# a) fill the data based on the seasonal/ daily variations/ and consider trend
# b) fill the data with the median/mean or with the some relations
# c) Search for the suitable... method
#############################################################################
library(tidyverse)
library(ggplot2)


data_amelia <- data |>
  select(Station.name, Year, Month, Day, Hour, Visibility, WD, WS, pm10, pm2.5)

data_amelia |>
  is.na() |>
  colSums()



######### Missing value packages 
########### 1. Time series values (Mice, Mi, Amelia) Amelia is the best.
library(Amelia)

df_amelia <- amelia(data_amelia, cs = "Station.name")
plot(df_amelia)
plot(df_amelia$imputations$imp1$Visibility, data_amelia$Visibility)


plot(density(df2$WS_normal_dist, na.rm=TRUE)) 

df1_imp_1 <- df_amelia$imputations$imp1
df1_imp_2 <- df_amelia$imputations$imp2
df1_imp_3 <- df_amelia$imputations$imp3
df1_imp_4 <- df_amelia$imputations$imp4
df1_imp_5 <- df_amelia$imputations$imp5

plot(density(data_amelia$WS, na.rm=TRUE))  
lines(density(df1_imp_1$WS), col = "red")  
lines(density(df1_imp_2$WS), col = "blue")  
lines(density(df1_imp_3$WS), col = "green")  
lines(density(df1_imp_4$WS), col = "purple")  
lines(density(df1_imp_5$WS), col = "yellow")  

plot(density(data_amelia$Visibility, na.rm=TRUE))  
lines(density(df1_imp_1$Visibility), col = "red")  
lines(density(df1_imp_2$Visibility), col = "blue")  
lines(density(df1_imp_3$Visibility), col = "green")  
lines(density(df1_imp_4$Visibility), col = "purple")  
lines(density(df1_imp_5$Visibility), col = "yellow")  



df1 <- df |>
  mutate(pm10_normal_dist = log(pm10_miss + 0.00003),
         pm2_normal_dist = log(pm2_miss + 0.001),
         Visibility_normal_dist = log(Visibility+0.001),
         WS_normal_dist = log1p(WS+2))
plot(df1)
df2 <- df |>
  mutate(pm10_normal_dist = exp(pm10_miss + 0.00003),
         pm2_normal_dist = exp(pm2_miss + 0.001),
         Visibility_normal_dist = exp(Visibility+0.001),
         WS_normal_dist = expm1(WS+2))

exp

plot(density(df2$pm10_normal_dist, na.rm=TRUE))
plot(density(df2$pm2_normal_dist, na.rm=TRUE))
plot(density(df2$Visibility_normal_dist, na.rm=TRUE))
plot(density(df2$WS_normal_dist, na.rm=TRUE))

plot(density(df$Visibility, na.rm=TRUE))
plot(density(df$WS, na.rm=TRUE))
plot(density(df$pm10_miss, na.rm=TRUE))
plot(density(df$pm2_miss, na.rm=TRUE))
hist(df$pm10_miss, col="grey", border="white")
hist(df$pm2_miss, col="grey", border="white")

df2 <- df1 |>
  select(Date, Year, Month, Day, Hour, Station.name, 
         pm10_normal_dist, pm2_normal_dist, 
         WS_normal_dist, Visibility_normal_dist) 
  ggplot()

write_csv(df2, file = "/Users/munkhtsetseg/WORK/Research/Data/02_data_tidy/normalized_df2.csv")

  


log(0.001)

df2 <- read.csv("normalized_df2.csv")

glimpse(df2)

df2$Station.name <- as.factor(df2$Station.name) 

df2$Date <- as.Date(with(df2, paste(Year, Month, Day, Hour,sep="-")),
                   "%Y-%m-%d %H:%M:%S")

library(lubridate)

df2 <- df2 %>%
  mutate(date = make_datetime(Year, Month, Day, Hour))

df2[is.na(df2) | df2=="Inf"] <- NA

df2$Date <- as.POSIXct(as.numeric(as.character(df2$Date)), origin='1970-01-01')

df2 |>
  is.na() |>
  colSums()

plot(density(df2$pm10_normal_dist, na.rm=TRUE)) 

df_amelia <- amelia(df2, m = 5, ts = "Date", cs = "Station.name")
plot(df_amelia)

plot(density(df2$WS_normal_dist, na.rm=TRUE)) 

df1_imp_1 <- df_amelia$imputations$imp1
df1_imp_2 <- df_amelia$imputations$imp2
df1_imp_3 <- df_amelia$imputations$imp3
df1_imp_4 <- df_amelia$imputations$imp4
df1_imp_5 <- df_amelia$imputations$imp5

plot(density(df1$pm10_normal_dist, na.rm=TRUE))  
lines(density(df1_imp_1$pm10_normal_dist), col = "red")  
lines(density(df1_imp_2$pm10_normal_dist), col = "blue")  
lines(density(df1_imp_3$pm10_normal_dist), col = "green")  
lines(density(df1_imp_4$pm10_normal_dist), col = "purple")  
lines(density(df1_imp_5$pm10_normal_dist), col = "yellow")  


plot(density(df1$pm2_miss, na.rm=TRUE)) 

  
  
  
##### ------ t-test
df_miss_pm <- df |>
  mutate(pm2_miss = is.na(pm2_fill), pm10_miss = is.na(pm10_fill))

obs1 <- df_miss_pm |>
  filter(Month <= 3) |>
  pull(pm2_miss)

obs2 <- df_miss_pm |>
  filter(Month >= 9) |>
  pull(pm2_miss)

t.test(obs1, obs2)

#### Vim package: visualizing missing value
library(VIM)
df_miss_pm |>
  aggr(combined = TRUE, numbered = TRUE)

df |>
  select(Station.name, PM10) |>
  spineMiss()

# Prepare data for plotting and draw a mosaic plot
df %>%
  # Create a dummy variable for US-produced movies
  mutate(is_UB_data = grepl("UB", Station.name)) %>%
  # Draw mosaic plot
  mosaicMiss(highlight = "PM2", 
             plotvars = c("is_UB_data", "Month"))

median(abs(biopics$earnings - model_1$fitted.values), na.rm = TRUE)
