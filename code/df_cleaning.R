############### Cleaning and Handling MISSING data #########################
# I. Mining bad data
# a) explore the spikes
# b) check the spikes against the other data whether to keep it or delete it
# c) do iterative process with a) and b) for all data elements

## 1. data cleaning
###  1. UB
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
###  2. DZ
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
###  3. SSh
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
###  4. ZU
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
######### Decision
##################################################################
# II. Remove the bad data
# a) replace with NA
# b) replace with Median or use Ratio = pm2.5/pm10
# c) replace with Mean
# III. Data gap filling, carefully choosing the correct strategy
# a) fill the data based on the seasonal/ daily variations/ and consider trend
# b) fill the data with the median/mean or with the some relations
# c) Search for the suitable... method
#############################################################################
#!/usr/bin/env Rscript
library(tidyverse)
library(ggplot2)
library(VIM)
library(readr)




df <- read.csv("data/raw/Preprocessing data.csv")

#Data$Station.name <- as.factor(Data$Station.name)
#view(Data)

## And remove duplicates
df1 <- df %>%
  distinct()


##### Converting types, renaming.
df1 <-  df1 |>
  mutate(pm2_swap=PM10, pm10_swap=PM2, ratio = pm2_swap/pm10_swap)

glimpse(df1)
df1$Date <- as.Date(df1$Date)
df1$Station.name <- as.factor(df1$Station.name) 





df1 |>
  dplyr::select(Station.name, pm2_swap) |>
  spineMiss()



df1 |>
  dplyr::select(Station.name, pm10_swap) |>
  spineMiss()


##### 1. Remove spikes
### spikes to NA, PM10 must be greater than PM2, and PM must be > 0
## a. data range constrain 0-7
df_spike_up <- df1 |>
  mutate(pm10_miss = replace(pm10_swap, pm10_swap > 7, NA), 
         pm2_miss = replace(pm2_swap, pm2_swap > 7 , NA))
plot(df_spike_up$pm10_miss, df_spike_up$pm2_miss)

df_spike_uplow <- df_spike_up |>
  mutate(pm10 = replace(pm10_miss, pm10_miss ==0, NA), 
         pm2.5 = replace(pm2_miss, pm2_miss ==0, NA))
plot(df_spike_uplow$pm10, df_spike_uplow$pm2.5)

## b. It is suggested that pm10 > pm2.5 value. 
####  But, this rule somehow cannot seen, particularly when the PMs low.
####. Maybe it is sensor accuracy. So:
### b-01 When pm10 =<0.001; if pm2.5>pm10*3.5 ===> pm2.5 <- NA
### b-02 When pm10 >0.5 and pm2.5 < 0.03 ===> pm2.5 <- NA (Note: It is detected with the site)
df_spike_ratio01 <- df_spike_uplow |>
  mutate(pm2.5 = replace(pm2.5, pm2.5 > pm10*3.5 | pm10>0.5 & pm2.5<0.03, NA))
### b-03 When pm10 >0.2 and pm2.5 < 0.01 ===> pm2.5 <- NA (Note: It is detected with UB)
df_spike7_0 <- df_spike_ratio01 |>
  mutate(pm2.5 = replace(pm2.5, pm10>0.2 & pm2.5<0.01, NA))
plot(df_spike7_0$pm10, df_spike7_0$pm2.5)
abline(a = 0, b = 1)

## Remove ZU bad data
df_spike7_0_ZU <- df_spike7_0 |>
  mutate(pm2.5 = replace(pm2.5, pm10>0.25 & pm2.5<0.1, NA))
plot(df_spike7_0_ZU$pm10, df_spike7_0_ZU$pm2.5)
abline(a = 0, b = 1)

###  1. UB
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
df_spike7_0_ZU |>
  ggplot(aes(pm10, pm2.5, color=Station.name)) +
  geom_point() 


data <- df_spike7_0_ZU |>
  mutate(id = "Gobi") 

data <- data |>
  mutate(id = replace(id, Station.name == "UB", "Urban") )

data$Station.name <- factor(data$Station.name, 
                            levels = c("UB", "Dalanzadgad",  "Zamynuud", "Sainshand"))  
levels(data$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')

df_clean <- data |>
            mutate(r = pm2.5/pm10) |>
            select(id, Station.name, Date, Year, Month, Day, Hour, 
                   Visibility, WD, WS, OPC, pm10, pm2.5, r )

write.csv(df_clean, file='data/processed/df.csv')


#################### end of GOAL #############################

########### RESULTS
######### 3.1. Two distinct spatial variations of PM10 and PM2.5 concentrations
########## UB: urban
########## Gobi: dust
######### 3.2. Temporal variations of PM10 and PM2.5 concentrations
######### 3.3. Meteorological effects on variations of PM10 and PM2.5 concentrations

########### RESULTS
######### 3.1. Two distinct spatial variations of PM10 and PM2.5 concentrations
########## UB: urban
########## Gobi: dust
######### 3.2. Temporal variations of PM10 and PM2.5 concentrations
######### 3.3. Meteorological effects on variations of PM10 and PM2.5 concentrations

# data |>
#   filter(Station.name == "Dalanzadgad") |>
#   ggplot(aes(pm10, pm2.5, size = WS, colour = Month)) +
#   geom_point()

# data |>
#   filter(Station.name == "Zamynuud") |>
#   ggplot(aes(pm10, pm2.5, size = WS, color=Day, group = interaction(Month, Day))) +
#   geom_point()

# data |>
#   filter(Station.name == "Sainshand") |>
#   ggplot(aes(pm10, pm2.5, size = WS, color=Day, group = interaction(Month, Day))) +
#   geom_point()

