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
library(tidyverse)
library(ggplot2)

setwd("~/2 Data Processing")
df <- read.csv("~/Data Input/Preprocessing data/Preprocessing data.csv")

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


##### 1. Remove spikes
##  data range explore
breaks_pm10 <- c(min(df1$pm10_swap, na.rm = T), 0.001, 3, max(df1$pm10_swap, na.rm = T))
ggplot(df1, aes(pm10_swap)) + 
  geom_histogram(breaks=breaks_pm10)

breaks_pm2 <- c(min(df1$pm2_swap, na.rm = T), 0.001, 3, max(df1$pm2_swap, na.rm = T))
ggplot(df1, aes(pm2_swap)) + 
  geom_histogram(breaks=breaks_pm2)
plot(df1$PM10, df1$PM2)

### spikes to NA, PM10 must be greater than PM2, and PM must be > 0
## a. data range constrain 0-7
df_spike_up <- df1 |>
  mutate(pm10_miss = replace(pm10_swap, pm10_swap > 7, NA), 
         pm2_miss = replace(pm2_swap, pm2_swap > 7 , NA))
write_csv(df2, file = "/Users/munkhtsetseg/WORK/Research/Data/02_data_tidy/clean_df2.csv")
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


# df2 <- df1 |>
#  mutate(pm10_miss = replace(pm10_swap, pm10_swap > 6 | pm10_swap < pm2_swap, NA), 
#         pm2_miss = replace(pm2_swap, pm2_swap > 6 | pm2_swap > pm10_swap, NA),
#         ratio_miss = ifelse(pm10_miss >0, pm2_miss/pm10_miss, NA))
# write_csv(df2, file = "/Users/munkhtsetseg/WORK/Research/Data/02_data_tidy/clean_df2.csv")

df2$Date < as.Date(df2$Date)
###  1. UB
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
df_spike7_0 |>
  filter(Station.name == "UB") |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility), title(main="UB")) +
  geom_point()



df_spike7_0 |>
  filter(Station.name == "Dalanzadgad") |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point()

df_spike7_0 |>
  filter(Station.name == "Zamynuud") |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point()

df_spike7_0 |>
  filter(Station.name == "Sainshand") |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Year)
####     2. is it temporal or continuous for some time 
### 2014, PM2.5 > PM10 for September ::: June-August has no data: Small variations:; is difficult; Usually scattered
### 2015, PM2.5 > PM10 for April ::: January-March has no data: Is able to remove
###       PM2.5 > PM10 for October : Should remove???! Small value as 2014 case.
### 2016, PM2.5 = 0 Хавар намарт шуурганы нөлөө их бж maybe. 0 болон 0.001 холимог бна
### 2017, PM2.5 = 0 constant small value for February. error to be removed

## UB site: by year

### 2014,
df_spike7_0 |>
  filter(Station.name == "UB", Year == 2014) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

df_spike7_0 |>
  filter(Station.name == "UB", Year == 2014,  Month==9) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

### 2015,
df_spike7_0 |>
  filter(Station.name == "UB", Year == 2015) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

df_spike7_0 |>
  filter(Station.name == "UB", Year == 2015,  Month==10) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

#### 2016. Хавар намарт шуурганы нөлөө их бж. 0 болон 0.001 холимог бна
df_spike7_0 |>
  filter(Station.name == "UB", Year == 2016) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

error2 <- df_spike7_0 |>
  filter(Station.name == "UB", Year == 2016,  Month==3 | Month==4 | Month==5 | Month ==11 | Month==12) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point()+ 
  facet_wrap(~Month)

#### 2017. 
df_spike7_0 |>
  filter(Station.name == "UB", Year == 2017) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)
#### 2017. February... strange, better to remove.
 df_spike7_0 |>
  filter(Station.name == "UB", Year == 2017,  Month==2 ) |>
  ggplot(aes(Date, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

### Decision: technical or equipment error. pm2_miss==0 needs to be changed as NA.

 
#### Sainshand
 
 df_spike7_0 |>
   filter(Station.name == "Sainshand") |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Year)


 #### 2017. 
 df_spike7_0 |>
   filter(Station.name == "Sainshand", Year == 2011) |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 #### 2017. February... strange, better to remove.
 df_spike7_0 |>
   filter(Station.name == "Sainshand", Year == 2017,  Month==2 ) |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 

 #### Dalanzadgad
 
 df_spike7_0 |>
   filter(Station.name == "Dalanzadgad") |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Year)
 
 
 #### 2017. 
 df_spike7_0 |>
   filter(Station.name == "Dalanzadgad", Year == 2010) |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 #### 2017. February... strange, better to remove.
 df_spike7_0 |>
   filter(Station.name == "Sainshand", Year == 2012,  Month==6 ) |>
   ggplot(aes(pm10, pm2.5_1, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 
 
 ### When pm10 =<0.001; if pm2.5>pm10 ===> pm2.5 <- NA
 df_spike7_0 <- df_spike7_0 |>
   mutate(pm2.5_1 = replace(pm2.5, pm10 <0.01 & pm2.5 > pm10, NA))
 
 #### Zamynuud
 
 df_spike7_0 |>
   filter(Station.name == "Zamynuud") |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Year)
 
 
 #### 2017. 
 df_spike7_0 |>
   filter(Station.name == "Zamynuud", Year == 2011) |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 #### 2017. February... strange, better to remove.
 df_spike7_0 |>
   filter(Station.name == "Zamynuud", Year == 2011,  Month==2 | Month==3  ) |>
   ggplot(aes(Date, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 
 
 

df2_SSh <- df_spike7 |>
  filter(Station.name == "Sainshand")
plot(df2_SSh$pm10_miss, df2_SSh$pm2_miss)

###### Шуурсан бололтой
df2 |>
  filter(Station.name == "Sainshand" & Year ==2011 & Month ==6) |>
  ggplot(aes(Date, pm10_miss, size = WS, color=Visibility)) +
  geom_point()


# DZ site 
df2_DZ <- df_spike7 |>
  filter(Station.name == "Dalanzadgad") |>
  ggplot(aes(pm10_miss, pm2_miss, size = WS)) +
  geom_point() +
  facet_wrap(~Year)
# DZ site: make sure of technical error for year of 2011 and 2012 
 df_spike7|>
  filter(Station.name == "Dalanzadgad", Year==2011 & Month==10)|>
  ggplot(aes(Hour, pm2_swap, size = WS)) +
  geom_point() +
   facet_wrap(~Day)
 
 df_spike7|>
   filter(Station.name == "Dalanzadgad", Year==2012 & Month==7 | Month==6)|>
   ggplot(aes(pm10_swap, pm2_swap, size = WS)) +
   geom_point() +
   facet_wrap(~Month)

  mutate(pm2_miss = replace(pm2_swap, pm2_swap > pm10_swap*3.5 | pm2_swap == 0, NA))
write_csv(df2_DZ, file = "/Users/munkhtsetseg/WORK/Research/Data/02_data_tidy/clean_df2_DZ.csv")
plot(df2_DZ$pm10_miss, df2_DZ$pm2_miss)
abline(a = 0, b = 1)

df2 |>
  filter(Station.name == "Dalanzadgad") |>
  mutate(pm2_miss = replace(pm2_swap, pm2_swap > pm10_swap*3.5 | pm2_swap == 0, NA)) |>
  ggplot(aes(pm10_miss, pm2_miss, size = WS)) +
  geom_point() +
  facet_wrap(~Year)


#### Zamyn uud
df2_ZU <- df2 |>
  filter(Station.name == "Zamynuud" ) |>
  mutate(pm2_miss = replace(pm2_swap, pm2_swap > pm10_swap*3.5 | pm2_swap == 0, NA),
         pm10_miss = replace(pm10_swap, Year > 2016 & pm10_swap > 0.5 | Year > 2017, NA))
write_csv(df2_ZU, file = "/Users/munkhtsetseg/WORK/Research/Data/02_data_tidy/clean_df2_ZU.csv")
plot(df2_ZU$pm10_miss, df2_ZU$pm2_miss)


df2 |>
  filter(Station.name == "Zamynuud") |>
  mutate(pm2_miss = replace(pm2_swap, pm2_swap > pm10_swap*3.5 | pm2_swap == 0, NA),
         pm10_miss = replace(pm10_swap, Year > 2016 & pm10_swap > 0.5| Year > 2017, NA)) |>
  ggplot(aes(pm2_miss, pm10_miss, size = WS)) +
  geom_point() +
  facet_wrap(~Year)




breaks_ratio <- c(min(df2$ratio_miss, na.rm = T), 0, 1, max(df2$ratio_miss, na.rm = T))
summary(breaks_ratio)

########################
#####################
######    df2 is nice. Now I can work with.
######.   Харин одоо data gap filling хийх үү?
######.   Handling missing data
###############################
###############################
######## Convert into log

df2_log <- df1 |>
  mutate(pm10_log = log(pm10_swap+0.0000000000000000001),
         pm2_log = log(pm2_swap + 0.00000001),
         WS_log = log1p(WS))

df2_log <- df2_log |>
  mutate(pm10_miss_log = replace(pm10_log, pm10_swap > 6 | pm10_swap < pm2_swap | pm10_swap == 0, NA), 
         pm2_miss_log = replace(pm2_log, pm2_swap > 6 | pm2_swap > pm10_swap |pm2_swap == 0 , NA))
write_csv(df2_log, file = "/Users/munkhtsetseg/WORK/Research/Data/02_data_tidy/clean_df2.csv")


plot(density(df2_log$WS_log, na.rm=TRUE)) 
plot(density(df1$pm10_swap, na.rm=TRUE))
plot(df2_b$pm10_swap, df2_b$pm10_normal_dist)

write_csv(df2_01, file = "/Users/munkhtsetseg/WORK/Research/Data/02_data_tidy/clean_df2_01.csv")


log(0.04)

log1p(0)
log



plot(df2$pm10_miss, df2$pm2_miss)        

###### Check 0
df0 <- df2 |>
  filter(pm2_miss == 0 | pm10_miss == 0)

plot(df2$ratio_miss, df2$ratio_adj)

######
####.   Dalanzad 2012 оны 6 сараас 7 сарын эхэн хүртэл PM10<PM2; солих <== WRONG!!!
########
df2 |>
  filter( Station.name == "Sainshand" & Year==2011 & Month ==6) |>
  ggplot(aes(x= Date, y= pm10_miss, 
             size = WS)) + 
  geom_point() +
  ylim(0,0.7)
facet_wrap(~Month)

####
ggplot(df2, aes(x= Date, y= WS, 
                color = WS)) + 
  geom_point() +
  #ylim(0,10000) +
  facet_wrap(~Station.name)

ggplot(df0, aes(x= WS, y= ratio_miss, 
                color = factor(Year))) + 
  geom_point() +
  ylim(0,3) +
  facet_wrap(~Station.name)
####

ggplot(df2, aes(x= Date, y= pm10_miss, 
                color = WS)) + 
  geom_point() +
  facet_wrap(~Station.name)


plot(df1$pm2_adj, df1$pm10_adj)
ggplot(df1, aes(pm2_adj, pm10_adj, color=Station.name, size=WS)) + 
  geom_point()

ggplot(df1, aes(pm2_miss, pm10_miss, color=Station.name, size=WS)) + 
  geom_point()




#################################
################### Not used pieces
### if PM2>PM10 , swap ------ NOT in this case
df_dal <- df2 |>
  filter(Station.name == "Dalanzadgad" & Year == 2011) |>
  transform(pm10_adj = pmax(pm2_miss, pm10_miss), 
            pm2_adj = pmin(pm2_miss, pm10_miss)) |>
  mutate(ratio_adj = pm2_adj/pm10_adj)

df_dal <- df2 |>
  transform(pm10_adj = pmax(pm2_miss, pm10_miss), 
            pm2_adj = pmin(pm2_miss, pm10_miss)) |>
  mutate(ratio_adj = pm2_adj/pm10_adj)

d <- df_dal |>
  filter(pm10_adj != pm10_miss | pm2_adj != pm2_miss)



## Remove 0 measurements. Looks like no data, or malfunction. 
arrange(PM10) 

df1[1:87840, 6][df1[1:87840, 6] == 0] <- NA


df1 <- df1 %>%
  distinct() |>
  arrange(PM2) 
df1[1:35460, 5][df1[1:35460, 5] == 0] <- NA

