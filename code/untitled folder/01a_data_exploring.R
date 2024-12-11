######### Data Processing
######
# 1. Input the data
# 2. Remove the bad data
# 3. Replace with NA? or Median/Mean?
# 4. Fill the gap 
# 5. Search for the suitable... method
######
# I. Mining bad data
# a) explore the spikes
# b) check the spikes against the other data whether to keep it or delete it
# c) do iterative process with a) and b) for all data elements
# II. Remove the bad data
# a) replace with NA
# III. Data gap filling, carefully choosing the correct strategy
# a) fill the data based on the seasonal/ daily variations/ and consider trend
# b) fill the data with the median/mean or with the some relations
#########
library(tidyverse)
library(ggplot2)
setwd("~/2 Data Processing")
df <- read.csv("~/Data Input/Preprocessing data/Preprocessing data.csv")

#Data$Station.name <- as.factor(Data$Station.name)
#view(Data)


##### Remove duplicates
df_01 <- df %>%
  distinct()

Data_f <- df %>%
  select(Year, Month, Day, Hour, PM2, PM10, Visibility, WD, WS, OPC, Station.name, Date) %>% 
  mutate(PM10LPM2 = if_else(PM10>=PM2, TRUE, FALSE)) %>% 
  distinct()

Data_f$Station.name <- as.factor(Data_f$Station.name) 

Data_f %>%
  select(Station.name=="UB")
ggplot(aes(x=date, y=PM10)) +
  geom_point() 

ggplot(data = Data_f, aes(x=Date, y=Ratio, size=WS)) +
  geom_point() +
  facet_wrap(~Month)
### 1. Removal of Spikes
Data_1 <- Data_f %>%
  select(Year, Month, Day, Hour, PM2, PM10, Visibility, WD, WS, OPC, Station.name, Date) %>% 
  mutate(PM2 = if_else(PM2 >=10, NA, PM2), PM10 = if_else(PM10 >=10, NA, PM10))

### 2. PM2, PM10 compare and swap the values for the raio
Data_2 <- Data_1 %>%
  select(Year, Month, Day, Hour, PM2, PM10, Visibility, WD, WS, OPC, Station.name, Date) %>% 
  mutate(PM2_2 = if_else(PM2 < PM10, PM2, PM10), PM10_2 = if_else(PM10 > PM2, PM10, PM2), ratio = PM2/PM10, ratio_2 = PM2_2/PM10_2)

#### Make a logical check if certain period it has continued or not.
##
# I need some script. But I may make it in excel sheet
#
#
Data_2b <- Data_2 %>%
  select(Year, Month, Day, Hour, PM2, PM10, Visibility, WD, WS, OPC, Station.name, Date, PM2_2, PM10_2, ratio, ratio_2) %>% 
  mutate(check = if_else(ratio == ratio_2, 0, 1))

 



Data_2c <- Data_2b %>% 
  filter(check == 1) %>% 
  group_by(Station.name, Year, check, Date, WS) %>% 
  summarise(check_n = n()) 


# select(Station.name, Year, Month, Day, Date, check_n) %>% 
Data_2d <- Data_2c %>% 
  filter(Station.name == "Sainshand") 

ggplot(data = Data_2d, aes(x=Date, y=check_n)) +
  geom_point() +
  facet_wrap(~Year)

### 3. Plotting
Data_3 <- Data_2 %>%
  select(Year, Month, Day, Hour, PM2, PM10, Visibility, WD, WS, OPC, Station.name, Date, PM2_2, PM10_2, ratio, ratio_2) %>% 
  filter(Station.name=="Dalanzadgad" & Year==2018) %>%
  ggplot(aes(x=Date, y=PM2, color = WS)) +
  geom_point() +
  facet_wrap(~Year)
Data_3 





Data_2 <- Data_1 %>%
  select(Year, Month, Day, Hour, PM2, PM10, Visibility, WD, WS, OPC, Station.name, Date) %>% 
  mutate(PM2_2 = if_else(PM2 < PM10, PM2, PM10), PM10_2 = if_else(PM10 > PM2, PM10, PM2), ratio = PM2/PM10, ratio_2 = PM2_2/PM10_2) %>% 
  ggplot(aes(x=WS, y=PM10)) +
  geom_point() +
  facet_wrap(~Year)
Data_2 

  
  mutate(PM10_SpikesRem = if_else(PM2 >=30, NA, PM2), PM2_SpikesRem = if_else(PM10 >=10, NA, PM10))

  library(tidyverse)
   
Data_f1 <- Data_f %>%
  select(Year, Month, Day, Hour, PM2, PM10, Visibility, WD, WS, OPC, Station.name, Date, PM10_SpikesRem, PM2_SpikesRem)


### Он оноор сонголттой филтер хийх          
  Data_f %>% 
  filter(Year==2008 & ratio >1) %>% 
   ggplot(aes(x=Date, y=ratio, size=WS)) +
  geom_point() +
  facet_wrap(~Station.name)

glimpse(Data_f) 
summary(Data)
str(Data_f)
summary(Data_f)  
  
  
  
  
  
  
  





