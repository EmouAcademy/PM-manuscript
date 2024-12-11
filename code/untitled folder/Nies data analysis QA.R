library(tidyverse)
Data <- read.csv("~/Data Input/Preprocessing data/Preprocessing data.csv")
dim(Data)
view(Data)
head(Data)
tail(Data)
names(Data)
length(Data)
### Өгөгдлийнхөө их бага утга
summary(Data)
str(Data)
glimpse(Data)

is.na(Data)



attach(Data)        ####### Data$...
class(PM2)
length(PM2)
unique(PM2)
view(sort(table(PM2), decreasing = TRUE))
barplot(sort(table(PM2), decreasing = TRUE))

Data %>% 
  select(PM2) %>% 
  count(PM2) %>% 
  arrange(desc(n))

Data[is.na(PM10), ]       ###### row, column
view(Data[is.na(PM2), ])

sum(is.na(PM2))

class(PM2)
length(PM2)
summary(PM2)
boxplot(PM2)
hist(PM2)

#################### Clean data
class(Data$Station.name)
unique(Data$Station.name)
Data$Station.name <- as.factor(Data$Station.name)

class(Data$Station.name)
levels(Data$Station.name)

Data$Station.name <- factor(Data$Station.name, 
                          levels = c("UB", 
                                     "Sainshand",
                                     "Zamynuud",
                                     "Dalanzadgad"))
levels(Data$Station.name)

### select variables
names(Data)

Data %>% 
  select(Date, Station.name, PM2, PM10, Visibility, starts_with("W")) %>% 
  names()

unique(Data$PM2)

### filter observations
Data %>% 
  select(Date, Station.name, PM2, PM10, Visibility, starts_with("W")) %>% 
  filter(Station.name %in% c("UB", "Dalanzadgad") & 
           PM2 > 5000)

### missing data
mean(Data$PM2, na.rm = TRUE)

Data %>% 
  select(Date, Station.name, PM2, PM10, Visibility, starts_with("W"))

Data %>% 
  select(Date, Station.name, PM2, PM10, Visibility, starts_with("W")) %>% 
  na.omit()

Data %>% 
  select(Date, Station.name, PM2, PM10, Visibility, starts_with("W")) %>% 
  filter(!complete.cases(.))

Data %>% 
  select(Date, Station.name, PM2, PM10, Visibility, starts_with("W")) %>% 
  filter(!complete.cases(.)) %>% 
  drop_na(PM2)

Data %>% 
  select(Date, Station.name, PM2, PM10, Visibility, starts_with("W")) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(PM2 = replace_na(PM2, "none"))

### Duplicates
Data_dupl_removed <- Data %>% 
  distinct()
attach(Data_dupl_removed)
sum(is.na(PM2))

Data[!duplicated(Data), ]
view(Data[!duplicated(Data), ])
### Recoding variables
starwars %>% 
  select(Station.name, PM2, PM10)

Data %>% 
  select(Station.name, PM2, PM10) %>% 
  mutate(Station.name = recode(Station.name, 
                         "UB" = 1,
                         "Sainshand" = 2,
                         "Zamynuud" = 3,
                         "Dalanzadgad" = 4))


################### Manipulate data
### Rename a variable name
Data <- Data %>% 
  rename("Station" = "Station.name")

### Reorder variables
Data <- Data %>% 
  select(Date, Station, everything())

### Select variables to work with
Data %>% 
  select(1:4, Visibility, starts_with("W"), contains("PM")) %>% 
  names()

### Filter and arrange data
unique(Data$PM2)

Data %>% 
  filter((Station.name == "UB" | 
            Station.name == "Zamynuud") &
           PM10 > PM2) %>% 
  select(Date, Station.name, Visibility, PM2, PM10) %>% 
  arrange(-PM2) %>% 
  view()

Data %>% 
  filter(Station.name %in% c("1") &
           PM10 > PM2) %>% 
  select(Date, Station.name, Visibility, PM2, PM10) %>% 
  arrange(PM2) %>% 
  view()



### change observations (mutate)
msleep %>% 
  mutate(brainwt = brainwt * 1000) %>% 
  view()

msleep %>% 
  mutate(brainwt_in_gramms = brainwt * 1000) %>% 
  view()

# conditional changes (if_else)
size_of_brain <- msleep %>% 
  select(name, brainwt) %>% 
  drop_na(brainwt) %>% 
  mutate(brain_size = if_else(brainwt > 0.1, 
                              "Large", 
                              "Small"))

# recode name changes (mutate with recode)
shape <- size_of_brain %>% 
  mutate(brain_size = recode(brain_size, 
                             "Large" =1, 
                             "Small" =2))

### reshape horizontal/vertical (pivot_wider, pivot_longer)
wide_data <- shape %>% 
  pivot_wider(names_from = name, values_from = brainwt)

long_data <- wide_data %>% 
  pivot_longer(2:5, names_to = "name", values_to = "brainwt")


############################## Dealing with the Missing Data
md.pattern(starwars)

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.))

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  na.omit()

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  drop_na(height) %>% 
  filter(!complete.cases(.))

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  drop_na(height) %>% 
  mutate(gender = replace_na(gender, "none")) %>%  # NA-g none-r orluulah
  filter(!complete.cases(.))

unique(starwars$hair_color)

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  mutate(hair_color = na_if(hair_color, "none")) %>%  # NA-g none-r orluulah
  filter(is.na(hair_color))

### Describe the spread, centrality, and variance of your data

min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)
mean(msleep$awake)
median(msleep$awake)
var(msleep$awake)

### Summarize selected variables
summary(msleep)
summary(msleep$awake)

msleep %>% 
  select(sleep_total, brainwt) %>% 
  summary()

# Create a summary table
## For each category of "vore"
## show the min, max, difference
## and average "sleep_total"
## and arrange data by the average

msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(Lower = min(sleep_total),
            Upper = max(sleep_total),
            Difference = Upper - Lower,
            average = mean(sleep_total)) %>% 
  arrange(average)

# Creating contingency tables
library(MASS)
attach(Cars93)

glimpse(Cars93)
table(Origin)
table(Origin, AirBags)
table(AirBags, Origin)
addmargins(table(AirBags, Origin))
prop.table(table(AirBags, Origin), 1)*100
round(prop.table(table(AirBags, Origin), 2)*100)

Cars93 %>% 
  group_by(Origin, AirBags) %>% 
  summarise(number = n()) %>% 
  pivot_wider(names_from = Origin, 
              values_from = number)



Table_NA_by_Year <- Data_dupl_removed %>%
  rowwise %>%
  group_by(Station.name,Year) %>% 
  summarise(NA_date = sum(!is.na(Date)),
            NA_PM2 = sum(is.na(PM2)),
            NA_PM10 = sum(is.na(PM10)),
            NA_Vis = sum(is.na(Visibility)),
            NA_WD = sum(is.na(WD)),
            NA_WS = sum(is.na(WS)),
            NA_OPC = sum(is.na(OPC))
            ) 
write_csv(Table_NA_by_Year, path = "Table_NA_by_Year.csv")

Table_NA_by_Station <- Data_dupl_removed %>%
  rowwise %>%
  group_by(Station.name) %>% 
  summarise(NA_date = sum(!is.na(Date)),
            NA_PM2 = sum(is.na(PM2)),
            NA_PM10 = sum(is.na(PM10)),
            NA_Vis = sum(is.na(Visibility)),
            NA_WD = sum(is.na(WD)),
            NA_WS = sum(is.na(WS)),
            NA_OPC = sum(is.na(OPC))
  ) 
  
Table_NA_by_Station_percentage <- Data_dupl_removed %>%
  rowwise %>%
  group_by(Station.name, Year) %>% 
  summarise(NA_date = sum(!is.na(Date)),
            NA_PM2 = sum(is.na(PM2)),
            NA_PM10 = sum(is.na(PM10)),
            NA_Vis = sum(is.na(Visibility)),
            NA_WD = sum(is.na(WD)),
            NA_WS = sum(is.na(WS)),
            NA_OPC = sum(is.na(OPC))
  ) %>% 
  mutate(
            NA_PM2p = if_else(NA_date != NA_PM2, NA_PM2/NA_date*100, 0)
  ) %>% 
  group_by(Station.name) %>% 
  summarise(NA_date = sum(!is.na(Date)),
            NA_PM2psum = sum(is.na(PM2)))
            