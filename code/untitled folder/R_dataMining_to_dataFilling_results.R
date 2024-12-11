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
library(VIM)
library(readr)

#set current directory
setwd("/Users/munkhtsetseg/WORK/Research/my-awesome-manuscripts/Data")

#read in CSV file


df <- read.csv("Preprocessing data.csv")

#Data$Station.name <- as.factor(Data$Station.name)
#view(Data)

## And remove duplicates
df1 <- df %>%
  distinct()


##### Converting types, renaming.
df1 <-  df1 |>
  mutate(pm2_swap=PM10, pm10_swap=PM2, ratio = pm2_swap/pm10_swap)







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



###  1. UB
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep

data <- df_spike7_0 |>
  mutate(id = "Gobi") 

data <- data |>
  mutate(id = replace(id, Station.name == "UB", "Urban") )



# Renaming factor levels dplyr
data$Station.name <- recode_factor(data$Station.name, 
                                   Dalanzadgad  = "DZ", 
                                   Sainshand = "SS", 
                                   UB = "UB",
                                   Zamynuud = "ZU")

data |>
  ggplot(aes(pm10, pm2.5, color=Station.name)) +
  geom_point() 

df_UB <- data |>
  filter(Station.name == "UB")
df_DZ <- data |>
  filter(Station.name == "DZ")
df_ZU <- data |>
  filter(Station.name == "ZU")
df_SS <- data |>
  filter(Station.name == "SS")
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

data |>
  filter(Station.name == "Dalanzadgad") |>
  ggplot(aes(pm10, pm2.5, size = WS, colour = Month)) +
  geom_point()
####------- Test
data |>
  ggplot(aes(Date, pm2.5, colour = Station.name)) +
  geom_point(position = "jitter") +
  geom_jitter
  facet_wrap(~Station.name) +
  scale_x_date(limits = c(data$Date[1], data$Date[345000])) +
scale_y_continuous(limits = c(0, 1)) 

data |>
#  filter(pm10<0.25) |>
  ggplot(aes(Station.name, pm2.5, colour = Station.name)) +
  geom_boxplot() +
#  facet_wrap(~Station.name) +
#  scale_x_continuous(limits = c(1, 7)) +
  scale_y_continuous(limits = c(0, 0.1)) +
labs(x = "Sites", y = "PM10",
     title = "Locations of study sites",
     subtitle ='Averaged wind speed in the spring (March to May)',
     caption = 'Data source: WorldClim, 2022') +
  cowplot::theme_cowplot() +
  theme(legend.position = c(.73, .70),
        legend.title = element_text(size=9), #change legend title font size
        legend.text = element_text(size=7),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linetype = "dashed",
                                        size = .05),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE,
        panel.background = element_rect(fill = NA, colour = 'black', size = 1.5))




##########    DATA PREPARE#######################################################
##########
##   who %>% pivot_longer(
##cols = new_sp_m014:newrel_f65,
##names_to = c("diagnosis", "gender", "age"),
##names_pattern = "new_?(.*)_(.)(.*)",
##values_to = "count"
## )
## 
##
long_data <- data %>% 
  pivot_longer(
  cols = c("pm10","pm2.5"),
  names_to = "PMs",
  names_prefix = "",
  values_to = "pm",
  values_drop_na = F
)

long_daily_data <- daily_data %>% 
  pivot_longer(
    cols = c("pm10","pm2.5"),
    names_to = "PMs",
    names_prefix = "",
    values_to = "pm",
    values_drop_na = F
  )



################################################################################
#####.  Annual: Mean daily averages of PM10, PM2.5 for p1_01, p2_01, p3, p4 plots

data$Station.name <- factor(data$Station.name, 
       levels = c("UB", "DZ",  "ZU", "SS"))  

daily_data <- data |>
  group_by(Station.name, Date) |>
  summarise(across(where(is.numeric), ~ mean(.x, by=Date, na.rm = TRUE)))
long_daily_data |>
  ggplot(aes(Month, pm, fill= PMs, group= Month, shape = Station.name)) +
  geom_boxplot() +
  facet_wrap(PMs~Station.name, scales =  'free_y', ncol=1) +
  labs(x = "Date", y = "PM10")
df_month_mean <- data |>
  group_by(Station.name, Month) |>
  summarise(across(where(is.numeric), ~ mean(.x, by=Month, na.rm = TRUE)))
df_hour_mean <- data |>
  group_by(Station.name, Hour) |>
  summarise(across(where(is.numeric), ~ mean(.x, by=Hour, na.rm = TRUE)))


glimpse(df1)
data$Date <- as.Date(data$Date)
data$Station.name <- as.factor(data$Station.name) 

daily_data <- data %>% group_by(Station.name, Date) %>% summarize(across(where(is.numeric), mean))
daily_data$Station.name <- factor(daily_data$Station.name, 
                                  levels = c("UB", "DZ",  "ZU", "SS"))  

p1 <- daily_data |>
  ggplot(aes(Date, pm10*1000, colour = Station.name, shape = Station.name)) +
  geom_point() +
  facet_wrap(~Station.name, scales =  'free_y', ncol=1) +
  labs(x = "Date", y = "PM10")
p1

p1_01 <- daily_data |>
  ggplot(aes(Date, pm10*1000, colour = Station.name, shape = Station.name)) +
  geom_point() +
#  facet_wrap(~Station.name, scales =  'free_y', ncol=1) +
  labs(y = "PM10")
p1_01

p2 <- daily_data |>
  ggplot(aes(Date, pm2.5*1000, colour = Station.name, shape = Station.name)) +
  geom_point() +
  facet_wrap(~Station.name, scales = 'free_y', ncol=1) +
  labs(x = "Date", y = "PM2.5")
p2
p2_01 <- daily_data |>
  ggplot(aes(Date, pm2.5*1000, colour = Station.name, shape = Station.name)) +
  geom_point() +
#  facet_wrap(~Station.name, scales = 'free_y', ncol=1) +
  labs(x = "Date", y = "PM2.5")
p2_01

p3 <- daily_data |>
  ggplot(aes(Station.name, pm10*1000)) +
  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.3)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  labs(x = "Sites", y = "PM10")
p3



p3_01 <- daily_data |>
  ggplot(aes(Station.name, pm10*1000, color=Station.name)) +
#  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.3)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  labs(x = "Sites", y = "PM10")
p3_01

p4 <- daily_data |>
  ggplot(aes(Station.name, pm2.5*1000)) +
  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.3)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  labs(x = "Sites", y = "PM2.5")
p4
p4_01 <- daily_data |>
  ggplot(aes(Station.name, pm2.5*1000, color=Station.name)) +
#  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.3)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  labs(x = "Sites", y = "PM2.5")
p4_01

##########
##########. Figure 3-2
###Seasonal:
library(timeSeries)

x <- MSFT
by <- timeSequence(from = start(data$), to = end(data), by = "week") 
X <- aggregate(x, by, mean)
X 
dayOfWeek(time(X)) 
isMonthly(X)

df_UB <- daily_data |>
  filter(Station.name == "UB")
df_DZ <- daily_data |>
  filter(Station.name == "DZ")
df_ZU <- daily_data |>
  filter(Station.name == "ZU")
df_SS <- daily_data |>
  filter(Station.name == "SS")

###WINTER

library(forecast)
y <- ts(df_UB$pm10, start=2008, frequency=365)
ggseasonplot(y, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("PM10") +
  ggtitle("Seasonal plot: UB")

ggseasonplot(y, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

ggsubseriesplot(y) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

p5 <- daily_data |>
  gg_season(pm10, labels = "both")
df_UB |> autoplot(pm10)
  

p2w <- daily_data |>
  filter(Month>11 | Month < 3) |>
  ggplot(aes(pm10*1000, Station.name )) +
  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.3)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  # scale_y_continuous(limits = c(0, 300)) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_shape_manual(values=c(8, 1, 0, 2))+
  scale_color_manual(values=c('#937f52','#b88d2d', '#c4b485','#dc9b09'))+
  scale_size_manual(values=c(1,1,1,1)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_y_break(c(130, 1000)) +
  scale_y_break(c(1100, 6600))

### SPRING
pms <- c(data$pm10,data$pm2.5)
p2s <- data |>
  filter(Month>2 & Month < 6) |>
#  summarise() |>
  ggplot(aes(Station.name, pm10)) +
 # geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.3)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  # scale_y_continuous(limits = c(0, 300)) +
#  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
#  scale_shape_manual(values=c(1, 0, 2,8))+
  #  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9','red'))+
#  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9','pink'))+
 # scale_size_manual(values=c(1,1,1,1)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_y_break(c(130, 1000)) +
  scale_y_break(c(1100, 6600))


grid.arrange(p2w, p2s, ncol=1)




          geom_point(aes(colour=Station.name), color = "black", size = c(2.5,2.5,2.5, 2.5), pch = c(1,24,22,8)) +
          geom_point(data = data, aes(x = Station.name, y = pm2.5), color = "white", size = 1.5, pch = c(1,24,22, 8)) +
  




###################### 1. Scale 

p1_01 <- p1_01 + scale_x_date(limits=c(as.Date("2008-01-01"), as.Date("2020-08-31")), date_labels = "%Y", date_breaks = "12 month")  #"%d/%m/%Y", "%b %Y"
p1_01

p3_01 <- p3_01 + scale_y_continuous(limits = c(0,190))
p4_01 <- p4_01 + scale_y_continuous(limits = c(0,190))
         scale_y_continuous(
        , sec.axis = sec_axis( trans=~.*10, name="Second Axis")) 

         scale_y_break(c(130, 1000)) +
           scale_y_break(c(1100, 6600))
         (scale_x_date(breaks=date_breaks("6 months"),
                       labels=date_format("%b %y")))
         p <- p + scale_fill_manual(values = getPalette(colourCount))
         p2_01 <- p2_01 + scale_x_date(limits = c(data$Date[1], data$Date[345000]))

library(hrbrthemes)
library(viridis)
library(ggbreak) # scale_y_break(c(130, 1000)), scale_x_break(c(130, 1000))
# scale_y_continuous(limits = c(0, 300)) +
p3 <- p3 + scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_shape_manual(values=c(8, 1, 0, 2))+
  scale_color_manual(values=c('#937f52','#b88d2d', '#c4b485','#dc9b09'))+
           #  scale_color_manual(values=c('#fee9b9','#c4b485', '#eca300','#999999'))+
          scale_size_manual(values=c(2,2,2,2)) 

###################### 2. Labels and Titles

p <- p + labs(x = "Sites", y = "PM10",
          title = "Locations of study sites",
          subtitle ='Averaged wind speed in the spring (March to May)',
          caption = 'Data source: WorldClim, 2022')


######################  3. Theme background
p1_01 <- p1_01 + theme_minimal() +
  theme(legend.position="none",
        legend.title = element_text(size=9), #change legend title font size
        legend.text = element_text(size=7),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linetype = "dashed",
                                        size = .05),
        panel.grid.minor = element_blank(),
        panel.ontop = F,
        panel.background = element_rect(fill = NA, colour = 'black', size = 0.5))

p4 <- p4 + theme_ipsum() +
         theme(
               legend.position="none",
               plot.title = element_text(size=11)
                )  

p1_01 <- p1_01 + cowplot::theme_cowplot() +
  theme(#legend.position = c(.73, .70),
         legend.position="none",
        legend.title = element_text(size=9), #change legend title font size
        legend.text = element_text(size=7),
        axis.title.y = element_text(size=12),
      #  axis.title.x = element_text(size=12),
        axis.text = element_text(size = 8),
       axis.text.x = element_blank(),
     #   axis.text.y = element_blank(),
       axis.ticks.x=element_blank(),
       # axis.ticks.y=element_blank(),  #remove y axis ticks
        axis.title.x = element_blank(),
     #  axis.title.y = element_blank(),
       # panel.grid.major = element_line(color = "grey", 
                                    #    linetype = "dashed",
                                   #     size = .05),
        panel.grid.minor = element_blank(),
        panel.ontop = F,
        panel.background = element_rect(fill = NA, colour = 'black', size = 0.7))


theme(axis.text.x=element_blank(), #remove x axis labels
      axis.ticks.x=element_blank(), #remove x axis ticks
      axis.text.y=element_blank(),  #remove y axis labels
      axis.ticks.y=element_blank(),  #remove y axis ticks
      axis.title.x = element_blank(),
      axis.title.y = element_blank()),
)




###################### Layout

grid.arrange(p1_01, p3, p2_01, p4, ncol = 2, widths = c(3,1))

grid.arrange(p1_01, p3_01, p2_01, p4_01, ncol = 2, widths = c(3,1))
p <- grid.arrange(p3, p4, ncol = 1)
grid.arrange(p1, p2, p, ncol = 2, widths = c(3,1.5))




library(gridExtra)
grid.arrange(graphic3_1a, graphic3_1b, widths = c(3,2))

grid.arrange(p1, p3, nrow = 1)
grid.arrange(p1, p3, widths = c(3,2))














##################################
library(hrbrthemes)
library(viridis)
library(ggbreak) # scale_y_break(c(130, 1000)), scale_x_break(c(130, 1000))
graphic3_1b <- daily_data |>
  ggplot(aes(Station.name, pm10*1000)) +
  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.3)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  # scale_y_continuous(limits = c(0, 300)) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_shape_manual(values=c(1, 0, 2,8))+
  scale_color_manual(values=c('#dc9b09','#b88d2d', '#c4b485','#937f52'))+
  #  scale_color_manual(values=c('#fee9b9','#c4b485', '#eca300','#999999'))+
  scale_size_manual(values=c(2,2,2,2)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )  
