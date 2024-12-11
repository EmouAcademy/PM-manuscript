library(readr)
library(fpp3)
tute1 <- read_csv("~/Downloads/tute1.csv")
tute1 <- read_csv("~/Downloads/tute1 (1).csv")
view(tute1)
mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)
autoplot(mytimeseries, facets=TRUE)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))


mytimeseries <- tute1 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter)

mytimeseriesM <- tute1 |>
  mutate(Month = yearmonth(Quarter)) |>
  as_tsibble(index = Month)

mytimeseriesD <- tute1 |>
  mutate(Month = yearmonth(Quarter),
         Quarter = yearquarter(Quarter),
         Day = day(Quarter),
         Hour = hour(Quarter)) |>
  as_tsibble(index = Month)

mytimeseriesD |>
#  pivot_longer(-Quarter) |>
  ggplot(aes(x = Month, y = GDP, colour = Quarter)) +
  geom_line() 

mytimeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")



y <- tsibble(
  Year = 2008:2020,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

df_01 <- data |>
         select(Station.name, Date, Visibility, WD, WS, OPC, pm10, pm2.5)
library(clock)
library(lubridate)
df_01d <- data 

# df_01d$Date <- as.Date(with(df_01d, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")
#df_01d$Date <- with(df_01d, ymd_hm(sprintf('%04d%02d%02d hm', Year, Month, Day, Hour)))

df_01d$Dates <- ymd_h(paste(df_01$Date, df_01$Hour), tz = "UTC")

df_fig3_02 <- df_01d %>%
  group_by(Station.name) %>% 
  distinct(Dates, .keep_all = TRUE)

dup <- duplicates(dup2, index = Dates, key = Station.name)
dup2 <- distinct(df_01d, index = Dates, key = Station.name)
dup1 <- !duplicated(df_01d$Dates, index=Dates, key = Station.name)
df_01d <- df_01d[!duplicated(df_01d$Dates, index=Dates, key = Station.name)]

df_01Q <- df_fig3_02 |>
  mutate(date = as_datetime(Dates),
         Quarter = yearquarter(Dates),
         Month = yearmonth(Dates),
         Week = yearweek(Dates), #,#
         Hour = as_datetime(Dates)) |> 
 select(-Dates) |>
  as_tsibble(index = date, key= Station.name)

df_01Qdd <- df_fig3_02 |>
  filter(Station.name %in%  c('UB', 'Dalanzadgad')) 
  
df_01Qd <- df_01Qdd |>
  mutate(date = as_datetime(Dates),
         Quarter = yearquarter(Dates),
         Month = yearmonth(Dates),
         Week = yearweek(Dates), #,#
         Hour = as_datetime(Dates)) |> 
  select(-Dates) |>
  as_tsibble(index = date, key= Station.name)

daily_UB_DZa <- df_01Qd |>
  fill_gaps()  |>
  group_by(Quarter) |>
  gg_season(pm2.5*1000, period = "day") +
 geom_line() +
  labs(y = "PM2.5 (ug m3)", x = "Time (UTC)", 
       title = "Daily variations: PM2.5 concentrations")
daily_UB_DZb <- df_01Qd |>
  fill_gaps()  |>
  group_by(Quarter) |>
  gg_season(pm10*1000, period = "day") +
  geom_line() +
  labs(y = "PM10 (ug m3)", x = "Time (UTC)", 
       title = "Daily variations: PM10 concentrations")

library(plotly)

data %>% 
  filter(Station.name == 'UB') %>% 
  plot_ly(x = ~Hour, y = ~ pm10, 
          type = "box", 
          color = ~Month, 
          colors = "Dark2",
          boxpoints = "all", 
          jitter = 0.3,
          pointpos = -1.8)


############# Heat map
library(timetk)
library(dplyr)
daily_data %>%
  group_by(Station.name) %>%
  summarise_by_time(
    .date_var  = Date,
    .by        = "month",
    pm10   = mean(pm10),
    Year=mean(Year),
    Month = mean(Month), na.rm=T) ->a10

us_gas_df <- a10 %>%
  filter(Station.name == 'UB') %>% 
  select(Year, Month, pm10) %>%
  tidyr::pivot_wider(names_from = Year,
                     values_from = pm10)


plot_ly(x = names(us_gas_df)[-1],
        y = us_gas_df$Month,
        z = as.matrix(us_gas_df[, -1]),
        colors = "Reds",
        xgap = 3,
        ygap = 3,
        type = "heatmap")

#################################

colSums(is.na(df_01Q))

df_01Q |>
  ggplot(aes(x = Week, y = pm10, colour = Station.name)) +
  geom_point() +
  geom_boxplot() +
  facet_grid(Station.name ~ ., scales = "free_y")

