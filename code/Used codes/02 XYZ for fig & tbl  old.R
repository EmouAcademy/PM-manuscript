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
