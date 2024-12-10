library(tidyverse)
library(ggplot2)
library(VIM)
library(readr)
library(ggbreak)      # scale_y_break
source(here("code/Colors.R"))

#### DAILY data
daily_data <- data |>
  group_by(Station.name, Date) |>
  summarise(across(where(is.numeric), ~ mean(.x,  by=c(Station.name, Date),na.rm = F)))


##### Monthly data
df_cleaned_monthly_PMs <- df_clean |>
  group_by(Station.name, Year, Month) |>
  summarise(across(where(is.numeric), ~ mean(.x, by=c(Station.name, Year, Month), na.rm = TRUE)))



##### Hourly data
df_hour_mean <- data |>
  group_by(Station.name, Hour) |>
  summarise(across(where(is.numeric), ~ mean(.x, by=Hour, na.rm = TRUE)))



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

long_daily_data |>
  ggplot(aes(Month, pm, fill= PMs, group= Month, shape = Station.name)) +
  geom_boxplot() +
  facet_wrap(PMs~Station.name, scales =  'free_y', ncol=1) +
  labs(x = "Date", y = "PM10")

######## Add DATETIME
data <- df02out %>% 
  mutate(Datetime = make_datetime(Year, Month, Day, Hour))
df02in <- df02in %>% 
  mutate(Datetime = make_datetime(Year, Month, Day, Hour))
# glimpse(df1)





######################## input DATA 



data <- read.csv("data/processed/df.csv")

## add order and levels
data$Station.name <- factor(data$Station.name, 
                            levels = c("UB", "DZ",  "ZU", "SS"))  
levels(data$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')
#### DAILY data
daily_data <- data |>
  group_by(Station.name, Date) |>
  summarise(across(where(is.numeric), ~ mean(.x,  by=c(Station.name, Date),na.rm = F)))




########################### Plotting










########################### Output


ggsave("visuals/fig3_1_annual_variations.png", p3_daily_01, width=8, height=5, unit="in", dpi = 300)
