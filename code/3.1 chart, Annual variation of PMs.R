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
p3_annual_02 <- data |>
  ggplot(aes(Month, pm2.5*1000)) +
  geom_bar(stat = 'summary', position=position_dodge(width=0.9), alpha=0.5)+
  geom_boxplot(aes(Month, pm2.5*1000, group = Month), width=0.3, outlier.size=0) +
#  geom_violin(aes(Month, pm10*1000, group = Month, fill=pm2.5*1000),  position= position_nudge(x=.2), width=0.35, size=0.3) +
  theme_bw()+
  #  theme(axis.text.x=element_text(angle = 90)) +
  #  geom_jitter(aes(shape=Station.name, color=Station.name), width = 0, height = 0)+
  # coord_flip() +
  facet_wrap(~Station.name,  scales = 'free', ncol=1) +
  #coord_cartesian(ylim = c(0, 1000)) +
  theme(strip.text.x = element_blank(), axis.title.y = element_blank(), 
        legend.position = c(.83, 0.18),
        #legend.position="none",
        legend.key.size = unit(.3, 'cm'), 
        legend.key.height = unit(.2, 'cm'), 
        legend.direction="horizontal",
        legend.title = element_text(size=7), #change legend title font size
        legend.title.align = 1,
        legend.text = element_text(size=7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  #  scale_x_discrete(limits= c(0:23)) +
  scale_y_continuous(breaks = c(50,100, 200)) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800)) +
  scale_y_break(c(120,700), scale= 0.18, space = 0.4, ticklabels = c(seq(50,800,350), 700),
                expand = c(0, 0))













########################### Output


ggsave("visuals/fig3_1_annual_variations.png", p3_daily_01, width=8, height=5, unit="in", dpi = 300)
