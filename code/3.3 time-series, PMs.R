library(ggplot2)
library(readr)
library(tidyverse)      # scale_y_break
source(here("code/Colors.R"))

####### input DATA 
data <- read.csv("data/processed/df.csv")
df <- read.csv("data/processed/df_filled.csv")
## add order and levels
data$Station.name <- factor(data$Station.name, 
                            levels = c("UB", "DZ",  "ZU", "SS"))  
levels(data$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')

df <- read.csv("data/processed/df_filled.csv")
df$Station.name <- factor(df$Station.name, 
                            levels = c("UB", "DZ",  "ZU", "SS"))  
levels(df$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')

data$Date <- as.Date(data$Date) 
df$Date <- as.Date(df$Date) 
########################### Plotting

##################### annual changes of PMs, with trendline, correlation
## pm10
annual_p10 <- df_trend_1028 |>
  ggplot(aes(Date, `pm10.f`*1000)) +
  geom_line(alpha=0.4) +
  geom_point(aes(y=pm10*1000), shape =1, size=0.1) +
  geom_smooth(mapping = aes(colour = Seasons), method = "lm", size = 0.6, se = F, formula = y ~ x, linetype = 'longdash') +
  stat_cor(aes(colour = Seasons, label=..label..), label.y.npc=0.97, label.x.npc = 0.7, 
           p.accuracy = 0.001, r.accuracy = 0.01, inherit.aes = TRUE, size=2.3) +
  theme_classic() +
  scale_x_date(date_breaks = "12 month", 
               labels=date_format("%Y"),
               limits = as.Date(c('2008-03-01','2020-03-01')), expand =c(0.01,0.01)) +
  facet_wrap(.~Station.name, ncol=1, strip.position="left", scales = "free_y") +
  scale_y_continuous(limits = c(0, 610), expand = c(0,0)) +
  scale_color_manual(values=c("red3", "blue", "green3")) +
  # scale_y_continuous(breaks = seq(0, 6000, by = 600), limits=c(0, 6000)) +
  # scale_y_continuous(breaks = seq(0,6000,100), limits = c(0,6000)) +
  #  scale_y_break(c(600,5900), space = 0.4, ticklabels = c(seq(0,6000,100), 6000),
  #               expand = c(0, 0)) +
  labs(y = NULL, subtitle = "PM10")+
  geom_hline(yintercept=0) +
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        panel.spacing = unit(14, units = "pt"),
        legend.position = "none") 
annual_p10
annual_p10 + coord_cartesian(ylim = c(0, 180))
library(ggbreak)
max_val <- c(200, 600, 600, 200)
## pm2.5
library(scales)
library(ggpubr)
df_trend_1028 <- df |>
  mutate(Seasons = case_when(
    Month > 10 | Month < 3 ~ "Q1",
    Month > 2 & Month < 6 ~ "Q2",
    Month > 5 & Month < 11 ~ "Q3/4")) 

annual_p2.5 <- df_trend_1028 |>
  ggplot(aes(Date, `pm2.5.f`*1000), na.rm=T) +
  geom_line(alpha=0.4) +
  geom_point(aes(y=pm2.5*1000), shape =1, size=0.1) +
  #geom_smooth(method = "lm", size = 0.6, se = F, formula = y ~ x, linetype = 'longdash') +
  geom_smooth(mapping = aes(colour = Seasons), method = "lm", size = 0.6, se = F, formula = y ~ x, linetype = 'longdash') +
  stat_cor(aes(colour = Seasons, label=paste(..r.label.., ..p.label.., sep = "~`,`~")), show.legend = F, label.y.npc=0.97, label.x.npc = 0.7, 
           p.accuracy = 0.001, r.accuracy = 0.01, inherit.aes = TRUE, size=2.5) +
  theme_classic() +
  scale_x_date(date_breaks = "12 month", 
               labels=date_format("%Y"),
               limits = as.Date(c('2008-03-01','2020-03-01')), expand =c(0.01,0.01)) +
  facet_wrap(.~Station.name, ncol=1, strip.position="left", scales = "free_y") +
  scale_y_continuous(limits = c(0, 610), expand = c(0,0)) +
 # geom_hline(yintercept=0) +
  #  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values=c("red3", "blue", "green3")) +
 # scale_y_continuous(expand = c(0,0)) +
 # scale_x_date(date_breaks = "12 month", 
            #   labels=date_format("%Y"),
            #   limits = as.Date(c('2008-03-01','2020-03-01')), expand =c(0.01,0.01)) +
  facet_wrap(.~Station.name, ncol=1, strip.position="left", scales = "free_y") +
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        panel.spacing = unit(14, units = "pt"),
        legend.position = "inside",
        legend.position.inside = c(.64, 0.96),
        legend.key.spacing.y = unit(3.7, units = "pt"),
        legend.background = element_blank(),
        legend.text = element_text(size = 6.3),
        legend.key.height = unit(4, units = "pt"),
        legend.key.width = unit(6, units = "pt"),
        # legend.key.size = unit(0, units = "pt"),
        legend.title = element_blank()) +
  labs(y=NULL, subtitle = "PM2.5") 
annual_p2.5 + coord_cartesian(ylim = c(0, 180))
annual_p2.5

library(patchwork)
plot_20241103 <- annual_p10 + annual_p2.5
ggsave("visuals/fig3_3_trends.png", plot_20241103, width = 9, height = 5.5, units = "in", dpi = 400)

############################ seasonal changes in PMs: winter and spring
df_clean <- df |>
  mutate(Seasons = case_when(
    Month > 10 | Month < 3 ~ "Q1,",
    Month > 2 & Month < 6 ~ "Q2,",
    Month > 5 & Month < 11 ~ "Q3/4,")) 

annual_r <- df_clean |>
  ggplot(aes(Date, pm2.5 ), na.rm=T) +
  geom_line(alpha=0.4) +
  geom_point(aes(y=pm2.5/pm10), shape =1, size=0.1) +
  #geom_smooth(method = "lm", size = 0.6, se = F, formula = y ~ x, linetype = 'longdash') +
  geom_smooth(mapping = aes(colour = Seasons), method = "lm", size = 0.6, se = F, formula = y ~ x, linetype = 'longdash') +
  stat_cor(aes(colour = Seasons, label=paste(..r.label.., ..p.label.., sep = "~`,`~")), show.legend = F, label.y.npc=0.97, label.x.npc = 0.7, 
           p.accuracy = 0.001, r.accuracy = 0.01, inherit.aes = TRUE, size=2.5) +
  theme_classic() +
  geom_hline(yintercept=0) +
  #  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values=c("red3", "blue", "green3")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = "12 month", 
               labels=date_format("%Y"),
               limits = as.Date(c('2008-03-01','2020-03-01')), expand =c(0.01,0.01)) +
  facet_wrap(.~Station.name, ncol=1, strip.position="left", scales = "free_y") +
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        panel.spacing = unit(14, units = "pt"),
        legend.position = "inside",
        legend.position.inside = c(.64, 0.96),
        legend.key.spacing.y = unit(3.7, units = "pt"),
        legend.background = element_blank(),
        legend.text = element_text(size = 6.3),
        legend.key.height = unit(4, units = "pt"),
        legend.key.width = unit(6, units = "pt"),
        # legend.key.size = unit(0, units = "pt"),
        legend.title = element_blank()) +
  labs(y=NULL, subtitle = "PM2.5") 


############################

library(hydroTSM)
Meanpm2.5<- df_trend_1028 %>%
  # filter(Station.name == "UB") %>%
  select(Station.name , Year, Date, pm2.5, `filled pm2.5`, 'filled pm10') %>%
  # group_by(Station.name) %>%
  group_by(Station.name, Year) %>%
  summarize(mean_pm2.5 = mean(`filled pm2.5`), 
            mean_pm10 = mean(`filled pm10`))
Meanpm2.5 |>
  group_by(Station.name) |>
  ggplot(aes( mean_pm10*1000)) +
  #  geom_point(shape=1, position = "jitter") +
  #  geom_point(aes(Date, `filled pm2.5`),  alpha=0.4) + 
  geom_histogram() +
  geom_line(aes(Year, mean_pm2.5*1000),  alpha=0.4, col = "red") + 
  theme_bw() +
  facet_wrap(~Station.name, ncol = 1, scales = 'free', strip.position="right") 
#  ylim(0,0.75)


df_trend_1028 |>
  group_by(Station.name) |>
  ggplot(aes(Date)) +
  #  geom_point(shape=1, position = "jitter") +
  #  geom_point(aes(Date, `filled pm2.5`),  alpha=0.4) + 
  geom_point(aes(y = rollmean(
    `filled pm2.5` , 365, fill = NA, align = "left"), colour="blue")) +
  geom_point(aes(y = rollmean(
    `filled pm10` , 365, fill = NA, align = "left"))) +
  # geom_smooth() +
  theme_bw() +
  facet_wrap(~Station.name, ncol = 1) 
#  ylim(0,0.75)





df_trend_1028 |>
  group_by(Station.name) |>
  ggplot(aes(Date, `filled pm10`)) +
  geom_point(shape=1, position = "jitter") +
  #  geom_line(aes(Date, `seasonal pm10`),  alpha=0.8, colour="blue") + 
  geom_line(aes(y = rollmean(
    df_trend_1028$`filled pm10` , 11, fill = NA, align = "left"), colour="blue"), linewidth = 0.5) +
  geom_point(aes(Date, `trend pm10`),  alpha=0.8, colour="red") + 
  geom_smooth() +
  theme_bw() +
  facet_wrap(~Station.name) +
  ylim(0,0.75)

df_trend_1028 |>
  group_by(Station.name) |>
  ggplot(aes(Date, `filled pm2.5`)) +
  geom_point(shape=1, position = "jitter") +
  #  geom_line(aes(Date, `seasonal pm10`),  alpha=0.8, colour="blue") + 
  geom_line(aes(y = rollmean(
    df_trend_1028$`filled pm2.5` , 11, fill = NA, align = "left"), colour="blue"), linewidth = 0.5) +
  geom_smooth(aes(Date, `trend pm2.5`),  alpha=0.8, colour="red") + 
  #  geom_smooth() +
  theme_bw() +
  facet_wrap(~Station.name) +
  ylim(0,0.75)









########################### Output


ggsave("visuals/fig3_3_trend.png", p3_daily_01, width=8, height=5, unit="in", dpi = 300)