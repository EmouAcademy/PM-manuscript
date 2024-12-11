par(mar=c(1,1,1,1))
par(mfrow = c(4,3))

df_daily_UB <- df_daily[df_daily$Station.name %in% c("UB"),] 
df_daily_DZ <- df_daily[df_daily$Station.name %in% c("DZ"),] 
df_daily_SS <- df_daily[df_daily$Station.name %in% c("SS"),] 
df_daily_ZU <- df_daily[df_daily$Station.name %in% c("ZU"),] 

library(TSstudio)    
UB_pm10_x <-  ts_reshape(annual_TimeSeries_UB_pm10$x, type = "long", frequency = NULL)
UB_pm10_seasonal <-  ts_reshape(annual_TimeSeries_UB_pm10$seasonal , type = "long", frequency = NULL)
UB_pm10_trend <-  ts_reshape(annual_TimeSeries_UB_pm10$trend , type = "long", frequency = NULL)
UB_pm10_random <-  ts_reshape(annual_TimeSeries_UB_pm10$random , type = "long", frequency = NULL)
UB_pm2.5_x <-  ts_reshape(annual_TimeSeries_UB_pm2.5$x, type = "long", frequency = NULL)
UB_pm2.5_seasonal <-  ts_reshape(annual_TimeSeries_UB_pm2.5$seasonal, type = "long", frequency = NULL)
UB_pm2.5_trend <-  ts_reshape(annual_TimeSeries_UB_pm2.5$trend, type = "long", frequency = NULL)
UB_pm2.5_random <-  ts_reshape(annual_TimeSeries_UB_pm2.5$random, type = "long", frequency = NULL)

DZ_pm10_x <-  ts_reshape(annual_TimeSeries_DZ_pm10$x, type = "long", frequency = NULL)
DZ_pm10_seasonal <-  ts_reshape(annual_TimeSeries_DZ_pm10$seasonal , type = "long", frequency = NULL)
DZ_pm10_trend <-  ts_reshape(annual_TimeSeries_DZ_pm10$trend , type = "long", frequency = NULL)
DZ_pm10_random <-  ts_reshape(annual_TimeSeries_DZ_pm10$random , type = "long", frequency = NULL)
DZ_pm2.5_x <-  ts_reshape(annual_TimeSeries_DZ_pm2.5$x, type = "long", frequency = NULL)
DZ_pm2.5_seasonal <-  ts_reshape(annual_TimeSeries_DZ_pm2.5$seasonal, type = "long", frequency = NULL)
DZ_pm2.5_trend <-  ts_reshape(annual_TimeSeries_DZ_pm2.5$trend, type = "long", frequency = NULL)
DZ_pm2.5_random <-  ts_reshape(annual_TimeSeries_DZ_pm2.5$random, type = "long", frequency = NULL)

SS_pm10_x <-  ts_reshape(annual_TimeSeries_SS_pm10$x, type = "long", frequency = NULL)
SS_pm10_seasonal <-  ts_reshape(annual_TimeSeries_SS_pm10$seasonal , type = "long", frequency = NULL)
SS_pm10_trend <-  ts_reshape(annual_TimeSeries_SS_pm10$trend , type = "long", frequency = NULL)
SS_pm10_random <-  ts_reshape(annual_TimeSeries_SS_pm10$random , type = "long", frequency = NULL)
SS_pm2.5_x <-  ts_reshape(annual_TimeSeries_SS_pm2.5$x, type = "long", frequency = NULL)
SS_pm2.5_seasonal <-  ts_reshape(annual_TimeSeries_SS_pm2.5$seasonal, type = "long", frequency = NULL)
SS_pm2.5_trend <-  ts_reshape(annual_TimeSeries_SS_pm2.5$trend, type = "long", frequency = NULL)
SS_pm2.5_random <-  ts_reshape(annual_TimeSeries_SS_pm2.5$random, type = "long", frequency = NULL)

ZU_pm10_x <-  ts_reshape(annual_TimeSeries_ZU_pm10$x, type = "long", frequency = NULL)
ZU_pm10_seasonal <-  ts_reshape(annual_TimeSeries_ZU_pm10$seasonal , type = "long", frequency = NULL)
ZU_pm10_trend <-  ts_reshape(annual_TimeSeries_ZU_pm10$trend , type = "long", frequency = NULL)
ZU_pm10_random <-  ts_reshape(annual_TimeSeries_ZU_pm10$random , type = "long", frequency = NULL)
ZU_pm2.5_x <-  ts_reshape(annual_TimeSeries_ZU_pm2.5$x, type = "long", frequency = NULL)
ZU_pm2.5_seasonal <-  ts_reshape(annual_TimeSeries_ZU_pm2.5$seasonal, type = "long", frequency = NULL)
ZU_pm2.5_trend <-  ts_reshape(annual_TimeSeries_ZU_pm2.5$trend, type = "long", frequency = NULL)
ZU_pm2.5_random <-  ts_reshape(annual_TimeSeries_ZU_pm2.5$random, type = "long", frequency = NULL)

## rename column and combine dataset
colnames(UB_pm10_x)[2] <- "filled pm10"
colnames(UB_pm10_seasonal)[2] <- "seasonal pm10" 
colnames(UB_pm10_trend)[2] <- "trend pm10"
colnames(UB_pm10_random)[2] <- "random pm10"
colnames(UB_pm2.5_x)[2] <- "filled pm2.5"
colnames(UB_pm2.5_seasonal)[2] <- "seasonal pm2.5" 
colnames(UB_pm2.5_trend)[2] <- "trend pm2.5"
colnames(UB_pm2.5_random)[2] <- "random pm2.5"
annual_filled_UB <- cbind((UB_pm10_x)[2], (UB_pm10_seasonal)[2], (UB_pm10_trend)[2], (UB_pm10_random)[2],
                          (UB_pm2.5_x)[2], (UB_pm2.5_seasonal)[2], (UB_pm2.5_trend)[2], (UB_pm2.5_random)[2])

colnames(DZ_pm10_x)[2] <- "filled pm10"
colnames(DZ_pm10_seasonal)[2] <- "seasonal pm10" 
colnames(DZ_pm10_trend)[2] <- "trend pm10"
colnames(DZ_pm10_random)[2] <- "random pm10"
colnames(DZ_pm2.5_x)[2] <- "filled pm2.5"
colnames(DZ_pm2.5_seasonal)[2] <- "seasonal pm2.5" 
colnames(DZ_pm2.5_trend)[2] <- "trend pm2.5"
colnames(DZ_pm2.5_random)[2] <- "random pm2.5"
annual_filled_DZ <- cbind((DZ_pm10_x)[2], (DZ_pm10_seasonal)[2], (DZ_pm10_trend)[2], (DZ_pm10_random)[2],
                          (DZ_pm2.5_x)[2], (DZ_pm2.5_seasonal)[2], (DZ_pm2.5_trend)[2], (DZ_pm2.5_random)[2])

colnames(SS_pm10_x)[2] <- "filled pm10"
colnames(SS_pm10_seasonal)[2] <- "seasonal pm10" 
colnames(SS_pm10_trend)[2] <- "trend pm10"
colnames(SS_pm10_random)[2] <- "random pm10"
colnames(SS_pm2.5_x)[2] <- "filled pm2.5"
colnames(SS_pm2.5_seasonal)[2] <- "seasonal pm2.5" 
colnames(SS_pm2.5_trend)[2] <- "trend pm2.5"
colnames(SS_pm2.5_random)[2] <- "random pm2.5"
annual_filled_SS <- cbind((SS_pm10_x)[2], (SS_pm10_seasonal)[2], (SS_pm10_trend)[2], (SS_pm10_random)[2],
                          (SS_pm2.5_x)[2], (SS_pm2.5_seasonal)[2], (SS_pm2.5_trend)[2], (SS_pm2.5_random)[2])

colnames(ZU_pm10_x)[2] <- "filled pm10"
colnames(ZU_pm10_seasonal)[2] <- "seasonal pm10" 
colnames(ZU_pm10_trend)[2] <- "trend pm10"
colnames(ZU_pm10_random)[2] <- "random pm10"
colnames(ZU_pm2.5_x)[2] <- "filled pm2.5"
colnames(ZU_pm2.5_seasonal)[2] <- "seasonal pm2.5" 
colnames(ZU_pm2.5_trend)[2] <- "trend pm2.5"
colnames(ZU_pm2.5_random)[2] <- "random pm2.5"
annual_filled_ZU <- cbind((ZU_pm10_x)[2], (ZU_pm10_seasonal)[2], (ZU_pm10_trend)[2], (ZU_pm10_random)[2],
                          (ZU_pm2.5_x)[2], (ZU_pm2.5_seasonal)[2], (ZU_pm2.5_trend)[2], (ZU_pm2.5_random)[2])

############### bind data clean daily data + filled daily data
df_UB_1028 <- cbind(df_daily_UB, annual_filled_UB )
df_DZ_1028 <- cbind(df_daily_DZ, annual_filled_DZ)
df_SS_1028 <- cbind(df_daily_SS, annual_filled_SS)
df_ZU_1028 <- cbind(df_daily_ZU, annual_filled_ZU)

df_trend_1028 <- rbind(df_UB_1028, df_DZ_1028, df_SS_1028, df_ZU_1028)

library(ggplot2)
ggplot(data=df_UB_1028) + 
  aes(Date, `filled pm10`) +
  geom_line() +
  geom_point(aes(df_UB_1028$Date, df_UB_1028$pm10), color = "pink", alpha=0.4)

##################### annual changes of PMs, with trendline, correlation
## pm10
annual_p10 <- df_trend_1028 |>
  ggplot(aes(Date, `filled pm10`*1000)) +
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
df_trend_1028 <- df_trend_1028 |>
  mutate(Seasons = case_when(
    Month > 10 | Month < 3 ~ "Q1,",
    Month > 2 & Month < 6 ~ "Q2,",
    Month > 5 & Month < 11 ~ "Q3/4,")) 

annual_p2.5 <- df_trend_1028 |>
  ggplot(aes(Date, `filled pm2.5`*1000), na.rm=T) +
  geom_line(alpha=0.4) +
  geom_point(aes(y=pm2.5*1000), shape =1, size=0.1) +
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

annual_p2.5

library(patchwork)
plot_20241103 <- annual_p10 + annual_p2.5
ggsave("fig, trends.png", plot_20241103, width = 9, height = 5.5, units = "in", dpi = 400)

############################ seasonal changes in PMs: winter and spring
df_clean <- df_clean |>
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


require(zoo)
df_UB_1028 |>
#  group_by(Station.name) |>
  ggplot(aes(Date, `filled pm10`)) +
  geom_point(shape=1, position = "jitter") +
 # geom_line(aes(Date, `seasonal pm10`),  alpha=0.8, colour="blue") + 
#  geom_line(aes(Date, `trend pm10`),  alpha=0.8, colour="blue") + 
  geom_line(aes(y = rollmean(
    df_UB_1028$`filled pm10` , 11, fill = NA, align = "left"), colour="blue"), linewidth = 0.5) +
  theme_bw() +
  geom_point(aes(y = rollmean(
    df_UB_1028$`filled pm10` , 365, fill = NA, align = "left"), colour="red", alpha = 0.2)) +
  theme_bw() +
#  facet_wrap(~Station.name) +
  ylim(0,0.75)


library(tidyverse)
test <- df_UB_1028 |>
  filter(pm10 < 1) |> 
ggplot(aes(day, pm10)) +
  geom_point() +
  geom_point(aes(df_SS_1028$Date, df_SS_1028$day), color = "red", alpha=0.2)



plot(test_convert,test_convert2.5)
df_cleaned_daily_UB = df_clean[df_clean$Station.name == "UB", ]
df_cleaned_daily_UB <- df_cleaned_daily_UB |>
  group_by(Date) |>
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), by =Date))
  
plot(df_cleaned_daily_UB$Date, df_cleaned_daily_UB$pm10, main = "", xlab = " ", ylab=" ")
lines(df_cleaned_daily_UB$Date, test_convert$day, main = "", ylab=" ", xlab = " ", col = "#EE7600")
lines(df_cleaned_daily_UB$Date, df_cleaned_daily_UB$pm2.5, main = "", ylab=" ", xlab = " ", col = "#CD0000")
plot(df_cleaned_daily_DZ$Date, df_cleaned_daily_DZ$pm10, main = "", xlab = " ", ylab=" ")
lines(df_cleaned_daily_DZ$Date, df_cleaned_daily_DZ$pm2.5, main = "", ylab=" ", xlab = " ", col = "#EE7600")
plot(df_cleaned_daily_ZU$Date, df_cleaned_daily_ZU$pm10, main = "", xlab = " ", ylab=" ")
lines(df_cleaned_daily_ZU$Date, df_cleaned_daily_ZU$pm2.5, main = "", ylab=" ", xlab = " ", col = "#FFFF00")
plot(df_cleaned_daily_SS$Date, df_cleaned_daily_SS$pm10, main = "", xlab = " ", ylab=" ")
lines(df_cleaned_daily_SS$Date, df_cleaned_daily_SS$pm2.5, main = "", ylab=" ", xlab = " ", col = "#008B45")


boxplot(df_UB_norm$Visibility, na.rm = TRUE)
boxplot(df_UB_comp$Visibility, na.rm = TRUE)
t.test(df_UB_norm$Visibility,df_UB_comp$Visibility)
plot(density(df_UB_norm$Visibility, na.rm = TRUE), main = "Visibility")
lines(density(df_UB_comp$Visibility, na.rm = TRUE), col = "red" )

### decompose x
plot(annual_TimeSeries_UB_pm10$x, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_UB_pm2.5$x, main = "", ylab=" ", xlab = " ", col = "#CD0000")
plot(annual_TimeSeries_DZ_pm10$x, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_DZ_pm2.5$x, main = "", ylab=" ", xlab = " ", col = "#EE7600")
plot(annual_TimeSeries_ZU_pm10$x, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_ZU_pm2.5$x, main = "", ylab=" ", xlab = " ", col = "#FFFF00")
plot(annual_TimeSeries_SS_pm10$x, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_SS_pm2.5$x, main = "", ylab=" ", xlab = " ", col = "#008B45")
### annual trends
plot(annual_TimeSeries_UB_pm10$trend, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_UB_pm2.5$trend, main = "", ylab=" ", xlab = " ", col = "#CD0000")
plot(annual_TimeSeries_DZ_pm10$trend, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_DZ_pm2.5$trend, main = "", ylab=" ", xlab = " ", col = "#EE7600")
plot(annual_TimeSeries_ZU_pm10$trend, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_ZU_pm2.5$trend, main = "", ylab=" ", xlab = " ", col = "#FFFF00")
plot(annual_TimeSeries_SS_pm10$trend, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_SS_pm2.5$trend, main = "", ylab=" ", xlab = " ", col = "#008B45")
## annual trend +seasonal
plot(annual_TimeSeries_UB_pm10$trend + annual_TimeSeries_UB_pm10$seasonal, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_UB_pm2.5$trend + annual_TimeSeries_UB_pm10$seasonal, main = "", ylab=" ", xlab = " ", col = "#CD0000")
plot(annual_TimeSeries_DZ_pm10$trend + annual_TimeSeries_DZ_pm10$seasonal, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_DZ_pm2.5$trend + annual_TimeSeries_DZ_pm10$seasonal, main = "", ylab=" ", xlab = " ", col = "#EE7600")
plot(annual_TimeSeries_ZU_pm10$trend + annual_TimeSeries_ZU_pm10$seasonal, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_ZU_pm2.5$trend + annual_TimeSeries_ZU_pm10$seasonal, main = "", ylab=" ", xlab = " ", col = "#FFFF00")
plot(annual_TimeSeries_SS_pm10$trend + annual_TimeSeries_SS_pm10$seasonal, main = "", xlab = " ", ylab=" ")
lines(annual_TimeSeries_SS_pm2.5$trend + annual_TimeSeries_SS_pm10$seasonal, main = "", ylab=" ", xlab = " ", col = "#008B45")

df_cleaned_daily_UB <- df_clean |>
  filter(Station.name == "UB")
df_cleaned_daily_DZ <- df_clean |>
  filter(Station.name == "DZ")
df_cleaned_daily_ZU <- df_clean |>
  filter(Station.name == "ZU")
df_cleaned_daily_SS <- df_clean |>
  filter(Station.name == "SS")

plot(df_cleaned_daily_UB$Date, df_cleaned_daily_UB$pm10, main = "", xlab = " ", ylab=" ")
lines(df_cleaned_daily_UB$Date, df_cleaned_daily_UB$pm2.5, main = "", ylab=" ", xlab = " ", col = "#CD0000")
plot(df_cleaned_daily_DZ$Date, df_cleaned_daily_DZ$pm10, main = "", xlab = " ", ylab=" ")
lines(df_cleaned_daily_DZ$Date, df_cleaned_daily_DZ$pm2.5, main = "", ylab=" ", xlab = " ", col = "#EE7600")
plot(df_cleaned_daily_ZU$Date, df_cleaned_daily_ZU$pm10, main = "", xlab = " ", ylab=" ")
lines(df_cleaned_daily_ZU$Date, df_cleaned_daily_ZU$pm2.5, main = "", ylab=" ", xlab = " ", col = "#FFFF00")
plot(df_cleaned_daily_SS$Date, df_cleaned_daily_SS$pm10, main = "", xlab = " ", ylab=" ")
lines(df_cleaned_daily_SS$Date, df_cleaned_daily_SS$pm2.5, main = "", ylab=" ", xlab = " ", col = "#008B45")
## ("#CD0000", "#EE7600", "#FFFF00", "#008B45")

plot(annual_TimeSeries_UB_pm10)
plot(annual_TimeSeries_DZ_pm10)
plot(annual_TimeSeries_ZU_pm10)
plot(annual_TimeSeries_SS_pm10)
plot(annual_TimeSeries_UB_pm2.5)
plot(annual_TimeSeries_DZ_pm2.5)
plot(annual_TimeSeries_ZU_pm2.5)
plot(annual_TimeSeries_SS_pm2.5)