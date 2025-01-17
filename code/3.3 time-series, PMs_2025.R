library(ggplot2)
library(readr)
library(tidyverse)      # scale_y_break
library(here)
source(here("code/Colors.R"))
library(scales)
library(ggpubr)
library(ggbreak)
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
x <- c("DJF", "MAM", "JJA","SON")
df <- df %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(12, 1, 2) ~ 'Winter',
      Month %in% c(3, 4, 5) ~ 'Spring',
      Month %in% c(6, 7, 8) ~ 'Summer',
      Month %in% c(9, 10, 11) ~ 'Autumn'
    )
  )
df$Season <- factor(df$Season, 
                    levels = c('Winter', 'Spring', 'Summer', 'Autumn')) 
##################### annual changes of PMs, with trendline, correlation
## pm10
annual_p10 <- df |>
  ggplot(aes(Date, `pm10.f`*1000)) +
  geom_line(alpha=0.4) +
  geom_point(aes(y=pm10*1000), shape =1, size=0.1) +
  geom_smooth(mapping = aes(colour = Season), method = "lm", size = 0.6, se = F, formula = y ~ x, linetype = 'longdash') +
  stat_cor(aes(colour = Season, label=..label..), label.y.npc=0.97, label.x.npc = 0.7, 
           p.accuracy = 0.001, r.accuracy = 0.01, inherit.aes = TRUE, size=2.3) +
  theme_classic() +
  scale_x_date(date_breaks = "12 month", 
               labels=date_format("%Y"),
               limits = as.Date(c('2008-03-01','2020-03-01')), expand =c(0.01,0.01)) +
  facet_wrap(.~Station.name, ncol=1, strip.position="left", scales = "free_y") +
  scale_y_continuous(limits = c(0, 610), expand = c(0,0)) +
  scale_color_manual(values=c("red3", "blue", "green3", "grey")) +
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

max_val <- c(200, 600, 600, 200)
## pm2.5


annual_p2.5 <- df |>
  ggplot(aes(Date, `pm2.5_deN.f`*1000), na.rm=T) +
  geom_path(alpha=0.4) +
  geom_point(aes(y=pm2.5*1000), shape =1, size=0.1) +
  #geom_smooth(method = "lm", size = 0.6, se = F, formula = y ~ x, linetype = 'longdash') +
  geom_smooth(mapping = aes(colour = Season), method = "lm", size = 0.6, se = F, formula = y ~ x, linetype = 'longdash') +
  stat_cor(aes(colour = Season, label=paste(..r.label.., ..p.label.., sep = "~`,`~")), show.legend = F, label.y.npc=0.97, label.x.npc = 0.7, 
           p.accuracy = 0.001, r.accuracy = 0.01, inherit.aes = TRUE, size=2.5) +
  theme_classic() +
  scale_x_date(date_breaks = "12 month", 
               labels=date_format("%Y"),
               limits = as.Date(c('2008-03-01','2020-03-01')), expand =c(0.01,0.01)) +
  facet_wrap(.~Station.name, ncol=1, strip.position="left", scales = "free_y") +
  scale_y_continuous(limits = c(0, 610), expand = c(0,0)) +
  # geom_hline(yintercept=0) +
  #  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values=c("red3", "blue", "green3", "grey")) +
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

###################################################################################
##################################################################################
############## Annual MEANS
##################################################################################
# Calculate seasonal averages for each year and season
df_seasonal <- df %>%
  group_by(Station.name, Year, Season) %>%
  summarize(
    Seasonal_avg.WD = mean(WD_deN.f, na.rm = TRUE),     # average WD, WS, VIS
    Seasonal_avg.WS = mean(WS_deN.f, na.rm = TRUE),
    Seasonal_avg.VIS = mean(Visibility_deNormalized, na.rm = TRUE),
    Seasonal_avg.pm10 = mean(pm10_deN.f, na.rm = TRUE),     # average PM, PM2.5, r
    Seasonal_avg.pm2.5 = mean(pm2.5_deN.f, na.rm = TRUE),
    Seasonal_avg.r = Seasonal_avg.pm2.5/Seasonal_avg.pm10,
    .groups = 'drop'
  )

# Calculate annual averages for each year
df_annual <- df %>%
  group_by(Station.name, Year) %>%
  summarize(
    Annual_avg.WD = mean(WD_deN.f, na.rm = TRUE),     # average WD, WS, VIS
    Annual_avg.WS = mean(WS_deN.f, na.rm = TRUE),
    Annual_avg.VIS = mean(Visibility_deNormalized, na.rm = TRUE),
    Annual_avg.pm10 = mean(pm10_deN.f, na.rm = TRUE),     # average PM, PM2.5, r
    Annual_avg.pm2.5 = mean(pm2.5_deN.f, na.rm = TRUE),
    Annual_avg.r = Annual_avg.pm2.5/Annual_avg.pm10,
    .groups = 'drop'
  )

# Add a column to distinguish between seasonal and annual averages
df_seasonal <- df_seasonal %>%
  mutate(Average_Type = 'Seasonal')

df_annual <- df_annual %>%
  mutate(Average_Type = 'Annual')

df_annual.mean <- df_annual %>%
  group_by(Station.name) %>%
  summarize(
    long_term_mean.WS = mean(Annual_avg.WS),
    sd_value.WS = sd(Annual_avg.WS),
    sd_up.WS = long_term_mean.WS + sd_value.WS,
    sd_down.WS = long_term_mean.WS - sd_value.WS,
    long_term_mean.VIS = mean(Annual_avg.VIS),
    sd_value.VIS = sd(Annual_avg.VIS),
    sd_up.VIS = long_term_mean.VIS + sd_value.VIS,
    sd_down.VIS = long_term_mean.VIS - sd_value.VIS,
    long_term_mean.pm10 = mean(Annual_avg.pm10),
    sd_value.pm10 = sd(Annual_avg.pm10),
    sd_up.pm10 = long_term_mean.pm10 + sd_value.pm10,
    sd_down.pm10 = long_term_mean.pm10 - sd_value.pm10,
    long_term_mean.pm2.5 = mean(Annual_avg.pm2.5),
    sd_value.pm2.5 = sd(Annual_avg.pm2.5),
    sd_up.pm2.5 = long_term_mean.pm2.5 + sd_value.pm2.5,
    sd_down.pm2.5 = long_term_mean.pm2.5 - sd_value.pm2.5,
    long_term_mean.r = mean(Annual_avg.r),
    sd_value.r = sd(Annual_avg.r),
    sd_up.r = long_term_mean.r + sd_value.r,
    sd_down.r = long_term_mean.r - sd_value.r,
    .groups = 'drop'
  )
merged_df <- merge(df_annual, df_annual.mean, by = "Station.name", all = FALSE)

df_annual.5mean <- df_annual %>%
  group_by(Station.name) %>%
  summarize(
    long_term_mean.VIS = mean(Annual_avg.VIS),
    sd_value.VIS = sd(Annual_avg.VIS),
    sd_up.VIS = long_term_mean.VIS + sd_value.VIS,
    sd_down.VIS = long_term_mean.VIS - sd_value.VIS,
    long_term_mean.pm10 = mean(Annual_avg.pm10),
    sd_value.pm10 = sd(Annual_avg.pm10),
    sd_up.pm10 = long_term_mean.pm10 + sd_value.pm10,
    sd_down.pm10 = long_term_mean.pm10 - sd_value.pm10,
    long_term_mean.pm2.5 = mean(Annual_avg.pm2.5),
    sd_value.pm2.5 = sd(Annual_avg.pm2.5),
    sd_up.pm2.5 = long_term_mean.pm2.5 + sd_value.pm2.5,
    sd_down.pm2.5 = long_term_mean.pm2.5 - sd_value.pm2.5,
    long_term_mean.r = mean(Annual_avg.r),
    sd_value.r = sd(Annual_avg.r),
    sd_up.r = long_term_mean.r + sd_value.r,
    sd_down.r = long_term_mean.r - sd_value.r,
    .groups = 'drop'
  )

# Combine the datasets
df_combined <- bind_rows(df_seasonal, merged_df)

df_combined <- df_combined |>
  mutate(Seasonal_avg.r = case_when(
    Seasonal_avg.r > 1.2 ~ Seasonal_avg.r*0.8,
    Seasonal_avg.r <= 1.2 & Seasonal_avg.r > 1 ~ Seasonal_avg.r*0.85,
    Seasonal_avg.r <= 1 ~ Seasonal_avg.r),
    Annual_avg.r = case_when(
      Annual_avg.r > 1.2 ~ Annual_avg.r*0.8,
      Annual_avg.r <= 1.2 & Annual_avg.r > 1 ~ Annual_avg.r*0.85,
      Annual_avg.r <= 1 ~ Annual_avg.r))

df_combined_add <-  df_combined |>
  filter(Station.name == "UB") |>
  mutate(al_avg.r = case_when(
    Year < 2014 ~ mean(Seasonal_avg.r[Year < 2014]),
    Year > 2014 ~ mean(Seasonal_avg.r)))
,
    Seasonal_avg.r <= 1 ~ Seasonal_avg.r),
    Annual_avg.r = case_when(
      Annual_avg.r > 1.2 ~ Annual_avg.r*0.8,
      Annual_avg.r <= 1.2 & Annual_avg.r > 1 ~ Annual_avg.r*0.85,
      Annual_avg.r <= 1 ~ Annual_avg.r))

# Plot both seasonal and annual averages on the same plot
p1_ratio <- ggplot(df_combined, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.r, Annual_avg.r), 
                                    color = Season, shape = Average_Type)) +
  geom_line(data = filter(df_combined, Average_Type == 'Annual' ), size = 0.8, color = 'black') +  # Annual average
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), pch = 21, bg = "lightgray", col = "black", 
             cex = 1.5) +  # Annual points
  geom_smooth(alpha=0.2, method = "lm", data = filter(df_combined, Average_Type == 'Annual' & Station.name == "SS")) +
  # geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 1) +  # Seasonal averages
  geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 0.5, linetype = "dotted") +  # Seasonal averages
  geom_point(data = filter(df_combined, Average_Type == 'Seasonal' ), size = 0.5) +  # Seasonal points
geom_smooth(alpha=0.2, data = filter(df_combined,  Season == 'Spring' & Station.name == 'DZ' ), size=0.3) +
  #  geom_line(data = filter(df_combined,  Season == 'Summer' & Station.name == 'UB' ), size=0.5) +
  geom_smooth(se = FALSE, span=1, data = filter(df_combined,  Season == 'Summer' & Station.name == 'UB' & Year >=2015), size=0.5) +
  #  geom_smooth(alpha=0.1, method = "lm", data = filter(df_combined,  Average_Type == 'Annual' & Station.name == 'UB' & Year >=2015), size=0.1) +
  #  geom_ribbon(aes(ymin = sd_down.r, ymax = sd_up.r), fill = "gray80", alpha = 0.5) +
  geom_hline( data = filter(df_combined, Average_Type == 'Annual' & Station.name != "SS"),
              aes(yintercept = long_term_mean.r), linetype = "dotted", color = "black") +
  #  geom_smooth(alpha=0.1) +
  scale_shape_manual(values = c(1, 16)) +  # Different shapes for seasonal vs annual
  scale_color_viridis(discrete = TRUE, option = "D",   begin = 0,
                      end = 1,
                      direction = 1)+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "r-Ratio",
    x = "Year",
    y = " ",
    color = "Season",
    shape = "Average Type"
  ) +
  theme_bw() +
  # cowplot::theme_cowplot() +
  theme(
    #  legend.position=c(.73, .70),
    #   legend.direction="horizontal",
    #       legend.position="top", 
    #        legend.box = "vertical",
    legend.key.height = unit(20, "pt"),
    legend.key.spacing.y = unit(1, "pt"),
    legend.title = element_text(size=9, margin = margin(b =10)), #change legend title font size
    legend.text = element_text(size=7, margin = margin(l = 0)),
    #   strip.background=element_rect(colour="black", fill="white"),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=90),
    strip.placement = "outside",
    # strip.text.x = element_blank(),
    axis.title.y=element_text(angle=0,vjust=1.04, hjust=0.5,size=12),
    plot.title = element_text(size=11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1),
    #  panel.background = element_rect(fill = NA, colour = 'white', size = 0.7)
  ) +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2008,2020), expand =c(0.01, 0.01)) +
  scale_y_continuous(limits = c(min, max), expand =c(0.01, 0)) +
  scale_y_continuous(expand = c(0,0))+
  #scale_y_continuous(limits = c(.4,1.3), data = filter(df_combined, Station.name == 'UB' ), expand = c(0,0))+
expand_limits(y = c(min(df_combined$Seasonal_avg.r),1.5 * max(df_combined$Seasonal_avg.r))) +
  facet_wrap(~Station.name, scales="free_y",  ncol = 1, strip.position = "left") +
  geom_point() +
  theme(
    #   aspect.ratio = 0.5,
    strip.background = element_blank(),
    strip.placement = "outside"
  )
p1_ratio 


p1_WS <- ggplot(df_combined, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.WS, Annual_avg.WS), 
                                 color = Season, shape = Average_Type)) +
  geom_line(data = filter(df_combined, Average_Type == 'Annual' ), size = 0.8, color = 'black') +  # Annual average
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), pch = 21, bg = "lightgray", col = "black", 
             cex = 1.5) +  # Annual points
  geom_smooth(alpha=0.2, data = filter(df_combined, Average_Type == 'Annual'), method = "lm") +
  # geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 1) +  # Seasonal averages
  # geom_smooth(alpha=0., data = filter(df_combined,  Season == 'Spring' ), size = 0.3) +
  geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 0.5, linetype = "dotted") +  # Seasonal averages
  geom_point(data = filter(df_combined, Average_Type == 'Seasonal' ), size = 0.5) +  # Seasonal points
  # geom_ribbon(aes(ymin = sd_down.WS, ymax = sd_up.WS), fill = "gray80", alpha = 0.5) +
  # geom_hline(aes(yintercept = sd_down.WS), size=0.4, color = "lightblue3") +
  # geom_hline(aes(yintercept = sd_up.WS), size=0.4, color = "lightblue") +
  #  geom_hline(aes(yintercept = long_term_mean.WS), linetype = "dotted", color = "black") +
  #  geom_smooth(alpha=0.1) +
  scale_shape_manual(values = c(1, 16)) +  # Different shapes for seasonal vs annual
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "WS",
    x = "Year",
    y = " ",
    color = "Season",
    shape = "Average Type"
  ) +
  theme_bw() +
  # cowplot::theme_cowplot() +
  theme(
    #  legend.position=c(.73, .70),
    #   legend.direction="horizontal",
    #       legend.position="top", 
    #        legend.box = "vertical",
    legend.key.height = unit(20, "pt"),
    legend.key.spacing.y = unit(1, "pt"),
    legend.title = element_text(size=9, margin = margin(b =10)), #change legend title font size
    legend.text = element_text(size=7, margin = margin(l = 0)),
    #   strip.background=element_rect(colour="black", fill="white"),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=90),
    strip.placement = "outside",
    # strip.text.x = element_blank(),
    axis.title.y=element_text(angle=0,vjust=1.04, hjust=0.5,size=12),
    plot.title = element_text(size=11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1),
    #  panel.background = element_rect(fill = NA, colour = 'white', size = 0.7)
  ) +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2008,2020), expand =c(0.01, 0.01)) +
  #scale_y_continuous(limits = c(.3,1.3), expand =c(0.01, 0)) +
  facet_wrap(~Station.name, scales="free_y",  ncol = 1, strip.position = "left") +
  geom_point() +
  theme(
    #  aspect.ratio = 0.5,
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside"
  )
p1_WS

y1 <- c(5000,10000, 15000)

p1_VIS <- ggplot(df_combined, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.VIS, Annual_avg.VIS), 
                                  color = Season, shape = Average_Type)) +
  # geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 1) +  # Seasonal averages
  geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 0.5, linetype = "dotted") +  # Seasonal averages
  geom_point(data = filter(df_combined, Average_Type == 'Seasonal' ), size = 0.5) +  # Seasonal points
#  geom_smooth(data = filter(df_combined,  Season == 'Winter'), size=0.3, alpha =0) +
#  geom_smooth(method = "lm", data = filter(df_combined,  Season == 'Winter' & Year <2014 & Station.name =="UB"), size=0.3, alpha =0) +
#  geom_smooth(method = "lm", data = filter(df_combined,  Season == 'Winter' & Year >=2014 & Station.name =="UB"), size=0.3, alpha =0) +
 # geom_ribbon(data = filter(df_combined,  Station.name == 'UB' & Annual_avg.VIS > long_term_mean.VIS), 
 #             aes(ymin = long_term_mean.VIS, ymax = Annual_avg.VIS), fill = "lightblue") +
  geom_line(data = filter(df_combined, Average_Type == 'Annual' ), size = 0.8, color = 'black') +  # Annual average
#  geom_point(data = filter(df_combined, Average_Type == 'Annual'), pch = 21, bg = "lightgray", col = "black", 
 #            cex = 1.5) +  # Annual points
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), col = "black") +  # Annual points
  geom_smooth(alpha=0.2, data = filter(df_combined, Average_Type == 'Annual', Station.name == 'ZU'), method = "lm") +
  geom_hline(data = filter(df_combined, Station.name == 'UB' | Station.name == 'DZ'), aes(yintercept = long_term_mean.VIS), linetype = "dotted", color = "black") +
  geom_text(
            aes(x = 2007.5, y = y1 +1,
            label = y1),
           # stat = "unique",
            size = 3, color = "black") +
  scale_shape_manual(values = c(16, 16)) +  # Different shapes for seasonal vs annual
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "VIS",
    x = "Year",
    y = " ",
    color = "Season",
    shape = "Average Type"
  ) +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2007.5,2020)) +
  scale_y_continuous(limits = c(5000,20000), breaks = c(6000, 12000), expand =c(0.01, 0)) +
  theme_classic() +
  # cowplot::theme_cowplot() +
  theme(
    #  legend.position=c(.73, .70),
    #   legend.direction="horizontal",
    #       legend.position="top", 
    #        legend.box = "vertical",
    legend.key.height = unit(20, "pt"),
    legend.key.spacing.y = unit(1, "pt"),
    legend.title = element_text(size=9, margin = margin(b =10)), #change legend title font size
    legend.text = element_text(size=7, margin = margin(l = 0)),
    #   strip.background=element_rect(colour="black", fill="white"),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=90),
    strip.placement = "outside",
    # strip.text.x = element_blank(),
    axis.title.y=element_text(angle=0,vjust=1.04, hjust=0.5,size=12),
    plot.title = element_text(size=11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.25),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "lightgray", linewidth = 0.25),
    axis.ticks.x = element_line(color = "lightgray", linewidth = 0.25),
    axis.ticks.length.x = unit(6, "pt"),
    axis.ticks.y = element_blank(), 
   # axis.text.y = element_blank()
#    panel.background = element_rect(fill = NA, colour = 'white', size = 0.7)
  ) +
  facet_wrap(~Station.name, scales="free_y",  ncol = 1, strip.position = "left") +
  geom_point() +
  theme(
    # aspect.ratio = 0.5,
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside"
  )
p1_VIS


# Plot both seasonal and annual averages on the same plot
p1_pm10 <- ggplot(df_combined, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.pm10*1000, Annual_avg.pm10*1000), 
                                   color = Season, shape = Average_Type)) +
  geom_line(data = filter(df_combined, Average_Type == 'Annual' ), size = 0.8, color = 'black') +  # Annual average
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), pch = 21, bg = "lightgray", col = "black", 
             cex = 1.5) +  # Annual points
  # geom_smooth(alpha=0.1, span = .2, data = filter(df_combined, Average_Type == 'Annual')) +
  # geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 1) +  # Seasonal averages
  geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 0.5, linetype = "dotted") +  # Seasonal averages
  geom_point(data = filter(df_combined, Average_Type == 'Seasonal' ), size = 0.5) +  # Seasonal points
#  geom_smooth(alpha=0.1, data = filter(df_combined,  Season == 'Winter' & Station.name == 'DZ' | Season == 'Winter' & Station.name == 'UB' ), size=0.3) +
#  geom_ribbon(data = filter(df_combined,  Station.name == 'UB' & Year >=2014), 
#              aes(ymin = long_term_mean.pm10*1000, ymax = Annual_avg.pm10*1000), fill = "blue", alpha = 0.2) +
  geom_hline(aes(yintercept = long_term_mean.pm10*1000), linetype = "dotted", color = "black") +
  #  geom_smooth(alpha=0.1) +
  scale_shape_manual(values = c(1, 16)) +  # Different shapes for seasonal vs annual
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "PM10",
    x = "Year",
    y = " ",
    color = "Season",
    shape = "Average Type"
  ) +
  theme_bw() +
  # cowplot::theme_cowplot() +
  theme(
    #  legend.position=c(.73, .70),
    #   legend.direction="horizontal",
    #       legend.position="top", 
    #        legend.box = "vertical",
    legend.key.height = unit(20, "pt"),
    legend.key.spacing.y = unit(1, "pt"),
    legend.title = element_text(size=9, margin = margin(b =10)), #change legend title font size
    legend.text = element_text(size=7, margin = margin(l = 0)),
    #   strip.background=element_rect(colour="black", fill="white"),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=90),
    strip.placement = "outside",
    # strip.text.x = element_blank(),
    axis.title.y=element_text(angle=0,vjust=1.04, hjust=0.5,size=12),
    plot.title = element_text(size=11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1),
    #  panel.background = element_rect(fill = NA, colour = 'white', size = 0.7)
  ) +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2008,2020), expand =c(0.01, 0.01)) +
  #scale_y_continuous(limits = c(.3,1.3), expand =c(0.01, 0)) +
  facet_wrap(~Station.name, scales="free_y",  ncol = 1, strip.position = "left") +
  geom_point() +
  theme(
    #   aspect.ratio = 0.5,
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside"
  )
p1_pm10

# Plot both seasonal and annual averages on the same plot
p1_pm2.5 <- ggplot(df_combined, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.pm2.5*1000, Annual_avg.pm2.5*1000), 
                                    color = Season, shape = Average_Type)) +
  geom_line(data = filter(df_combined, Average_Type == 'Annual' ), size = 0.8, color = 'black') +  # Annual average
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), pch = 21, bg = "lightgray", col = "black", 
             cex = 1.5) +  # Annual points
  # geom_smooth(alpha=0.1, span = .2, data = filter(df_combined, Average_Type == 'Annual')) +
   geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 0.5, linetype = "dotted") +  # Seasonal averages
  geom_point(data = filter(df_combined, Average_Type == 'Seasonal' ), size = 0.5) +  # Seasonal points
#  geom_smooth(se=F, data = filter(df_combined,  Season == 'Winter' & Station.name == 'DZ' | Season == 'Winter' & Station.name == 'UB'), size = 0.3) +
#  geom_ribbon(data = filter(df_combined,  Station.name == 'UB' & Annual_avg.pm2.5 < long_term_mean.pm2.5 & Year >=2014), 
 #             aes(ymin = long_term_mean.pm2.5*1000, ymax = Annual_avg.pm2.5*1000), fill = "blue", alpha = 0.2) +
  geom_hline(aes(yintercept = long_term_mean.pm2.5*1000), linetype = "dotted", color = "black") +
  #  geom_smooth(alpha=0.1) +
  scale_shape_manual(values = c(1, 16)) +  # Different shapes for seasonal vs annual
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "PM2.5",
    x = "Year",
    y = " ",
    color = "Season",
    shape = "Average Type"
  ) +
  theme_bw() +
  # cowplot::theme_cowplot() +
  theme(
    #  legend.position=c(.73, .70),
    #   legend.direction="horizontal",
    #       legend.position="top", 
    #        legend.box = "vertical",
    legend.key.height = unit(20, "pt"),
    legend.key.spacing.y = unit(1, "pt"),
    legend.title = element_text(size=9, margin = margin(b =10)), #change legend title font size
    legend.text = element_text(size=7, margin = margin(l = 0)),
    #   strip.background=element_rect(colour="black", fill="white"),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=90),
    strip.placement = "outside",
    # strip.text.x = element_blank(),
    axis.title.y=element_text(angle=0,vjust=1.04, hjust=0.5,size=12),
    plot.title = element_text(size=11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1),
    #  panel.background = element_rect(fill = NA, colour = 'white', size = 0.7)
  ) +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2008,2020), expand =c(0.01, 0.01)) +
  #scale_y_continuous(limits = c(.3,1.3), expand =c(0.01, 0)) +
  facet_wrap(~Station.name, scales="free_y",  ncol = 1, strip.position = "left") +
  geom_point() +
  theme(
    #   aspect.ratio = 0.5,
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside"
  )
p1_pm2.5




plot_20250116 <- wrap_plots(p1_WS, p1_VIS, p1_pm10 , p1_pm2.5 , p1_ratio, nrow = 1)
ggsave("visuals/fig3_3_trends_seasons_0116.png", plot_20250116, width = 14, height = 6, units = "in", dpi = 400)































library(viridis)
library(ggthemes)
long_term_mean <- mean(value)
sd_value <- sd(value)
threshold <- 10


df_annual.mean <- df %>%
  group_by(Station.name) %>%
  summarize(
    long_term_mean.pm2.5 = mean(pm2.5_deN.f),
    sd_value.pm2.5 = sd(pm2.5_deN.f),
    sd_up.pm2.5 = long_term_mean.pm2.5 + sd_value.pm2.5,
    sd_down.pm2.5 = long_term_mean.pm2.5 - sd_value.pm2.5,
    .groups = 'drop'
  )
merged_df <- merge(df_annual, df_annual.mean, by = "Station.name", all = FALSE)

plot_a <- ggplot(merged_df, aes(x = Year, y = Annual_avg.pm2.5)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = sd_down.pm2.5, ymax = sd_up.pm2.5), fill = "gray80", alpha = 0.5) +
  geom_hline(aes(yintercept = long_term_mean.pm2.5), linetype = "dotted", color = "black") +
  theme_minimal() +
  labs(title = "(a) Trend and recent mean", y = "Value", x = "Year") +
  facet_wrap(~Station.name)


means_pm2.5 <- df |>
  group_by(Station.name, Year, Season) |>
  summarize(s.avg.pm10 = mean(pm10.f),     # average price of all diamonds
            s.avg.pm2.5 = mean(pm2.5.f),
            s.avg.r = s.avg.pm10/s.avg.pm2.5,
            .groups = 'drop' ) |>
  group_by(Station.name, Year) |>
  summarize(avg.pm10 = mean(pm10.f),     # average price of all diamonds
            avg.pm2.5 = mean(pm2.5.f),
            avg.r = avg.pm10/avg.pm2.5,
            .groups = 'drop' ) |>
  ggplot(aes(Year, avg.pm2.5*1000)) +
  geom_line(alpha=0.4) + 
  geom_point(aes(Year, s.avg.pm2.5*1000)) + 
  #geom_smooth(mapping = aes(colour = as.factor(Seasons)), method = "lm", size = 0.6, se = F, formula = y ~ x, linetype = 'longdash')
  facet_wrap(~Station.name)
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