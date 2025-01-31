library(ggplot2)
library(readr)
library(tidyverse)      # scale_y_break
library(here)
source(here("code/Colors.R"))
library(scales)
library(ggpubr)
library(ggbreak)
library(patchwork)
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
df <- df |>
  filter(Year<2021)
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

# Calculate 5 year means for 4 SEASONS
df_seasonal_5year.mean <- df_seasonal %>%
  group_by(Station.name, Season) %>%
  summarize(
    I_5y_mean.WS = mean(Seasonal_avg.WS[Year<= 2013]),
    II_5y_mean.WS = mean(Seasonal_avg.WS[Year>=2014]),
    long_term_seasonal_mean.WS = mean(Seasonal_avg.WS),
    sd_value.WS = sd(Seasonal_avg.WS),
    sd_up.WS = long_term_seasonal_mean.WS + sd_value.WS,
    sd_down.WS = long_term_seasonal_mean.WS - sd_value.WS,
    I_5y_mean.VIS = mean(Seasonal_avg.VIS[Year<= 2013]),
    II_5y_mean.VIS = mean(Seasonal_avg.VIS[Year>=2014]),
    long_term_seasonal_mean.VIS = mean(Seasonal_avg.VIS),
    sd_value.VIS = sd(Seasonal_avg.VIS),
    sd_up.VIS = long_term_seasonal_mean.VIS + sd_value.VIS,
    sd_down.VIS = long_term_seasonal_mean.VIS - sd_value.VIS,
    I_5y_mean.pm10 = mean(Seasonal_avg.pm10[Year<= 2013]),
    II_5y_mean.pm10 = mean(Seasonal_avg.pm10[Year>=2014]),
    long_term_seasonal_mean.pm10 = mean(Seasonal_avg.pm10),
    sd_value.pm10 = sd(Seasonal_avg.pm10),
    sd_up.pm10 = long_term_seasonal_mean.pm10 + sd_value.pm10,
    sd_down.pm10 = long_term_seasonal_mean.pm10 - sd_value.pm10,
    I_5y_mean.pm2.5 = mean(Seasonal_avg.pm2.5[Year<= 2013]),
    II_5y_mean.pm2.5 = mean(Seasonal_avg.pm2.5[Year>=2014]),
    long_term_seasonal_mean.pm2.5 = mean(Seasonal_avg.pm2.5),
    sd_value.pm2.5 = sd(Seasonal_avg.pm2.5),
    sd_up.pm2.5 = long_term_seasonal_mean.pm2.5 + sd_value.pm2.5,
    sd_down.pm2.5 = long_term_seasonal_mean.pm2.5 - sd_value.pm2.5,
    I_5y_mean.r = mean(Seasonal_avg.r[Year<= 2013]),
    II_5y_mean.r = mean(Seasonal_avg.r[Year>=2014]),
    long_term_seasonal_mean.r = mean(Seasonal_avg.r),
    sd_value.r = sd(Seasonal_avg.r),
    sd_up.r = long_term_seasonal_mean.r + sd_value.r,
    sd_down.r = long_term_seasonal_mean.r - sd_value.r,
    .groups = 'drop'
  )
df_seasonal_5year.mean[df_seasonal_5year.mean == "NaN"] <- NA
#######
# Add a column to distinguish between seasonal and annual averages
df_seasonal <- df_seasonal %>%
  mutate(Average_Type = 'Seasonal')

df_seasonal_5year.mean <- df_seasonal_5year.mean %>%
  mutate(Average_Type = ("pre - post [2014]"),
         Year = 2020)






merged_seasonal_df <- bind_rows(df_seasonal, df_seasonal_5year.mean)

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
df_annual <- df_annual %>%
  mutate(Average_Type = 'Annual')

df_annual.mean <- df_annual %>%
  group_by(Station.name) %>%
  summarize(
    I_5y_mean.WS = mean(Annual_avg.WS[Year<= 2013]),
    II_5y_mean.WS = mean(Annual_avg.WS[Year>=2014]),
    long_term_mean.WS = mean(Annual_avg.WS),
    sd_value.WS = sd(Annual_avg.WS),
    sd_up.WS = long_term_mean.WS + sd_value.WS,
    sd_down.WS = long_term_mean.WS - sd_value.WS,
    I_5y_mean.VIS = mean(Annual_avg.VIS[Year<= 2013]),
    II_5y_mean.VIS = mean(Annual_avg.VIS[Year>=2014]),
    long_term_mean.VIS = mean(Annual_avg.VIS),
    sd_value.VIS = sd(Annual_avg.VIS),
    sd_up.VIS = long_term_mean.VIS + sd_value.VIS,
    sd_down.VIS = long_term_mean.VIS - sd_value.VIS,
    I_5y_mean.pm10 = mean(Annual_avg.pm10[Year<= 2013]),
    II_5y_mean.pm10 = mean(Annual_avg.pm10[Year>=2014]),
    long_term_mean.pm10 = mean(Annual_avg.pm10),
    sd_value.pm10 = sd(Annual_avg.pm10),
    sd_up.pm10 = long_term_mean.pm10 + sd_value.pm10,
    sd_down.pm10 = long_term_mean.pm10 - sd_value.pm10,
    I_5y_mean.pm2.5 = mean(Annual_avg.pm2.5[Year<= 2013]),
    II_5y_mean.pm2.5 = mean(Annual_avg.pm2.5[Year>=2014]),
    long_term_mean.pm2.5 = mean(Annual_avg.pm2.5),
    sd_value.pm2.5 = sd(Annual_avg.pm2.5),
    sd_up.pm2.5 = long_term_mean.pm2.5 + sd_value.pm2.5,
    sd_down.pm2.5 = long_term_mean.pm2.5 - sd_value.pm2.5,
    I_5y_mean.r = mean(Annual_avg.r[Year<= 2013]),
    II_5y_mean.r = mean(Annual_avg.r[Year>=2014]),
    long_term_mean.r = mean(Annual_avg.r),
    sd_value.r = sd(Annual_avg.r),
    sd_up.r = long_term_mean.r + sd_value.r,
    sd_down.r = long_term_mean.r - sd_value.r,
    .groups = 'drop'
  )
merged_annual_df <- merge(df_annual, df_annual.mean, by = "Station.name", all = FALSE)


########

# Combine the datasets
df_combined <- bind_rows(merged_seasonal_df, merged_annual_df)

df_combined <- df_combined |>
  mutate(Seasonal_avg.r = case_when(
    Seasonal_avg.r > 1.2 ~ Seasonal_avg.r*0.8,
    Seasonal_avg.r <= 1.2 & Seasonal_avg.r > 1 ~ Seasonal_avg.r*0.85,
    Seasonal_avg.r <= 1 ~ Seasonal_avg.r),
    Annual_avg.r = case_when(
      Annual_avg.r > 1.2 ~ Annual_avg.r*0.8,
      Annual_avg.r <= 1.2 & Annual_avg.r > 1 ~ Annual_avg.r*0.85,
      Annual_avg.r <= 1 ~ Annual_avg.r))


# Set aesthetics color and symbol
seasonal_color <- c('#a6611a','#dfc27d', '#80cdc1','#018571')
seasonal_color <- c('#0072B2','#E69F00', '#009E73','#D55E00')
seasonal_color <- c('#4E79A7','#E2C57D', '#F28E2B', '#9C755F')
seasonal_color <- c('#1f77b4','#2ca02c', '#ff7f0e', '#8c564b')



names(seasonal_color) <- unique(df_combined$Season[1:4])
df_combined$Average_Type <- factor(df_combined$Average_Type, 
                                   levels = c('Annual', 'Seasonal', 'pre - post [2014]')) 

mean_pch = 21
mean_bg_color = "white"
geom_line_size_L = 0.5
geom_line_size_M = 0.2
geom_point_size_L = 0.8
geom_point_size_M = 0.7
cex_size = 0.8
# Plot both seasonal and annual averages on the same plot
library(ggforce)
p1_ratio <- ggplot(df_combined, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.r, Annual_avg.r), 
                                    color = Season, shape = Average_Type)) +
  # geom_rect(data = filter(df_combined,  Station.name == 'UB' ), aes(xmin = 2013.9, xmax = 2020, ymin = -Inf, ymax = Inf),
  #           alpha = 0.8,
  #           fill = "azure", show.legend = F, linewidth = 0) +
  # geom_rect(data = filter(df_combined,  Station.name == 'DZ' ), aes(xmin = 2013.9, xmax = 2020, ymin = -Inf, ymax = Inf),
  #           alpha = 0.8,
  #           fill = "azure2", show.legend = F, linewidth = 0) +
  #  geom_smooth(alpha=0.2, method = "lm", data = filter(df_combined, Average_Type == 'Annual' & Station.name == "SS")) +
  geom_line(data = filter(df_combined, Average_Type == 'Annual' ), size = geom_line_size_L, color = 'black') +  # Annual average
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), bg = mean_bg_color, col = "black", 
             cex = cex_size, show.legend = T) +
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), pch = mean_pch, bg = mean_bg_color, col = "black", 
             cex = cex_size, show.legend = F) +
  # geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 1) +  # Seasonal averages
  geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = geom_line_size_M, linetype = "dashed", show.legend = F) +  # Seasonal averages
  geom_point(data = filter(df_combined, Average_Type == 'Seasonal' ), size = geom_point_size_M, show.legend = T) +  # Seasonal points
  geom_smooth(alpha=0.1, data = filter(df_combined,  Average_Type == 'Seasonal' & Season == 'Spring' & Station.name == 'DZ' ), size=0.2, show.legend = F) +
  geom_vline(aes(xintercept = 2020.5), linetype = "twodash", color = "grey", linewidth = 0.1) +
  #geom_segment(data=filter(df_combined, Average_Type == 'Annual'), aes(x=2020.3, y = I_5y_mean.r, yend = II_5y_mean.r), arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Winter' ), aes(x=2020.8, y = I_5y_mean.r, yend = II_5y_mean.r, color = Season), arrow = arrow(length = unit(0.1, "cm"), type = "closed"), show.legend = F) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Spring' ), aes(x=2021.1, y = I_5y_mean.r, yend = II_5y_mean.r, color = Season), arrow = arrow(length = unit(0.1, "cm"), type = "closed"), show.legend = F) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Summer' ), aes(x=2021.4, y = I_5y_mean.r, yend = II_5y_mean.r, color = Season), arrow = arrow(length = unit(0.1, "cm"), type = "closed"), show.legend = F) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Autumn' ), aes(x=2021.7, y = I_5y_mean.r, yend = II_5y_mean.r, color = Season), arrow = arrow(length = unit(0.1, "cm"), type = "closed"), show.legend = F) +    
  #  geom_line(data = filter(df_combined,  Season == 'Summer' & Station.name == 'UB' ), size=0.5) +
#  geom_smooth(se = FALSE, span=1, data = filter(df_combined,  Average_Type == 'Seasonal' & Season == 'Summer' & Station.name == 'UB' & Year >=2015), size=0.5, show.legend = F) +
  # geom_mark_ellipse(data = filter(df_combined,  Average_Type == 'pre - post [2014]' & Station.name == 'DZ'),
  #                  aes(x0=2021, y0=0.55, label = "changes", description = "pre - post, 2014"),  label.margin = margin(0, 0, 0, 0, "mm"),
  #                  label.fontsize = 3, label.buffer = unit(1, "mm"),
  #                  label.minwidth = unit(5, "mm"),
  #                  expand = unit(0, "mm"),  con.type = "straight",
  #                  linewidth=0, alpha=0.1, 
  #                   show.legend = F) +
  #  geom_smooth(alpha=0.1, method = "lm", data = filter(df_combined,  Average_Type == 'Annual' & Station.name == 'UB' & Year >=2015), size=0.1) +
  #geom_ribbon(aes(ymin = long_term_mean.r - 0.1, ymax = long_term_mean.r + 0.1), fill = "grey", alpha = 0.5, linewidth = 0) +
#  geom_smooth(alpha=0.1, method = "lm", data = filter(df_combined,  Average_Type == 'Annual' & Station.name == 'SS' ), size=0.1) +
  geom_line(aes(x= Year, y = long_term_mean.r), alpha=0.3, linetype="solid", linewidth = 0.3, color = "grey") +
  geom_line(aes(x= Year, y = long_term_mean.r),  linetype="dotdash", linewidth = 0.3, color = "black") +
  #  geom_smooth(alpha=0.1) +
  # geom_text(data = filter(df_combined, Station.name == 'UB'),
  #           x = 2020.5, y = 0.96, label = "finer PM (summer, post-2015)",
  #           # stat = "unique",
  #           color = "orange", size = 2, hjust = 0) +
  # geom_text(data = filter(df_combined, Station.name == 'ZU'),
  #           x = 2011.1, y = 0.45, label = "0.45",
  #           # stat = "unique",
  #           size = 2, color = "red", hjust = 0) +
  # geom_text(data = filter(df_combined, Station.name == 'ZU'),
  #           x = 2016.1, y = 0.45, label = "0.45",
  #           # stat = "unique",
  #           size = 2, color = "red", hjust = 0) +
  # geom_text(data = filter(df_combined, Station.name == 'SS'),
  #           x = 2011.1, y = 0.4, label = "0.4",
  #           # stat = "unique",
  #           size = 2, color = "red", hjust = 0) +
  # geom_text(data = filter(df_combined, Station.name == 'SS'),
  #           x = 2012.1, y = 0.4, label = "0.4-Dust",
  #           # stat = "unique",
  #           size = 2, color = "red", hjust = 0) +
  # geom_text(data = filter(df_combined, Station.name == 'SS'),
  #           x = 2016.1, y = 0.4, label = "0.4-Dust",
  #           # stat = "unique",
  #           size = 2, color = "red", hjust = 0) +
  geom_text_repel(data = filter(df_combined, Average_Type == 'Seasonal' & Station.name == 'ZU' & Season == "Spring"), aes(label = ifelse(Seasonal_avg.r< 0.5, round(Seasonal_avg.r,2), ""), color = Season),
                  size = 2, 
                  nudge_y = -0.1,  # Adjusts vertical position
                  segment.color = "gray50", 
                  segment.size = 0.1,
                  show.legend = F) +  # Dynamic labels
  geom_text_repel(data = filter(df_combined, Average_Type == 'Seasonal' & Station.name == 'SS' & Year == 2011 |
                                  Station.name == 'SS' & Season == "Spring" & Year <= 2016), aes(label = ifelse(Seasonal_avg.r < 0.5, round(Seasonal_avg.r,2), ""), color = Season),
                  size = 2, 
                  nudge_y = -0.1,  # Adjusts vertical position
                  segment.color = "gray50", 
                  segment.size = 0.5,
                  show.legend = F) +  # Dynamic labels
  scale_shape_manual(values = c(1, 16, 17)) +  # Different shapes for seasonal vs annual
  # scale_color_viridis(discrete = TRUE, option = "D",   begin = 0,
  #                     end = 1,
  #                     direction = 1)+
  scale_color_manual(values=seasonal_color) +
  #scale_fill_viridis(discrete = TRUE) +
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
    legend.position = "right",
    legend.key.height = unit(10, "pt"),
    legend.key.spacing.y = unit(0.2, "pt"),
    legend.title = element_text(size=6, margin = margin(0,0,0,0)), #change legend title font size
    legend.text = element_text(size=6, margin = margin(0,0,0,0)),
    legend.justification="right",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,6,-10,-5),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=0),
    strip.text = element_text(size=6, margin=margin(0,0,0,0)),
    strip.placement = "outside",
    axis.title=element_text(angle=0,vjust=1.04, size=6),
    axis.text = element_text(size=5),
    plot.title = element_text(size=7),
    plot.margin = unit(c(0,0,0,-2), "mm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1)
  ) +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2008, 2021.7), expand =c(0.01, 0.5)) +
#  scale_y_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1), limits = c(0.3, 1.3), expand =c(0, 0)) +
  facet_wrap(~Station.name, scales="free_y", ncol = 1, strip.position = "left") 
p1_ratio



p1_WS <- ggplot(df_combined, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.WS, Annual_avg.WS), 
                                 color = Season, shape = Average_Type)) +
  # geom_rect(data = filter(df_combined,  Station.name == 'UB' ), aes(xmin = 2019.95, xmax = 2020, ymin = 1.7, ymax = 1.75),
  #           alpha = 0.8,
  #           fill = "blue", show.legend = F, linewidth = 0) +
  # geom_rect(data = filter(df_combined,  Station.name == 'ZU' ), aes(xmin = 2015.8, xmax = 2016.2, ymin = 3.15, ymax = 3.25),
  #           alpha = 0.8,
  #           fill = "red", show.legend = F, linewidth = 0) +
  # geom_rect(data = filter(df_combined,  Station.name == 'ZU' ), aes(xmin = 2010.8, xmax = 2011.2, ymin = 3.15, ymax = 3.25),
  #           alpha = 0.8,
  #           fill = "red", show.legend = F, linewidth = 0) +
  # geom_rect(data = filter(df_combined,  Station.name == 'SS' ), aes(xmin = 2011.8, xmax = 2012.2, ymin = 4.5, ymax = 4.6),
  #           alpha = 0.8,
  #           fill = "red", show.legend = F, linewidth = 0) +
  # geom_rect(data = filter(df_combined,  Station.name == 'SS' ), aes(xmin = 2015.8, xmax = 2016.2, ymin = 4.5, ymax = 4.6),
  #           alpha = 0.8,
  #           fill = "red", show.legend = F, linewidth = 0) +
  #  geom_smooth(alpha=0.2, method = "lm", data = filter(df_combined, Average_Type == 'Annual' & Station.name == "SS")) +
  geom_line(data = filter(df_combined, Average_Type == 'Annual' ), size = geom_line_size_L, color = 'black') +  # Annual average
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), pch = mean_pch, bg = mean_bg_color, col = "black", 
             cex = cex_size, show.legend = T) +  # Annual points
  # geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = 1) +  # Seasonal averages
  geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = geom_line_size_M, linetype = "dashed") +  # Seasonal averages
  geom_point(data = filter(df_combined, Average_Type == 'Seasonal' ), size = geom_point_size_M) +  # Seasonal points
  #geom_smooth(alpha=0.2, data = filter(df_combined, Average_Type == 'Annual'), method = "lm") +
  geom_vline(aes(xintercept = 2020.5), linetype = "twodash", color = "grey", linewidth = 0.1) +
 # geom_segment(data=filter(df_combined, Average_Type == 'Annual'), aes(x=2020.3, y = I_5y_mean.WS, yend = II_5y_mean.WS), arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Winter' ), aes(x=2020.8, y = I_5y_mean.WS, yend = II_5y_mean.WS, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Spring' ), aes(x=2021.1, y = I_5y_mean.WS, yend = II_5y_mean.WS, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Summer' ), aes(x=2021.4, y = I_5y_mean.WS, yend = II_5y_mean.WS, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Autumn' ), aes(x=2021.7, y = I_5y_mean.WS, yend = II_5y_mean.WS, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +
  geom_rect(data=filter(df_combined, Average_Type == 'pre - post [2014]'), aes(xmin = 2020, xmax = 2022, ymin = min(df_combined$I_5y_mean.WS), ymax = max(df_combined$I_5y_mean.WS))) +
  geom_smooth(alpha=0.1, method = "lm", data = filter(df_combined,  Average_Type == 'Annual' & Station.name == 'SS' | Average_Type == 'Annual' & Station.name == 'ZU'), size=0.1) +
  geom_smooth(alpha=0.1, method = "lm", data = filter(df_combined,  Average_Type == 'Annual'), size=0.05) +
#    geom_line( data = filter(df_combined, Average_Type == 'Annual' ),
   #          aes(x= Year, y = long_term_mean.WS), linetype = "dotted", color = "black") +
  # geom_text(data = filter(df_combined, Station.name == 'DZ'),
  #           x = 2014, y = 3.2, label = "decline (2014)",
  #           # stat = "unique",
  #           size = 2, color = "blue", hjust = 0) +
  geom_text_repel(data = filter(df_combined, Average_Type == 'Seasonal' & Season == "Spring" & Year == 2011 & Station.name == 'ZU' | 
                                  Season == "Spring" & Year == 2012 & Station.name == 'SS' |
                                  Season == "Spring" & Year == 2016 & Station.name == 'ZU'|
                                  Season == "Spring" & Year == 2016 & Station.name == 'SS'), aes(label = round(Seasonal_avg.WS,1), color = Season),
                  size = 2, 
                  nudge_y = 0.5,  # Adjusts vertical position
                  segment.color = "gray50", 
                  segment.size = 0.5) +  # Dynamic labels
  geom_text_repel(data = filter(df_combined, Average_Type == 'Seasonal' & Station.name == 'SS' & Year > 2008), aes(label = ifelse(Seasonal_avg.WS > 10, round(Seasonal_avg.WS,1), ""), color = Season),
                  size = 2, 
                  nudge_y = 0.5,  # Adjusts vertical position
                  segment.color = "gray50", 
                  segment.size = 0.5) +  # Dynamic labels
    scale_shape_manual(values = c(1, 16, 17)) +  # Different shapes for seasonal vs annual
  # scale_color_viridis(discrete = TRUE, option = "D",   begin = 0,
  #                     end = 1,
  #                     direction = 1)+
  scale_color_manual(values=seasonal_color) +
  #scale_fill_viridis(discrete = TRUE) +
  labs(
    title = bquote("WS"~(m~s^-1)),
    x = "Year",
    y = " ",
    color = "Season",
    shape = "Average Type"
  ) +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2008, 2021.7), expand =c(0.01, 0.5)) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.key.height = unit(10, "pt"),
    legend.key.spacing.y = unit(0.2, "pt"),
    legend.title = element_text(size=6, margin = margin(b =10)), #change legend title font size
    legend.text = element_text(size=6, margin = margin(l = 0)),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=0),
    strip.text = element_text(size=6, margin=margin(0,0,0,0)),
    strip.placement = "outside",
    axis.title=element_text(angle=0,vjust=1.04, size=6),
    axis.text = element_text(size=5),
    plot.title = element_text(size=7),
    plot.margin = unit(c(0,0,0,-2), "mm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1),
) +
  facet_wrap(~Station.name, scales="free_y",  ncol = 1, strip.position = "left")

p1_WS



df_mean.UB <- df_annual.mean |>
  filter(Station.name == 'UB') |>
  mutate(long_term_mean.VIS = long_term_mean.VIS*0.001)

df_combined_noSS <- df_combined |>
  filter(Station.name != 'SS') |>
  mutate(Seasonal_avg.VIS = Seasonal_avg.VIS*0.001,
         Annual_avg.VIS = Annual_avg.VIS*0.001,
         long_term_mean.VIS = long_term_mean.VIS*0.001)

p1_VIS <- ggplot(df_combined_noSS, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.VIS, Annual_avg.VIS), 
                                       color = Season, shape = Average_Type)) +
  geom_line(data = filter(df_combined_noSS, Average_Type == 'Seasonal'), size = geom_line_size_M, linetype = "dotted") +  # Seasonal averages
  geom_point(data = filter(df_combined_noSS, Average_Type == 'Seasonal' ), size = geom_point_size_M) +  # Seasonal points
  geom_line(data = filter(df_combined_noSS, Average_Type == 'Annual' ), size = geom_line_size_L, color = 'black') +  # Annual average
  geom_point(data = filter(df_combined_noSS, Average_Type == 'Annual'), pch = mean_pch, bg = mean_bg_color, col = "black", cex = cex_size) +  # Annual points
  # geom_smooth(alpha=0.2, data = filter(df_combined, Average_Type == 'Annual', Station.name == 'ZU'), method = "lm") +
  geom_hline(data = filter(df_combined_noSS), aes(yintercept = long_term_mean.VIS), linetype = "dotted", color = "black") +
  geom_hline(data = filter(df_combined, Station.name == 'UB'), aes(yintercept = 6), linewidth=0.25, color = "lightgray") +
  geom_vline(aes(xintercept = 2020.5), linetype = "twodash", color = "grey", linewidth = 0.1) +
  geom_text(data = filter(df_combined, Station.name == 'UB'),
            aes(x = 2020, y = 10),
            label = "12 km",
            # stat = "unique",
            size = 2, color = "lightgray") +
  geom_text(data = filter(df_combined, Station.name == 'UB'),
            x = 2020, y = 1 + df_mean.UB$long_term_mean.VIS,
            label = "16,614 km",
            # stat = "unique",
            size = 2, color = "lightgray") +
  scale_shape_manual(values = c(1, 16, 24)) +  # Different shapes for seasonal vs annual
  # scale_color_viridis(discrete = TRUE, option = "D",   begin = 0,
  #                     end = 1,
  #                     direction = 1)+
  scale_color_manual(values=seasonal_color) +
  #scale_fill_viridis(discrete = TRUE) +
  labs(title = bquote("VIS"~(km)), x = "Year", y = " ", color = "Season", shape = "Average Type") +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2008, 2021.5)) +
  #scale_y_continuous(limits = c(6000,20000)) +
  scale_y_reverse(limits = c(20, 6), expand=c(0,0)) +
  geom_text(data = filter(df_combined_noSS, Station.name == 'UB'),
            x = 2015, y = 10, label = "Winter visibility drops (post-2014)",
            # stat = "unique",
            size = 2, color = "blue", hjust = 0) +
  theme_bw() +
  theme(
    legend.key.height = unit(10, "pt"),
    legend.key.spacing.y = unit(0.2, "pt"),
    legend.title = element_text(size=6, margin = margin(b =10)), #change legend title font size
    legend.text = element_text(size=6, margin = margin(l = 0)),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=0),
    strip.text = element_text(size=6, margin=margin(0,0,0,0)),
    strip.placement = "outside",
    axis.title=element_text(angle=0,vjust=1.04, size=6),
    axis.text = element_text(size=5),
    plot.title = element_text(size=7),
    plot.margin = unit(c(0,0,0,0), "mm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1),
    legend.position = "none") +
  # annotate("text", x = rep(c(2015,0 ,0 ), 3), y = rep(c(15000, 0,0 ),3), label = "hghjg", color = "blue", size = 2, hjust = 0) +
  facet_wrap(~Station.name, scales="free_y",  ncol = 1, strip.position = "left") 
p1_VIS


  
                         

# Plot both seasonal and annual averages on the same plot
p1_pm10 <- ggplot(df_combined, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.pm10*1000, Annual_avg.pm10*1000), 
                                   color = Season, shape = Average_Type)) +
  # geom_rect(data = filter(df_combined,  Station.name == 'UB' ), aes(xmin = 2013.9, xmax = 2020, ymin = -Inf, ymax = Inf),
  #           alpha = 0.8,
  #           fill = "azure", show.legend = F, linewidth = 0) +
  # geom_rect(data = filter(df_combined,  Station.name == 'DZ' ), aes(xmin = 2013.9, xmax = 2020, ymin = -Inf, ymax = Inf),
  #           alpha = 0.8,
  #           fill = "azure2", show.legend = F, linewidth = 0) +
  geom_line(data = filter(df_combined, Average_Type == 'Annual' ), size = geom_line_size_L, color = 'black') +  # Annual average
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), pch = mean_pch, bg = mean_bg_color, col = "black", 
             cex = cex_size, show.legend = T) +  # Annual points
  geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = geom_line_size_M, linetype = "dashed") +  # Seasonal averages
  geom_point(data = filter(df_combined, Average_Type == 'Seasonal' ), size = geom_point_size_M) +  # Seasonal points
  geom_line(aes(x= Year, y = long_term_mean.pm10*1000), alpha=0.3, linetype="solid", linewidth = 0.3, color = "grey") +
  geom_line(aes(x= Year, y = long_term_mean.pm10*1000),  linetype="dotdash", linewidth = 0.3, color = "black") +
  # geom_text(data = filter(df_combined, Station.name == 'ZU'),
  #           x = 2011, y = 80, label = "High PM10 levels (ZU, 2011)",
  #           # stat = "unique",
  #           size = 2, color = "darkgreen", hjust = 0) +
  # geom_text(data = filter(df_combined, Station.name == 'ZU'),
  #           x = 2012.1, y = 57, label = "57",
  #           # stat = "unique",
  #           size = 2, color = "red", hjust = 0) +
  # geom_text(data = filter(df_combined, Station.name == 'ZU'),
  #           x = 2016.1, y = 70, label = "70",
  #           # stat = "unique",
  #           size = 2, color = "red", hjust = 0) +
  # geom_text(data = filter(df_combined, Station.name == 'SS'),
  #           x = 2011.1, y = 32, label = "32",
  #           # stat = "unique",
  #           size = 2, color = "red", hjust = 0) +
  geom_text_repel(data = filter(df_combined, Average_Type == 'Seasonal' & Station.name == 'ZU' & Year > 2008), aes(label = ifelse(Seasonal_avg.pm10*1000 > 50, round(Seasonal_avg.pm10*1000,1), ""), color = Season),
                  size = 2, 
                    # nudge_y = 0, Adjusts vertical position
                  hjust=-0.5,
                  nudge_y = 0,
                  segment.color = "gray50", 
                  segment.size = 0.5) +  # Dynamic labels
  geom_text_repel(data = filter(df_combined, Average_Type == 'Seasonal' & Station.name == 'SS' & Year > 2008), aes(label = ifelse(Seasonal_avg.pm10*1000 > 30, round(Seasonal_avg.pm10*1000,1), ""), color = Season),
                  size = 2, 
                  hjust=-0.5,  # Adjusts vertical position
                  nudge_y = 0,  # Adjusts vertical position
                  segment.color = "gray50", 
                  segment.size = 0.5) +  # Dynamic labels
#  geom_segment(aes(x=2020.3, y = I_5y_mean.pm10*1000, yend = II_5y_mean.pm10*1000), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_vline(aes(xintercept = 2020.5), linetype = "twodash", color = "grey", linewidth = 0.1) +
 # geom_segment(data=filter(df_combined, Average_Type == 'Annual'), aes(x=2020.3, y = I_5y_mean.pm10*1000, yend = II_5y_mean.pm10*1000), arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Winter' ), aes(x=2020.8, y = I_5y_mean.pm10*1000, yend = II_5y_mean.pm10*1000, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Spring' ), aes(x=2021.1, y = I_5y_mean.pm10*1000, yend = II_5y_mean.pm10*1000, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Summer' ), aes(x=2021.4, y = I_5y_mean.pm10*1000, yend = II_5y_mean.pm10*1000, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Autumn' ), aes(x=2021.7, y = I_5y_mean.pm10*1000, yend = II_5y_mean.pm10*1000, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +    
    scale_shape_manual(values = c(1, 16, 24)) +  # Different shapes for seasonal vs annual
  # scale_color_viridis(discrete = TRUE, option = "D",   begin = 0,
  #                     end = 1,
  #                     direction = 1)+
  scale_color_manual(values=seasonal_color) +
  #scale_fill_viridis(discrete = TRUE) +
  labs(title = bquote("PM10"~(mu*g~m^-3)), x = "Year", y = " ", color = "Season", shape = "Average Type") +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2008, 2021.7), expand =c(0.01, 0.5)) +
  theme_bw() +
  theme(
    legend.key.height = unit(10, "pt"),
    legend.key.spacing.y = unit(0.2, "pt"),
    legend.title = element_text(size=6, margin = margin(b =10)), #change legend title font size
    legend.text = element_text(size=6, margin = margin(l = 0)),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=0),
    strip.text = element_text(size=0, margin=margin(0,0,0,0)),
    strip.placement = "outside",
    axis.title=element_text(angle=0,vjust=1.04, size=6),
    axis.text = element_text(size=5),
    plot.title = element_text(size=7),
    plot.margin = unit(c(0,0,0,0), "mm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1),
    legend.position = "none"
  ) +
  facet_wrap(~Station.name, scales="free_y",  ncol = 1)
p1_pm10



# Plot both seasonal and annual averages on the same plot
p1_pm2.5 <- ggplot(df_combined, aes(x = Year, y = ifelse(Average_Type == 'Seasonal', Seasonal_avg.pm2.5*1000, Annual_avg.pm2.5*1000), 
                                    color = Season, shape = Average_Type)) +
  # geom_rect(data = filter(df_combined,  Station.name == 'UB' ), aes(xmin = 2013.9, xmax = 2020, ymin = -Inf, ymax = Inf),
  #           alpha = 0.8,
  #           fill = "azure", show.legend = F, linewidth = 0) +
  # geom_rect(data = filter(df_combined,  Station.name == 'DZ' ), aes(xmin = 2013.9, xmax = 2020, ymin = -Inf, ymax = Inf),
  #           alpha = 0.8,
  #           fill = "azure2", show.legend = F, linewidth = 0) +
  # geom_text(data = filter(df_combined, Station.name == 'SS'),
  #           x = 2011.1, y = 12, label = "12",
  #           # stat = "unique",
  #           size = 2, color = "red", hjust = 0) +
  geom_line(data = filter(df_combined, Average_Type == 'Annual' ), size = geom_line_size_L, color = 'black') +  # Annual average
  geom_point(data = filter(df_combined, Average_Type == 'Annual'), pch = mean_pch, bg = mean_bg_color, col = "black", 
             cex = cex_size, show.legend = T) +  # Annual points
  geom_line(data = filter(df_combined, Average_Type == 'Seasonal'), size = geom_line_size_M, linetype = "dashed") +  # Seasonal averages
  #geom_line(data = filter(df_combined, Average_Type == 'Seasonal' & Station.name == 'UB' & Season == 'Summer' & Year >2013), size = geom_line_size_M) +  # Seasonal averages
  geom_point(data = filter(df_combined, Average_Type == 'Seasonal' ), size = geom_point_size_M) +  # Seasonal points
  geom_line(aes(x= Year, y = long_term_mean.pm2.5*1000), alpha=0.3, linetype="solid", linewidth = 0.3, color = "grey") +
  geom_line(aes(x= Year, y = long_term_mean.pm2.5*1000),  linetype="dotdash", linewidth = 0.3, color = "black") +
  geom_vline(aes(xintercept = 2020.5), linetype = "twodash", color = "grey", linewidth = 0.1) +
 # geom_segment(data=filter(df_combined, Average_Type == 'Annual'), aes(x=2020.3, y = I_5y_mean.pm2.5*1000, yend = II_5y_mean.pm2.5*1000), arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Winter'), aes(x=2020.8, y = I_5y_mean.pm2.5*1000, yend = II_5y_mean.pm2.5*1000, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Spring'), aes(x=2021.1, y = I_5y_mean.pm2.5*1000, yend = II_5y_mean.pm2.5*1000, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Summer'), aes(x=2021.4, y = I_5y_mean.pm2.5*1000, yend = II_5y_mean.pm2.5*1000, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +  
  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]' & Season == 'Autumn'), aes(x=2021.7, y = I_5y_mean.pm2.5*1000, yend = II_5y_mean.pm2.5*1000, color = Season), arrow = arrow(length = unit(0.1, "cm"), type="closed")) +    
  geom_text_repel(data = filter(df_combined, Average_Type == 'Seasonal' & Station.name == 'ZU' & Year > 2008), aes(label = ifelse(Seasonal_avg.pm2.5*1000 > 30, round(Seasonal_avg.pm2.5*1000,1), ""), color = Season),
                  size = 2, 
                  nudge_x = 0.5,  # Adjusts vertical position
                  segment.color = "gray50", 
                  segment.size = 0.5) +  # Dynamic labels
  geom_text_repel(data = filter(df_combined, Average_Type == 'Seasonal' & Station.name == 'SS' & Year > 2008), aes(label = ifelse(Seasonal_avg.pm2.5*1000 > 10, round(Seasonal_avg.pm2.5*1000,1), ""), color = Season),
                  size = 2, 
                  nudge_x = 0.5,  # Adjusts vertical position
                  segment.color = "gray50", 
                  segment.size = 0.5) +  # Dynamic labels
  #  geom_vline( data = filter(df_combined, Average_Type == 'Annual' ),
 #             aes(xintercept = 2013.5), linetype = "dotted", color = "black") +
  scale_shape_manual(values = c(1, 16, 17)) +  # Different shapes for seasonal vs annual
  # scale_color_viridis(discrete = TRUE, option = "D",   begin = 0,
  #                     end = 1,
  #                     direction = 1)+
  scale_color_manual(values=seasonal_color) +
  #scale_fill_viridis(discrete = TRUE) +
  labs(title = bquote("PM2.5"~(mu*g~m^-3)), x = "Year", y = " ", color = "Season", shape = "Average Type") +
  scale_x_continuous(breaks = seq(min(df_combined$Year), max(df_combined$Year), 3), limits = c(2008, 2021.7), expand = c(0.01, 0.5)) +
  theme_bw() +
  theme(
    legend.key.height = unit(10, "pt"),
    legend.key.spacing.y = unit(0.2, "pt"),
    legend.title = element_text(size=6, margin = margin(b =10)), #change legend title font size
    legend.text = element_text(size=6, margin = margin(l = 0)),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle=0),
    strip.text = element_text(size=0, margin=margin(0,0,0,0)),
    strip.placement = "outside",
    axis.title=element_text(angle=0,vjust=1.04, size=6),
    axis.text = element_text(size=5),
    plot.title = element_text(size=7),
    plot.margin = unit(c(0,0,0,0), "mm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1),
    legend.position = "none") +
  facet_wrap(~Station.name, scales="free_y", strip.position = "right",  ncol = 1)
p1_pm2.5




library(ggtext)
library(gridtext)

title_text <- glue::glue(
  'In the period of 2008–2020 with 4 seasons: ',
  '<span style = "color:{seasonal_color["Winter"]}"> "**Winter**"</span>, '
  , 
  '<span style = "color:{seasonal_color["Spring"]}"> "**Spring**"</span>, ', 
  '<span style = "color:{seasonal_color["Summer"]}"> "**Summer**"</span> ',
  'and ', 
  '<span style = "color:{seasonal_color["Autumn"]}"> "**Autumn**"</span> ' 
)


p_combined <- p1_ratio+ 
  labs(
    title = "Annual and seasonal trends in WS, VIS, PM10, PM2.5, and r-Ratio at the study sites",
    subtitle = title_text,
    caption = "Note: Visibility data unavailable for SS."
  ) +
  plot_layout(guides = "collect", nrow=1,) & theme(legend.position = "bottom")


patchwork0 <- p_combined + labs(
  title = "Annual and seasonal trends in WS, VIS, PM10, PM2.5, and r-Ratio at the study sites (2008–2020)",
  subtitle = title_text,
  caption = "Note: Visibility data unavailable for SS."
) +
  theme(
        plot.subtitle = ggtext::element_markdown(size=7, lineheight =1),
        plot.title.position = 'plot', 
        text = element_text(colour = 'gray20'),
        plot.title=element_markdown(size=6, margin=margin(0,0,0,0)),
        legend.position = "top",
          legend.key.height = unit(10, "pt"),
          legend.key.spacing.y = unit(0.2, "pt"),
          legend.title = element_text(size=6, margin = margin(b =10)), #change legend title font size
          legend.text = element_text(size=6, margin = margin(l = 0)),
        )



title <- get_subtitle(patchwork0)

legend_b <- get_legend(
  patchwork0 +
    theme(legend.box.background = "white", legend.title = element_text(size=6, margin = margin(b =10)),
      legend.position = "bottom",
          legend.direction = "vertical")
                      )
season5 <- c('#a6611a','#dfc27d', '#80cdc1','#018571')


legend_plot <- ggplot(df_combined, aes(color = Season, shape = Average_Type, fill = "Autumn")) +
  geom_point(aes(y = 1, x = Season), size = 4) +
  scale_color_manual(name = "Season", values = seasonal_color) +
#  geom_segment(data=filter(df_combined, Average_Type == 'pre - post [2014]'), aes(x=1, y = 1, yend = 2, color = Season), arrow = arrow(length = unit(0.1, "cm")), show.legend = T) +  
  swimmer_arrows(Average_Type == 'pre - post [2014]', arrow_start=1,
                 cont = 'Continued_treatment',name_col='Arm',type =
                   "open",cex=1) +
   geom_point(aes(y = 2, x = Average_Type), size = 4) +
  geom_point(data=filter(df_combined, Average_Type == 'Annual' ), aes(x=1, y = 1), pch =0 ) +  
  theme_void() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))
legend_plot

library(cowplot)
plot_20250129legend <- wrap_plots(p1_WS, p1_pm10, p1_pm2.5, p1_ratio, nrow = 1)
ggsave("visuals/fig3_3_trends_seasons_0131_legend.png",plot_20250129legend, width = 8, height = 4, units = "in", dpi = 300)


plot <- plot_grid(plot_20250129legend, legend_b, ncol = 1, nrow = 2, rel_heights = c(0.95, 0.1))
ggsave("visuals/fig3_3_trends_seasons_0129legend_added.png", plot, width = 6, height = 3, units = "in", dpi = 300)

plot <- p1_WS+ p1_pm10+ p1_pm2.5+p1_ratio + plot_layout(nrow = 1, ncol = 4, guides = "collect", theme(legend.position = "bottom"))


################## time-lag, seasonal cycle figure
### data 
time_lag_df <- merged_seasonal_df |>
  select(1:9)

write.csv(time_lag_df, "data/processed/df_time_lag.csv")
time_lag_df |>
  filter(Season == "Winter"   |  Season == "Spring" ) |>
  ggplot( aes(x=Seasonal_avg.pm2.5, y=Seasonal_avg.r, color = Season)) +
  geom_point() +
  geom_line(aes(x=Year-1, y=Seasonal_avg.pm2.5 , color = Season)) +
  geom_smooth() +
  facet_grid(Season~Station.name)


qplot(x=time_lag_df$Seasonal_avg.pm2.5,y=c(tail(x,-2),0,0))

Seasonal_avg.pm2.5*1000, Annual_avg.pm2.5*1000 long_term_mean.pm2.5*1000

value <- df_combined$long_term_mean.pm2.5
# (c) Quad plot
recent_avg <- value[31:41]  # Last 10 years' average
trend <- diff(df_combined$long_term_mean.pm2.5[11:20])        # Example trend calculation
quad_data <- data.frame(
  recent_avg = scale(df_combined$long_term_mean.pm2.5[11:20]),
  trend = scale(diff(df_combined$long_term_mean.pm2.5[11:20])
)


ggplot(df_combined, aes(x = scale(df_combined$long_term_mean.pm2.5[11:20], 
                                            y = scale(diff(df_combined$long_term_mean.pm2.5[11:20]), 
                                                      colour=scale(df_combined$long_term_mean.pm2.5[11:20]))))) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  facet_wrap(~Station.name)
  labs(title = "(c) Quad plot", x = "Recent Average (z-score)", y = "Trend (z-score)")























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








