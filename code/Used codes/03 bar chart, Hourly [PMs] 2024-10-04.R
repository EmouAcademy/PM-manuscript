#### Daily variations by GGPLOT2
fig3_2a_hmUB <- data |>
  filter(Station.name=='UB' ) |>
#  group_by(Station.name, Hour) |>
  ggplot(aes(Hour, pm10*1000, group = Hour)) +
  #  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.1)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
   facet_wrap(~Station.name, scales = 'free_y') +
  labs(x = "Month", y = "PM10")

data_ub_dz <- data |>
  filter(Station.name %in%  c('UB', 'DZ'))

fig3_2a_hmDZ <- data_ub_dz |>
 # filter(Station.name %in%  c('UB', 'DZ')) +
 # filter(Station.name=='DZ' ) |>
  ggplot(aes(Hour, pm2.5*1000, group = Hour)) +
  #  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.1)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  facet_wrap(~Station.name, scales = 'free_y') +
  labs(x = "Month", y = "PM10")

p <- fig3_2a_hmDZ 

fig3_2b_hm <- data |>
  group_by(Station.name, Hour) |>
  ggplot(aes(Hour, pm2.5*1000, group=Hour)) +
  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.1)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  facet_wrap(~Station.name, scales = 'free_y', ncol=1) +
  labs(x = "Month", y = "PM2.5")

########## "day: HOUR"


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

########## "day: WEEK"
weekly_UB_DZa <- df_01Qd |>
  fill_gaps()  |>
  group_by(Quarter) |>
  gg_season(pm2.5*1000, period = "week") +
  geom_line() +
  labs(y = "PM2.5 (ug m3)", x = "Weekday", 
       title = "Weekly variations: PM2.5 concentrations")
weekly_UB_DZb <- df_01Qd |>
  fill_gaps()  |>
  group_by(Quarter) |>
  gg_season(pm10*1000, period = "week") +
  geom_line() +
  labs(y = "PM10 (ug m3)", x = "Weekday ", 
       title = "Weekly variations: PM10 concentrations")

#############################
##############
df_01Q |>
  filter(Station.name=='UB' ) |>
  fill_gaps()  |>
  gg_subseries(pm10, period = "year") +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

weekly_UB_DZaq <- df_01Qq |>
  fill_gaps()  |>
  group_by(date) |>
  gg_season(pm2.5*1000, period = "quarter") +
  geom_line() +
  labs(y = "PM2.5 (ug m3)", x = "Weekday", 
       title = "Weekly variations: PM2.5 concentrations")
weekly_UB_DZbq <- df_01Qq |>
  fill_gaps()  |>
  group_by(Quarter) |>
  gg_season(pm10*1000, period = "week") +
  geom_line() +
  labs(y = "PM10 (ug m3)", x = "Weekday ", 
       title = "Weekly variations: PM10 concentrations")



################
###############
###############
position_jitterDaily <- position_jitterdodge(
  jitter.width = NULL,
  jitter.height = 0,
  dodge.width = 0.75,
  seed = NA
)

p3_daily_01 <- data_ub_dz |>
  ggplot(aes(Hour, pm10*1000)) +
geom_bar(stat = 'summary', position=position_dodge2(padding = 0.3), alpha=0)+
  geom_boxplot(aes(Hour, pm10*1000, group = Hour, color='PM10'), position= position_nudge(x = -.2), alpha=0.4, width=0.3, outlier.size=0) +
  geom_boxplot(aes(Hour, pm2.5*1000, group = Hour,color='PM2.5'),  position= position_nudge(x=.2), width=0.3, alpha=0.4, outlier.size=0) +
  # geom_violin(aes(Hour, pm10*1000, group = Month, fill=pm2.5*1000),  position= position_nudge(x=.2), width=0.35, size=0.3) +
  labs(x = "Time (UTC)", y = "Daily variations of PM10 and PM2.5") +
  theme_bw()+
  #  theme(axis.text.x=element_text(angle = 90)) +
  #  geom_jitter(aes(shape=Station.name, color=Station.name), width = 0, height = 0)+
  # coord_flip() +
  scale_color_manual(name='',
                     breaks=c('PM10', 'PM2.5'),
                     values=c('PM2.5'='lightblue1', 'PM10'='grey4'))+
  facet_wrap(~Station.name,  scales = 'free_y', ncol=1,  strip.position="right") +
#coord_cartesian(ylim = c(0, 1000)) +
  scale_x_discrete(limits= c(0:23), breaks=c(0,3,6,9,12,15,18,21)) +
#  scale_y_continuous(breaks = c(50,100, 200)) +
  scale_y_continuous(breaks = seq(50,800,100), limits = c(0,800), sec.axis = dup_axis(labels = NULL, breaks=NULL)) +
  scale_y_break(c(180,700), scale= 0.18, space = 0.4, ticklabels = c(seq(50,800,350), 700),
               ) +
  theme(#strip.text.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.title.y.left = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) 

p3_daily_01 <- p3_daily_01 +
theme(
legend.text = element_text(size=7),
#legend.title.align = 0,
#legend.title = element_text(size=7, vjust = .5, hjust = .1),
legend.position = "top", 
legend.justification = "right",
legend.key.height = unit(0.3, 'cm'), #change legend key height
legend.key.width = unit(0.2, 'cm'),
legend.margin=margin(b = 0, unit='cm'),
legend.direction = "horizontal")

p3_daily_01 <- p3_daily_01 +
 ggtitle("Daily variations of PM10 and PM2.5 concentrations")+
  annotate("text", x = 0, y = 0, label = "8am",size = 2) +
 annotate("text", x = 6, y = 0, label = "2pm", size = 2) +
  annotate("text", x = 12, y = 0, label = "8pm", size = 2) +
  annotate("text", x = 18, y = 0, label = "2am", size = 2) +
  annotate("text", x = 23, y = 0, label = "7am", size = 2) +
  theme(plot.title = element_text(hjust=0.5, size=10),
        axis.scale.y.left = element_blank())

p3_daily_01 
  grid.text("- Extreme outliers", x = 0.16, y = 0.875, gp=gpar(fontsize=5.5)) 
  grid.text("- Box plots", x = 0.14, y = 0.73, gp=gpar(fontsize=5.5))
  ggsave("fig3_2_monthly_01.png", fig3_2_01, width=8, height=5, unit="in", dpi = 300)
  
  grid.text("label", x = 0.2, y = 0.78) 

###########
p3_daily_02 <- data_ub_dz |>
  ggplot(aes(Hour, pm2.5*1000)) +
  #  geom_bar(stat = 'summary', position=position_dodge(width=0.9), alpha=0.5)+
  geom_boxplot(aes(Hour, pm2.5*1000, group = Hour), width=0.3, outlier.size=0) +
  # geom_violin(aes(Hour, pm10*1000, group = Month, fill=pm2.5*1000),  position= position_nudge(x=.2), width=0.35, size=0.3) +
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
  scale_x_discrete(limits= c(0:23)) +
  scale_y_continuous(breaks = c(50,100, 200)) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800)) +
  scale_y_break(c(120,700), scale= 0.18, space = 0.4, ticklabels = c(seq(50,800,350), 700),
                expand = c(0, 0))

p3_daily_02 <- p3_daily_02 +
  ggtitle("PM2.5")+
  theme(plot.title = element_text(hjust=0.5, size=10))



scale_y_continuous(breaks = seq(0,250,20), limits = c(0,250)) +
  scale_y_break(c(60,240), space = 0.4, ticklabels = c(seq(0,250,20), 250),
                expand = c(0, 0))
library(ggh4x)
  p3_daily_01 + ggh4x::facetted_pos_scales(y = list(
    Station.name == "UB" ~  scale_y_continuous(breaks = seq(100,800,200), limits = c(0,200)), 
    Station.name == "DZ" ~  scale_y_continuous(limits = c(0, 200), breaks = c(50, 100, 100))
   # cyl == 8 ~ scale_y_continuous(trans = "reverse")
  ))

  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x)))