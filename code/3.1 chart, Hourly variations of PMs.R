library(ggplot2)
library(readr)
library(ggbreak)      # scale_y_break
source(here("code/Colors.R"))

####### DATA 
data <- read.csv("data/processed/df.csv")

## add order and levels
data$Station.name <- factor(data$Station.name, 
                            levels = c("UB", "DZ",  "ZU", "SS"))  
levels(data$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')
#### hourly data for UB&DZ

data_ub_dz <- data |>
  filter(Station.name %in%  c('UB', 'DZ'))

########################### Plotting

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

ggsave("visuals/fig3_2_hourly.png", p3_daily_01, width=8, height=5, unit="in", dpi = 300)
