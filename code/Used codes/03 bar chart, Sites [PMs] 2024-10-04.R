library(ggplot2)
library(ggbreak)      # scale_y_break


p3_spatial <- data |>
  ggplot(aes(pm2.5*1000, pm10*1000, color=Station.name, shape = Station.name)) +
  geom_point(position = "jitter") +
  labs(x = "PM2.5", y = "PM10") +
#  scale_y_continuous(breaks = seq(50,800,100), limits = c(0,800), sec.axis = dup_axis(labels = NULL, breaks=NULL)) +
    scale_y_break(c(4000,6000), scale= 0.18, space = 0.01, ticklabels = c(seq(500,1000,1000), 6000),
    ) 

    stat_ellipse()
  geom_abline(slope = 1, intercept = 0)
  


library(ggsignif)
p3 <- daily_data |>
  ggplot(aes(Station.name, pm10*1000)) +
  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.3)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  labs(x = "Sites", y = "PM10") +
  geom_signif(comparisons = list(c("UB", "DZ"),
                               c("UB", "ZU"),
                              # c("UB", "SS"),
                               c("DZ", "ZU"),
                              # c("DZ", "SS"),
                               c("ZU", "SS")),
            test = "wilcox.test", step_increase = 0.105,
            map_signif_level = T, tip_length = 0.01, 
          margin_top = 0.18, 
            alpha = 0.5) +
  ylim(NA, 990)
p3


p4 <- daily_data |>
  ggplot(aes(Station.name, pm2.5*1000)) +
  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.3)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  labs(x = "Sites", y = "PM2.5") +
  geom_signif(comparisons = list(c("UB", "DZ"),
                               #  c("UB", "ZU"),
                               #  c("UB", "SS"),
                                 c("DZ", "ZU"),
                               #  c("DZ", "SS"),
                                 c("ZU", "SS")),
              test = "wilcox.test", step_increase = 0.075,
              map_signif_level = TRUE, tip_length = 0.03, 
              margin_top = 0.18, alpha = 0.5) +
  ylim(NA, 750)
p4


######################  3. Theme background
p3 <- p3 + cowplot::theme_cowplot() +
  theme(#legend.position = c(.73, .70),
    legend.position="none",
    legend.title = element_text(size=9), #change legend title font size
    legend.text = element_text(size=7),
    axis.title.y = element_text(size=9),
  #  axis.title.x = element_text(size=9),
    axis.text = element_text(size = 8),
    axis.text.x = element_blank(),
  #  axis.text.y = element_blank(),
    axis.ticks.x=element_blank(),
    # axis.ticks.y=element_blank(),  #remove y axis ticks
    axis.title.x = element_blank(),
  #  axis.title.y = element_blank(),
    # panel.grid.major = element_line(color = "grey", 
    #    linetype = "dashed",
    #     size = .05),
   #  aspect.ratio = 1.2,
    panel.grid.minor = element_blank(),
    panel.ontop = F,
    panel.background = element_rect(fill = NA, colour = 'black', size = 0.7))
p3
p4 <- p4 + cowplot::theme_cowplot() +
  theme(#legend.position = c(.73, .70),
    legend.position="none",
   # legend.title = element_text(size=9), #change legend title font size
   #legend.text = element_text(size=7),
    axis.title.y = element_text(size=9),
    axis.title.x = element_text(size=9),
    axis.text = element_text(size = 8),
  #  axis.text.x = element_blank(),
  #  axis.text.y = element_blank(),
  #  axis.ticks.x=element_blank(),
    # axis.ticks.y=element_blank(),  #remove y axis ticks
  #  axis.title.x = element_blank(),
  #  axis.title.y = element_blank(),
    # panel.grid.major = element_line(color = "grey", 
    #    linetype = "dashed",
    #     size = .05),
  #  plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
  # aspect.ratio = 1.2,
    panel.grid.minor = element_blank(),
    panel.ontop = F,
    panel.background = element_rect(fill = NA, colour = 'black', size = 0.7))
p4
###################### 1. Points Color, Shape, and Size 
library(hrbrthemes)
library(viridis)
library(ggbreak) # scale_y_break(c(130, 1000)), scale_x_break(c(130, 1000))
# scale_y_continuous(limits = c(0, 300)) +
p4 <- p4 + scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_shape_manual(values=c(8, 1, 0, 2))+
  scale_color_manual(values=c('#937f52','#c4b485', '#b88d2d','#dc9b09'))+
  #  scale_color_manual(values=c('#fee9b9','#c4b485', '#eca300','#999999'))+
  scale_size_manual(values=c(2,2,2,2)) 


p3_spatial <- p3_spatial + cowplot::theme_cowplot() +
  theme(#legend.position = c(.73, .70),
    legend.position="none",
    legend.title = element_text(size=9), #change legend title font size
    legend.text = element_text(size=7),
    axis.title.y = element_text(size=9),
    axis.title.x = element_text(size=9),
    axis.text = element_text(size = 8),
  #  axis.text.x = element_blank(),
   # axis.text.y = element_blank(),
   # axis.ticks.x=element_blank(),
  #  axis.ticks.y=element_blank(),  #remove y axis ticks
   # axis.title.x = element_blank(),
  #  axis.title.y = element_blank(),
    # panel.grid.major = element_line(color = "grey", 
    #    linetype = "dashed",
    #     size = .05),
    panel.grid.minor = element_blank(),
    panel.ontop = F,
    panel.background = element_rect(fill = NA, colour = 'black', size = 0.7))

  
p3_spatial <- p3_spatial + scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_shape_manual(values=c(8, 1, 0, 2))+
  scale_color_manual(values=c('#937f52','#c4b485', '#b88d2d','#dc9b09'))+
  #  scale_color_manual(values=c('#fee9b9','#c4b485', '#eca300','#999999'))+
  scale_size_manual(values=c(2.8,2.8,2.8,2.8)) 

