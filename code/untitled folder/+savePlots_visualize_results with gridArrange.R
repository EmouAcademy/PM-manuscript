###################### Layout
#### Fig3-1
setwd("~/WORK/Research/my-awesome-manuscripts/images")
library(gridExtra)
#______________________________Results 3.1 Spatio-temporal variations______________________________
#
#### Fig3-1_spatialVariations
fig3_1_01 <- grid.arrange(p3, p4, ncol = 1, heights=c(1.3,1))
ggsave("fig3_1_spatialVariations_a.png", fig3_1_01, width=4, height=5, unit="in", dpi = 300)
fig3_1_01 <- grid.arrange(fig3_1_01, p3_spatial, ncol = 2)
ggsave("fig3_1_spatialVariations_ab.png", fig3_1_01, width=8, height=5, unit="in", dpi = 300)
ggsave("fig3_1_spatialVariations_b.png", p3_spatial, width=4, height=5, unit="in", dpi = 300)



grid.arrange(arrangeGrob(p3,p4, ncol=1, nrow=2, heights=c(2,1.9)),
             arrangeGrob(p3_spatial, ncol=1, nrow=1), heights=c(6,1), widths=c(1.5,1))



#### Fig3-1_monthly_annualVariations
fig3_1_02 <- grid.arrange(p3_annual_01, p3_annual_02, ncol = 2, widths = c(1,1))
fig3_1_02 <- arrangeGrob(p3_annual_01, p3_annual_02, ncol = 2, widths = c(1,1))
ggsave("fig3_1_monthly_annualVariations.png", fig3_1_02, width=8, height=5, unit="in", dpi = 300)

#### Fig3-1_dailyVariations
ggsave("fig3_1_dailyVariations.png", p3_daily_01, width=8, height=5, unit="in", dpi = 300)
ggsave("fig3_1_dailyVariations_01a.png", p3_daily_02, width=8, height=5, unit="in", dpi = 300)

#______________________________Results 3.2 Relations between PM and WS, Vis, among seasonal___________________
# 
#### Fig3-2_ratioHeatMap
fig3_2_01 <- grid.arrange(p1_01, p3, p2_01, p4, ncol = 2, widths = c(3,1))
ggsave("fig3_2_ratioHeatMap.png", fig3_2_01, width=8, height=5, unit="in", dpi = 300)

setwd("~/WORK/Research/Data/1 Projects/03 tbl & fig, XYZ for manuscript")
#### Fig3-2_relationScatter
#fig3_2_02 <- grid.arrange(p3_annual_01, p3_annual_02, ncol = 2, widths = c(1,1))
#fig3_2_02 <- arrangeGrob(p3_annual_01, p3_annual_02, ncol = 2, widths = c(1,1))
ggsave("fig3_2_relationScatter-2024-10-10.png", p3_2_scatter, width=8, height=7, unit="in", dpi = 300)

#### Fig3-2_relationPCA
ggsave("fig3_2_relationPCA.png", p3_daily_01, width=8, height=5, unit="in", dpi = 300)
ggsave("fig3_3_relationPCA_01a.png", p3_daily_02, width=5, height=4.5, unit="in", dpi = 300)


#______________________________Results 3.3 Trends and Changes ___________________
# 
#### Fig3-3_annualTrendChanges
fig3_3_01 <- grid.arrange(p1_01, p3, p2_01, p4, ncol = 2, widths = c(3,1))
ggsave("fig3_3_annualTrendChanges.png", fig3_3_01, width=8, height=5, unit="in", dpi = 300)

#### Fig3-3_dailyTrendChanges
fig3_3_02 <- grid.arrange(p3_annual_01, p3_annual_02, ncol = 2, widths = c(1,1))
fig3_3_02 <- arrangeGrob(p3_annual_01, p3_annual_02, ncol = 2, widths = c(1,1))
ggsave("fig3_3_dailyTrendChanges.png", fig3_3_02, width=8, height=5, unit="in", dpi = 300)


#___________________________________________________________________________________

grid.arrange(fig3_2a, fig3_2b, ncol = 2, widths = c(1,1))


grid.arrange(daily_UB_DZa, daily_UB_DZb, ncol = 2, widths = c(1,1))


######################  3. Theme background
fig3_2b <- fig3_2b + cowplot::theme_cowplot() +
  theme(#legend.position = c(.73, .70),
    legend.position="none",
    legend.title = element_text(size=9), #change legend title font size
    legend.text = element_text(size=7),
    axis.title.y = element_text(size=9),
      axis.title.x = element_text(size=9),
    axis.text = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x=element_blank(),
    # axis.ticks.y=element_blank(),  #remove y axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # panel.grid.major = element_line(color = "grey", 
    #    linetype = "dashed",
    #     size = .05),
    panel.grid.minor = element_blank(),
    panel.ontop = F,
    panel.background = element_rect(fill = NA, colour = 'black', size = 0.7))


###################### 1. Points Color, Shape, and Size 
library(hrbrthemes)
library(viridis)
library(ggbreak) # scale_y_break(c(130, 1000)), scale_x_break(c(130, 1000))
# scale_y_continuous(limits = c(0, 300)) +
fig3_2b <- fig3_2b + scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_shape_manual(values=c(8, 1, 0, 2))+
  scale_color_manual(values=c('#937f52','#b88d2d', '#c4b485','#dc9b09'))+
  #  scale_color_manual(values=c('#fee9b9','#c4b485', '#eca300','#999999'))+
  scale_size_manual(values=c(2,2,2,2)) 









