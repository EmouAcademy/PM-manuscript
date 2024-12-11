######################  3. Theme background
p1_01 <- p1_01 + cowplot::theme_cowplot() +
  theme(#legend.position = c(.73, .70),
    legend.position="none",
    legend.title = element_text(size=9), #change legend title font size
    legend.text = element_text(size=7),
    axis.title.y = element_text(size=12),
    #  axis.title.x = element_text(size=12),
    axis.text = element_text(size = 8),
    axis.text.x = element_blank(),
    #   axis.text.y = element_blank(),
    axis.ticks.x=element_blank(),
    # axis.ticks.y=element_blank(),  #remove y axis ticks
    axis.title.x = element_blank(),
    #  axis.title.y = element_blank(),
    # panel.grid.major = element_line(color = "grey", 
    #    linetype = "dashed",
    #     size = .05),
    panel.grid.minor = element_blank(),
    panel.ontop = F,
    panel.background = element_rect(fill = NA, colour = 'black', size = 0.7))

###################### 1. Scale of x as a date
p1_01 <- p1_01 + 
  scale_x_date(limits=c(as.Date("2008-01-01"), as.Date("2020-08-31")), 
               date_labels = "%Y", date_breaks = "12 month")  #"%d/%m/%Y", "%b %Y"


###################### 1. Points Color, Shape, and Size 
library(hrbrthemes)
library(viridis)
library(ggbreak) # scale_y_break(c(130, 1000)), scale_x_break(c(130, 1000))
# scale_y_continuous(limits = c(0, 300)) +
p1_01 <- p1_01 + scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_shape_manual(values=c(8, 1, 0, 2))+
  scale_color_manual(values=c('#937f52','#b88d2d', '#c4b485','#dc9b09'))+
  #  scale_color_manual(values=c('#fee9b9','#c4b485', '#eca300','#999999'))+
  scale_size_manual(values=c(2,2,2,2)) 

p1_01
