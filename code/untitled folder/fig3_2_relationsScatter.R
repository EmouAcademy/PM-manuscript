##### use Hourly data
#### data
library(hrbrthemes)

p3_2_scatter <- data |>
  ggplot(aes(pm2.5*1000, pm10*1000, size = WS, shape = Station.name,  color = Visibility)) +
  geom_point() +
  facet_wrap(~Station.name, strip.position="left", scales = 'free') +
  scale_shape_manual(values=c(8, 1, 0, 2)) +
#  scale_color_continuous(trans = 'reverse') +
 # guides(size = guide_legend(ncol = 2)) +
  labs(x = "PM2.5", y = "PM10", shape = " ",  size = "WS (m/s)", color="VIS (km)") +
  theme_minimal() +
  theme_ipsum() +
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
 strip.text.y.left = element_text(angle=0),
 strip.placement = "outside",
   # strip.text.x = element_blank(),
    axis.title.y = element_text(size=12),
    plot.title = element_text(size=5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey", linetype = "dashed", size = .1),
    panel.background = element_rect(fill = NA, colour = 'white', size = 0.7)
  )  

p3_2_scatter <- p3_2_scatter + guides(shape = guide_legend(position = "top", 
                                                           legend.title = element_text(size=10, margin = margin(b = 0))
                                                           ),
                                      #reverse size order (higher diameter on top) 
                                     size = guide_legend(order = 1))
                                        #reverse color order (higher value on top)
                                    #    color = guide_colorbar(reverse = TRUE)
                                      

p3_2_scatter

#################### shape with Month

p3_2_scatter <- data |>
  mutate(Season = case_when(
    Month %in% 1:2 ~ 'NOV-FEB',
    Month %in% 3:6 ~ 'MAR-JUN',
    Month %in% 7:10 ~ 'JUL-OCT',
    Month %in% 11:12 ~ 'NOV-FEB')) |>
  ggplot(aes(pm2.5*1000, pm10*1000, size = WS, shape = as.factor(Season),  color = Visibility)) +
  geom_point() +
  geom_abline(linetype="dashed") +
  facet_wrap(~Station.name, strip.position="left", scales = 'free') +
  scale_shape_manual(values=c(2, 1,  3)) +
  #  scale_color_continuous(trans = 'reverse') +
  # guides(size = guide_legend(ncol = 2)) +
  labs(x = "PM2.5", y = "PM10", shape = " ",  size = "WS (m/s)", color="VIS (km)") +
  theme_minimal() +
  theme_ipsum() +
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
    strip.text.y.left = element_text(angle=0),
    strip.placement = "outside",
    # strip.text.x = element_blank(),
    axis.title.y = element_text(size=12),
    plot.title = element_text(size=5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dashed", size = .1),
    panel.background = element_rect(fill = NA, colour = 'white', size = 0.7)
  )  

p3_2_scatter <- p3_2_scatter + guides(shape = guide_legend(position = "top", reverse = T, 
                              legend.title = element_text(size=0, margin = margin(b = 0)),
),
#reverse size order (higher diameter on top) 
size = guide_legend(order = -1))
#reverse color order (higher value on top)
#    color = guide_colorbar(reverse = TRUE)


p3_2_scatter 

