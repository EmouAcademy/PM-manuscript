#                             UB, DZ, SS, ZU
# scale_shape_manual(values=c(8, 1, 0, 2))+
#  scale_color_manual(values=c('#937f52','#b88d2d', '#c4b485','#dc9b09'
library(hrbrthemes)
library(viridis)
f3_2_01a <- daily_data %>% 
  filter(Station.name == 'UB') %>% 
  plot_ly(x = ~Month, y = ~ pm10, 
          type = "box", 
        #  color = ~Month, 
          color = I("#937f52"),
          symbol = I("8"),
          boxpoints = "all",
          jitter = 0.3,
          pointpos = -1.8)

f3_2_01a_hm <- df_01d %>% 
  filter(Station.name == 'UB') %>% 
  plot_ly(x = ~Hour, y = ~ pm10, 
     type = "bar")


f3_2_02a <- daily_data %>% 
  filter(Station.name == 'DZ') %>% 
  plot_ly(x = ~Month, y = ~ pm10, 
          type = "box", 
         # color = ~Month, 
          color = I("#b88d2d"),
          symbol = I("1"),
          boxpoints = "all", 
          jitter = 0.3,
          pointpos = -1.8)

f3_2_03a <- daily_data %>% 
  filter(Station.name == 'SS') %>% 
  plot_ly(x = ~Month, y = ~ pm10, 
          type = "box", 
          color = I("#c4b485"), 
          fillcolor = col2rgb("#c4b485"),
          symbol = I("0"),
          boxpoints = "none", 
          jitter = 0.3,
          pointpos = -1.8)

f3_2_04a <- daily_data %>% 
  filter(Station.name == 'ZU') %>% 
  plot_ly(x = ~Month, y = ~ pm10, 
          type = "box", 
          #color = ~Month, 
          color = I("#dc9b09"),
          symbol = I("2"),
          boxpoints = "all", 
          jitter = 0.3,
          pointpos = -1.8)


### pm2.5

f3_2_01b <- daily_data %>% 
  filter(Station.name == 'UB') %>% 
  plot_ly(x = ~Month, y = ~ pm2.5, 
          type = "box", 
          #  color = ~Month, 
          color = I("#937f52"),
          symbol = I("8"),
          boxpoints = "all",
          jitter = 0.3,
          pointpos = -1.8)

f3_2_02b <- daily_data %>% 
  filter(Station.name == 'DZ') %>% 
  plot_ly(x = ~Month, y = ~ pm2.5, 
          type = "box", 
          #color = ~Month, 
          #strokes = I("black"),
          color = I("#b88d2d"),
          symbol = I("1"),
          boxpoints = "all", 
          jitter = 0.3,
          pointpos = -1.8)

f3_2_03b <- daily_data %>% 
  filter(Station.name == 'SS') %>% 
  plot_ly(x = ~Month, y = ~ pm2.5, 
          type = "box", 
          color = I("#c4b485"), 
          fillcolor = col2rgb("#c4b485"),
          symbol = I("0"),
          boxpoints = "all", 
          jitter = 0.3,
          pointpos = -1.8)

f3_2_04b <- daily_data %>% 
  filter(Station.name == 'ZU') %>% 
  plot_ly(x = ~Month, y = ~ pm2.5, 
          type = "box", 
          #color = ~Month, 
          color = I("#dc9b09"),
          symbol = I("2"),
          boxpoints = "all", 
          jitter = 0.3,
          pointpos = -1.8)

p <- subplot(f3_2_01a, f3_2_01b,
        f3_2_02a, f3_2_02b,
        f3_2_03a, f3_2_03b,
        f3_2_04a, f3_2_04b, nrows=4, margin = 0.02, shareX = T, shareY =  T) %>% 
  layout(title = "My title",
         sub = "My subtitle",
         xlab = "Custom X-axis label",
         ylab = "Custom Y-axis label",
         showlegend = F #legend=list(tracegroupgap=150
         )


p <- subplot(f3_2_01a, f3_2_01b,
             f3_2_02a, f3_2_02b,
            # f3_2_03a, f3_2_03b,
            # f3_2_04a, f3_2_04b, 
            nrows=2, margin = 0.02, shareX = T, shareY =  T) %>% 
  layout(title = "My title",
         sub = "My subtitle",
         xlab = "Custom X-axis label",
         ylab = "Custom Y-axis label",
         showlegend = F #legend=list(tracegroupgap=150
  )


p %>% layout(annotations = list(
  list(x = 0.0 , y = 1.0, text = "pm10", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.55 , y = 1.0, text = "pm2.5", showarrow = F, xref='paper', yref='paper'))
)


#### Monthly variations by GGPLOT2
fig3_2a <- daily_data |>
  group_by(Station.name) |>
  ggplot(aes(Month, pm10, fill=pm2.5, group = Month)) +
  stat_summary(geom="bar", position=position_stack(), na.rm = T) +
 # geom_line(aes(Month, pm2.5/pm10, group=Month))+
 # geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.1)+
 # geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  facet_wrap(~Station.name, scales = 'free_y', ncol=1) +
  coord_flip() +
  labs(x = "Month", y = "PM10")
fig3_2a

ggplot(aes(time, tip, fill=sex)) +
  stat_summary(geom="bar", position=position_stack()) +

fig3_2b <- daily_data |>
  group_by(Station.name) |>
  ggplot(aes(Month, pm2*1000, group=Month)) +
  geom_jitter(aes(shape=Station.name, color=Station.name, size=Station.name), width = 0.1)+
  geom_boxplot(width = 0.4, outlier.shape=NA, alpha = 0.1) +
  facet_wrap(~Station.name, scales = 'free_y', ncol=1) +
  labs(x = "Month", y = "PM2.5")
fig3_2b

