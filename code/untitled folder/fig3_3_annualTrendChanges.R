
p1_01 <- daily_data |>
  ggplot(aes(Date, pm10*1000, colour = Station.name, shape = Station.name)) +
  geom_point() +
  #  facet_wrap(~Station.name, scales =  'free_y', ncol=1) +
  labs(y = "PM10")
p1_01

p2_01 <- daily_data |>
  ggplot(aes(Date, pm2.5*1000, colour = Station.name, shape = Station.name)) +
  geom_point() +
  #  facet_wrap(~Station.name, scales = 'free_y', ncol=1) +
  labs(x = "Date", y = "PM2.5")
p2_01




data |>
  ggplot(aes(pm10, pm2.5, color = Visibility, size = WS)) +
  geom_point() +
  facet_grid(~Station.name, scales = 'free')

daily_data |>
  ggplot(aes(pm10, pm2.5, color = Visibility, size = WS)) +
  geom_point() +
  facet_wrap(~Station.name, scales = 'free')
