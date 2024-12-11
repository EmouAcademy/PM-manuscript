
p1_01 <- data |>
  ggplot(aes(Date, pm10*1000), shape = 2) +
  geom_point() +
  facet_wrap(~Station.name, scales =  'free_y', ncol=1) +
  labs(y = "PM10")
p1_01

p2_01 <- data |>
  ggplot(aes(Date, pm2.5*1000)) +
  geom_point() +
  geom_point(aes(df02out$Date, df02out$pm2.5*1000), col='red', alpha = 0.01) +
  facet_wrap(~Station.name, scales = 'free_y', ncol=1) +
  labs(x = "Date", y = "PM2.5")
p2_01

jitter <- position_jitter(width = 0.1, height = 0.1)
p2_01 <- data |>
  ggplot(aes(Date, pm2.5*1000)) +
  geom_point(position = jitter) +
  geom_point(position = jitter, aes(df02out$Date, df02out$pm2.5*1000), col='red', alpha = 0.3) +
  facet_wrap(~Station.name, scales = 'free_y', ncol=1) +
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
