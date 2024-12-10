library(tidyverse)
head(df_combined)

df_combined_01 # the same with above

plot(df_combined$pm10...10 , df_combined$pm10_deNormalized)
head(df_combined)
df_filled <- df_combined |>
                      rename(Station.name = Station.name...1,
                             Date = Date...2,
                             Year = Year...3,
                             Month = Month...4,
                             Day = Day...5,
                             Hour = Hour...6,
                             Visibility = Visibility...7,
                             WD = WD...8,
                             WS = WS...9,
                             pm10 = pm10...10,
                             pm2.5 = pm2.5...11, # 12-22 skip
                             Visibility.f = Visibility...23, # 12-22 skip
                             WD.f = WD...24,
                             WS.f = WS...25,
                             pm10.f = pm10...26,
                             pm2.5.f = pm2.5...27,
                             WD_deN.f = WD_deNormalized,
                             WS_deN.f = WS_deNormalized,
                             pm10_deN.f = pm10_deNormalized,
                             pm2.5_deN.f = pm2.5_deNormalized)

write.csv(df_filled, file='data/processed/df_filled.csv')
