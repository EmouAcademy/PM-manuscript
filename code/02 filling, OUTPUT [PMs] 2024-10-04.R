library(tidyverse)
df_combined
df_combined_01 # the same with above

plot(df_combined$pm2.5...11 , df_combined$pm2.5_deNormalized)
df_filling_output <- df_combined |>
                      rename(Station.name = Station.name...1,
                           #  Date = Date...2,
                             Year = Year...2,
                             Month = Month...3,
                             Day = Day...4,
                             Hour = Hour...5,
                             Visibility = Visibility_deNormalized,
                             WD = WD_deNormalized,
                             WS = WS_deNormalized,
                             pm10 = pm10_deNormalized,
                             pm2.5 = pm2.5_deNormalized)

df_filled_monthly_PMs <- df_filling_output |>
              select(Station.name, 
                    # Date, 
                     Year, Month, Day, Hour, Visibility, 
                             WD, WS, pm10, pm2.5)

df_input_to_fill_daily_PMs <- df_combined |>
  rename(Station.name = Station.name...1,
       #  Date = Date...2,
         Year = Year...3,
         Month = Month...4,
         Day = Day...5,
         Hour = Hour...6,
         Visibility = Visibility...24,
         WD = WD...25,
         WS = WS...26,
         OPC = OPC...27,
         pm10 = pm10...28,
         pm2.5 = pm2.5...29)
df_input_to_fill_daily_PMs <- df_input_to_fill_daily_PMs |>
  select(Station.name, Date, Year, Month, Day, Hour, Visibility, 
         WD, WS, OPC, pm10, pm2.5)
