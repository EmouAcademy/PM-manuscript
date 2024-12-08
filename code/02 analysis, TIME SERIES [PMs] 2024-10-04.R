library(tidyverse)
### UB
pm10_UB_test <- df_filled_daily_PMs |>
        filter(Station.name == 'UB') |>
        select(pm10)

ts_UB <- ts(pm10_UB_test, start = c(2008, 59), frequency = 365)
plot.ts(ts_UB)
annual_TimeSeries_UB_pm10 <- decompose(ts_UB)
plot(annual_TimeSeries_UB_pm10)

### DZ
pm10_DZ_test <- df_filled_daily_PMs |>
  filter(Station.name == 'DZ') |>
  select(pm10)

ts_DZ <- ts(pm10_DZ_test, start = c(2008, 59), frequency = 365)
plot.ts(ts_DZ)

annual_TimeSeries_DZ_pm10 <- decompose(ts_DZ)
plot(annual_TimeSeries_DZ_pm10)

### ZU
pm10_ZU_test <- df_filled_daily_PMs |>
  filter(Station.name == 'ZU') |>
  select(pm10)

ts_ZU <- ts(pm10_ZU_test, start = c(2008, 59), frequency = 365)
plot.ts(ts_ZU)

annual_TimeSeries_ZU_pm10 <- decompose(ts_ZU)
plot(annual_TimeSeries_ZU_pm10)

### SS
pm10_SS_test <- df_filled_daily_PMs |>
  filter(Station.name == 'SS') |>
  select(pm10)
pm10_SS_test$pm10 <- replace(pm10_SS_test$pm10, pm10_SS_test$pm10 > 0.15, 0.15)

ts_SS <- ts(pm10_SS_test, start = c(2008, 59), frequency = 365)
plot.ts(ts_SS)

annual_TimeSeries_SS_pm10 <- decompose(ts_SS)
plot(annual_TimeSeries_SS_pm10)

########### pm2.5

### UB
pm2.5_UB_test <- df_filled_daily_PMs |>
  filter(Station.name == 'UB') |>
  select(pm2.5)

ts_UB <- ts(pm2.5_UB_test, start = c(2008, 59), frequency = 365)
plot.ts(ts_UB)
annual_TimeSeries_UB_pm2.5 <- decompose(ts_UB)
plot(annual_TimeSeries_UB_pm2.5)


### DZ
pm2.5_DZ_test <- df_filled_daily_PMs |>
  filter(Station.name == 'DZ') |>
  select(pm2.5)

ts_DZ <- ts(pm2.5_DZ_test, start = c(2008, 59), frequency = 365)
plot.ts(ts_DZ)

annual_TimeSeries_DZ_pm2.5 <- decompose(ts_DZ)
plot(annual_TimeSeries_DZ_pm2.5)


### ZU
pm2.5_ZU_test <- df_filled_daily_PMs |>
  filter(Station.name == 'ZU') |>
  select(pm2.5)

ts_ZU <- ts(pm2.5_ZU_test, start = c(2008, 59), frequency = 365)
plot.ts(ts_ZU)

annual_TimeSeries_ZU_pm2.5 <- decompose(ts_ZU)
plot(annual_TimeSeries_ZU_pm2.5)


### SS
pm2.5_SS_test <- df_filled_daily_PMs |>
  filter(Station.name == 'SS') |>
  select(pm2.5)

ts_SS <- ts(pm2.5_SS_test, start = c(2008, 59), frequency = 365)
plot.ts(ts_SS)

annual_TimeSeries_SS_pm2.5 <- decompose(ts_SS)
plot(annual_TimeSeries_SS_pm2.5)


#################### END of GOAL ############################

plot(annual_TimeSeries_UB_pm10)
plot(annual_TimeSeries_DZ_pm10)
plot(annual_TimeSeries_ZU_pm10)
plot(annual_TimeSeries_SS_pm10)
plot(annual_TimeSeries_UB_pm2.5)
plot(annual_TimeSeries_DZ_pm2.5)
plot(annual_TimeSeries_ZU_pm2.5)
plot(annual_TimeSeries_SS_pm2.5)



############# MONTHLY
library(tidyverse)
### UB
pm10_UB_test <- df_filled_monthly_PMs |>
  filter(Station.name == 'UB') |>
  select(pm10)

ts_UB <- ts(pm10_UB_test, start = c(2008, 3), frequency = 12)
plot.ts(ts_UB)
annual_TimeSeries_UB_pm10 <- decompose(ts_UB)
plot(annual_TimeSeries_UB_pm10)

### DZ
pm10_DZ_test <- df_filled_monthly_PMs |>
  filter(Station.name == 'DZ') |>
  select(pm10)

ts_DZ <- ts(pm10_DZ_test, start = c(2008, 3), frequency = 12)
plot.ts(ts_DZ)

annual_TimeSeries_DZ_pm10 <- decompose(ts_DZ)
plot(annual_TimeSeries_DZ_pm10)

### ZU
pm10_ZU_test <- df_filled_monthly_PMs |>
  filter(Station.name == 'ZU') |>
  select(pm10)

ts_ZU <- ts(pm10_ZU_test, start = c(2008, 3), frequency = 12)
plot.ts(ts_ZU)

annual_TimeSeries_ZU_pm10 <- decompose(ts_ZU, type = 'multiplicative')
plot(annual_TimeSeries_ZU_pm10)

### SS
pm10_SS_test <- df_filled_monthly_PMs |>
  filter(Station.name == 'SS') |>
  select(pm10)

ts_SS <- ts(pm10_SS_test, start = c(2008, 3), frequency = 12)
plot.ts(ts_SS)

annual_TimeSeries_SS_pm10 <- decompose(ts_SS)
plot(annual_TimeSeries_SS_pm10)

########### pm2.5

### UB
pm2.5_UB_test <- df_filled_monthly_PMs |>
  filter(Station.name == 'UB') |>
  select(pm2.5)

ts_UB <- ts(pm2.5_UB_test, start = c(2008, 3), frequency = 12)
plot.ts(ts_UB)
annual_TimeSeries_UB_pm2.5 <- decompose(ts_UB)
plot(annual_TimeSeries_UB_pm2.5)


### DZ
pm2.5_DZ_test <- df_filled_monthly_PMs |>
  filter(Station.name == 'DZ') |>
  select(pm2.5)

ts_DZ <- ts(pm2.5_DZ_test, start = c(2008, 3), frequency = 12)
plot.ts(ts_DZ)

annual_TimeSeries_DZ_pm2.5 <- decompose(ts_DZ)
plot(annual_TimeSeries_DZ_pm2.5)


### ZU
pm2.5_ZU_test <- df_filled_monthly_PMs |>
  filter(Station.name == 'ZU') |>
  select(pm2.5)

ts_ZU <- ts(pm2.5_ZU_test, start = c(2008, 3), frequency = 12)
plot.ts(ts_ZU)

annual_TimeSeries_ZU_pm2.5 <- decompose(ts_ZU)
plot(annual_TimeSeries_ZU_pm2.5)


### SS
pm2.5_SS_test <- df_filled_monthly_PMs |>
  filter(Station.name == 'SS') |>
  select(pm2.5)

ts_SS <- ts(pm2.5_SS_test, start = c(2008, 3), frequency = 12)
plot.ts(ts_SS)

annual_TimeSeries_SS_pm2.5 <- decompose(ts_SS)
plot(annual_TimeSeries_SS_pm2.5)


