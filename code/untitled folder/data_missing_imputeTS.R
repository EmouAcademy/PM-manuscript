library(forecast)
library(imputeTS)
imp <- na_kalman(miss_DZ$pm10_miss, maxgap = 15)
imp <- na_interpolation(miss_DZ$pm10_miss, option = "spline",  maxgap = 7)
imp_seas <- na_seasplit(data_amelia, find_frequency=TRUE, maxgap = 3)


ggplot_na_imputations(imp[900:1000], miss_DZ$pm10_miss[900:1000],miss_DZ$WS[900:1000])
ggplot_na_imputations(miss_DZ$pm10_miss[3000:4000], imp[3000:4000])
na_seasplit

ggplot_na_distribution(miss_UB$pm10_miss)
ggplot_na_distribution(imp)
ggplot_na_gapsize(imp)
plot(imp)
lines(miss_DZ$pm10_miss, col = "red")
