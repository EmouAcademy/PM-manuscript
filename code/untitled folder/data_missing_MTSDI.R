###############
########
#.  mtdsi time series package
# install.packages("mtsdi"). install.packages("mstats")
install.packages("devtools")
devtools::install_github("truemoid/mstats")

library(mtsdi)
library(ggplot2)
library(mstats)
data(mtcars)
mstats(mtcars)
miss <- edaprep(mtcars)
setwd("~/WORK/Research/Data/02_data_tidy")
miss_UB <- read.csv("clean_df2_UB.csv")
miss_SSh <- read.csv("clean_df2_SSh.csv")
miss_DZ <- read.csv("clean_df2_DZ.csv")
miss_ZU <- read.csv("~/WORK/Research/Data/02_data_tidy/clean_df2_ZU.csv")
data(df)
edaprep(df)
mstats(df)

library(mstats)
library(tidyverse)
library(dplyr)
library(forecast)
pm10_ub <- miss_UB |>
  dplyr::select(pm10_miss)

order=c(4,1,1)
class(order)
fitSfx = arima(pm10_ub, order=c(2,0,1), seasonal=c(1,0,0))
fi_UB <- mnimput(f1,pm10_ub,eps=1e-3,ts=TRUE, 
                 method="arima",
                 ar.control = list(order=order))
fi_UB <- mnimput(f,miss_UB,eps=1e-3,ts=TRUE, 
                method="spline",
                sp.control=list(f=c(7,7,7,7,7)),
                ar.control=list(order = order, period = 12))
plot(miss_UB$pm10_miss[800:1365], type = "l")
lines(fi_UB$filled.dataset$pm10_miss[800:1365], col="red")

auto.arima(pm10_ub)

plot(miss_UB$pm10_miss)
lines(density(i_UB$filled.dataset$pm10_miss), col = "red")  
lines(density(fitSfx), col = "blue")  

plot(fitSfx)

f <- ~pm2_miss + pm10_miss + Visibility + WS
b<-c(rep("year1",365),rep("year2",365))
i_UB <- mnimput(f,by = b, miss_UB,eps=1e-3,ts=TRUE, method="spline",sp.control=list(f=c(7,7,7,7)))
i_SSh <- mnimput(f,miss_SSh,eps=1e-3,ts=TRUE, method="spline",sp.control=list(f=c(7,7,7,7,7)))
i_DZ <- mnimput(f,miss_DZ,eps=1e-3,ts=TRUE, method="spline",sp.control=list(f=c(7,7,7,7,7)))
i_ZU <- mnimput(f,miss_ZU,eps=1e-3,ts=TRUE, method="spline",sp.control=list(f=c(7,7,7,7,7)))
predict(i_UB)
plot(i_UB)
plot(fi_UB)

plot(i_SSh)
plot(i_DZ)
plot(i_ZU)

write_csv(i_UB$, file = "/Users/munkhtsetseg/WORK/Research/Data/02_data_tidy/clean_df2_ZU.csv")

Diff<-diff(miss_UB$pm2_miss, differences=2)
plot(Diff, type="l")
library(tseries)
Diff |>
  na.omit() |>
adf.test(Diff, alternative = "stationary")

plot(i_DZ$filled.dataset$pm2_miss, i_DZ$filled.dataset$pm2_miss)

plot(i_UB$filled.dataset$pm10_miss, i_SSh$filled.dataset$pm2_miss, main = "PM2.5 concentrations in reference to PM10 concentrations in UB", xlim = c(0,1.6), ylim = c(0, 1.6))
abline(a=0,b=1,col=2,lwd=3)
plot(i_SSh$filled.dataset$pm10_miss, i_SSh$filled.dataset$pm2_miss, main = "PM2.5 concentrations in reference to PM10 concentrations in UB", xlim = c(0,1.6), ylim = c(0, 1.6))

abline(a=0,b=1,col=2,lwd=3)


plot(miss_UB$pm10_miss, i_UB$filled.dataset$pm10_miss)
plot(miss$pm10_miss, miss$pm2_miss)
plot(miss$Year, miss$pm2_miss)

plot(density(miss_UB$pm2_miss, na.rm=TRUE))
lines(density(i_DZ$filled.dataset$pm2_miss), col = "red")  
lines(density(i_SSh$filled.dataset$pm10_miss), col = "green")  
lines(density(i_ZU$filled.dataset$pm10_miss), col = "blue") 

plot(density(miss_SSh$pm2_miss , na.rm=TRUE))
lines(density(i_SSh$filled.dataset$pm2_miss), col = "green")  
