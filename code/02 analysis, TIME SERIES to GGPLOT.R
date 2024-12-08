library(tidyverse) # Includes the packages ggplot2 and tidyr, which we use below
devtools:: install_github("brisneve/ggplottimeseries")
library(ggplottimeseries)
library(tidyr)
library(ggplot2)

data(df_filled_daily_PMs)

x <- df_clean$Date
y <- df_filled_daily_PMs$pm10
z <- 365.25 #number of days in a year

df <- dts1(x,y,z, type = "additive")

head(df)

#plots decomposed time series into one figure
ggdecompose(dts1)+
  xlab("Date")+
  ylab("Atmospheric Concentration of CO2")


# Get the time values for the time series
Time = attributes(ts_SS)[[1]]
Time = seq(Time[1],Time[2], length.out=(Time[2]-Time[1])*Time[3])

# Convert td to data frame
dat = cbind(Time, with(td, data.frame(Observed=x, Trend=trend, Seasonal=seasonal, Random=random)))

ggplot(gather(dat, component, value, -Time), aes(Time, value)) +
  facet_grid(component ~ ., scales="free_y") +
  geom_line() +
  theme_bw() +
  labs(y=expression(CO[2]~(ppm)), x="Year") +
  ggtitle(expression(Decomposed~CO[2]~Time~Series)) +
  theme(plot.title=element_text(hjust=0.5))