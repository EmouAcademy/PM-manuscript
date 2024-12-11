library(mice)

data_mice <- data_amelia

data_mice_imp <- mice(data_mice)

data_comp <- complete(data_mice_imp)

par(mfrow = c(1,2))
boxplot(data_mice$Visibility, na.rm = TRUE)
boxplot(data_comp$Visibility, na.rm = TRUE)
t.test(data_mice$Visibility,data_comp$Visibility)
plot(density(data_mice$Visibility, na.rm = TRUE), main = "Visibility")
lines(density(data_comp$Visibility, na.rm = TRUE), col = "red" )

plot(density(data_mice$WS, na.rm = TRUE), main = "WS")
lines(density(data_comp$WS, na.rm = TRUE), col = "red" )

plot(density(data_mice$pm10, na.rm = TRUE), main = "PM10")
lines(density(data_comp$pm10, na.rm = TRUE), col = "red", lty = 3 )

plot(density(data_mice$pm2.5, na.rm = TRUE), main = "PM2.5")
lines(density(data_comp$pm2.5, na.rm = TRUE), col = "red", lty = 3 )


