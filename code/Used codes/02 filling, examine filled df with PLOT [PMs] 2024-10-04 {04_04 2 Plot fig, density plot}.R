library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)



ggplot(data = df_dataTransform, aes(x = pm2.5_Normalized, fill=Station.name, colour=Station.name)) +
  geom_density(alpha=0.4, size=0.5) +
  geom_density(data = df_dataImputed,
               aes(x = pm2.5_Normalized, group=Station.name),
               inherit.aes = FALSE,
                 alpha=0.4) +
  facet_wrap(~Station.name) +
  theme_bw()


# With transparency (right)
p2_Vis <- df_combined %>%
  group_by(Station.name...1) %>%
  ggplot(aes(x= Visibility_Normalized, 
              fill=Station.name...1)) +
  geom_density( alpha=.2, linetype = 0) +
  geom_line(aes(x=Visibility_Normalized), stat = "density", col = "red", linetype= "3313") +
  theme_ipsum()
p2_Vis


p2_WS <- df_combined %>%
  group_by(Station.name...1) %>%
  ggplot(aes(x= WS_Normalized, 
             fill=Station.name...1)) +
  geom_density( alpha=.2) +
  geom_line(aes(x=WS_Imputed), stat = "density", col = "red", linetype= "3313") +
  theme_ipsum()

p2_pm10 <- df_combined %>%
#  group_by(Station.name) %>%
  ggplot(aes(x= pm10_Normalized, 
             fill=Station.name...1)) +
  geom_density( alpha=.2) +
  geom_line(aes(x=pm10_Imputed), stat = "density", col = "red", linetype= "3313") +
  theme_ipsum()
p2_pm10

p2_pm2.5 <- df_combined %>%
#  group_by(Station.name) %>%
  ggplot(aes(x= pm2.5_Normalized, 
             fill=Station.name...1)) +
  geom_density( alpha=.2) +
  geom_line(aes(x=pm2.5_Imputed), stat = "density", col = "red", linetype= "3313") +
  theme_ipsum()
p2_pm2.5


p2_pm2.5_d <- df_combined %>%
  filter(Station.name...1 == "Dalanzadgad") %>%
  ggplot(aes(Date...2 , y= pm2.5_deNormalized , 
             fill=Station.name...1)) +
  geom_line()
  geom_density( alpha=.2) +
  geom_line(aes(x=pm10_deNormalized), stat = "density", col = "red", linetype= "3313") +
  theme_ipsum()
p2_pm2.5_d

### Examining output
#### Figure 2b,c (Variable plot + boxplot)
### a. long_form


par(mfrow = c(2,2))
boxplot(df_UB_norm$Visibility, na.rm = TRUE)
boxplot(df_UB_comp$Visibility, na.rm = TRUE)
t.test(df_UB_norm$Visibility,df_UB_comp$Visibility)
plot(density(df_UB_norm$Visibility, na.rm = TRUE), main = "Visibility")
lines(density(df_UB_comp$Visibility, na.rm = TRUE), col = "red" )

plot(density(df_UB_norm$WS, na.rm = TRUE), main = "WS")
lines(density(df_UB_comp$WS, na.rm = TRUE), col = "red")

plot(density(df_UB_norm$pm10, na.rm = TRUE), main = "PM10")
lines(density(df_UB_comp$pm10, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_UB_norm$pm2.5, na.rm = TRUE), main = "PM2.5")
lines(density(df_UB_comp$pm2.5, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_UB_norm$Visibility_Normalized, na.rm = TRUE), main = "Visibility_Normalized")
lines(density(df_UB_comp$Visibility_Normalized, na.rm = TRUE), col = "red", lty = 3 )

boxplot(df_UB_norm$WD_Normalized, na.rm = TRUE)
boxplot(df_UB_comp$WD_Normalized, na.rm = TRUE)
plot(density(df_UB_norm$WD_Normalized, na.rm = TRUE), main = "WD_Normalized")
lines(density(df_UB_comp$WD_Normalized, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_UB_norm$WS_Normalized, na.rm = TRUE), main = "WS_Normalized")
lines(density(df_UB_comp$WS_Normalized, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_UB_norm$pm10_Normalized, na.rm = TRUE), main = "pm10_Normalized")
lines(density(df_UB_comp$pm10_Normalized, na.rm = TRUE), col = "red", lty = 3 )


plot(density(df_UB_norm$pm2.5_Normalized, na.rm = TRUE), main = "pm2.5_Normalized")
lines(density(df_UB_comp$pm2.5_Normalized, na.rm = TRUE), col = "red", lty = 3 )

par(mfrow = c(1,1))
plot(df_UB_comp$Date, df_UB_comp$pm10_Normalized)

##### DZ
par(mfrow = c(3,3))

plot(density(df_DZ_norm$Visibility_Normalized, na.rm = TRUE), main = "Visibility_Normalized")
lines(density(df_DZ_comp$Visibility_Normalized, na.rm = TRUE), col = "red", lty = 3 )

boxplot(df_DZ_norm$WD_Normalized, na.rm = TRUE)
boxplot(df_DZ_comp$WD_Normalized, na.rm = TRUE)
plot(density(df_DZ_norm$WD_Normalized, na.rm = TRUE), main = "WD_Normalized")
lines(density(df_DZ_comp$WD_Normalized, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_DZ_norm$WS_Normalized, na.rm = TRUE), main = "WS_Normalized")
lines(density(df_DZ_comp$WS_Normalized, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_DZ_norm$pm10_Normalized, na.rm = TRUE), main = "pm10_Normalized")
lines(density(df_DZ_comp$pm10_Normalized, na.rm = TRUE), col = "red", lty = 3 )


plot(density(df_DZ_norm$pm2.5_Normalized, na.rm = TRUE), main = "pm2.5_Normalized")
lines(density(df_DZ_comp$pm2.5_Normalized, na.rm = TRUE), col = "red", lty = 3 )

par(mfrow = c(1,1))
plot(df_DZ_comp$Date, df_DZ_comp$pm10_Normalized)


##### SS
par(mfrow = c(1,3))

plot(density(df_SS_norm$Visibility_Normalized, na.rm = TRUE), main = "Visibility_Normalized")
lines(density(df_SS_comp$Visibility_Normalized, na.rm = TRUE), col = "red", lty = 3 )

boxplot(df_SS_norm$WD_Normalized, na.rm = TRUE)
boxplot(df_SS_comp$WD_Normalized, na.rm = TRUE)
plot(density(df_SS_norm$WD_Normalized, na.rm = TRUE), main = "WD_Normalized")
lines(density(df_SS_comp$WD_Normalized, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_SS_norm$WS_Normalized, na.rm = TRUE), main = "WS_Normalized")
lines(density(df_SS_comp$WS_Normalized, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_SS_norm$pm10_Normalized, na.rm = TRUE), main = "pm10_Normalized")
lines(density(df_SS_comp$pm10_Normalized, na.rm = TRUE), col = "red", lty = 3 )


plot(density(df_SS_norm$pm2.5_Normalized, na.rm = TRUE), main = "pm2.5_Normalized")
lines(density(df_SS_comp$pm2.5_Normalized, na.rm = TRUE), col = "red", lty = 3 )
plot(df_SS_norm$pm2.5_Normalized,df_SS_comp$pm2.5_Normalized)
par(mfrow = c(1,1))
plot(df_SS_comp$Date, df_SS_comp$pm10_Normalized)

##### ZU
par(mfrow = c(3,3))

plot(density(df_ZU_norm$Visibility_Normalized, na.rm = TRUE), main = "Visibility_Normalized")
lines(density(df_ZU_comp$Visibility_Normalized, na.rm = TRUE), col = "red", lty = 3 )

boxplot(df_ZU_norm$WD_Normalized, na.rm = TRUE)
boxplot(df_ZU_comp$WD_Normalized, na.rm = TRUE)
plot(density(df_ZU_norm$WD_Normalized, na.rm = TRUE), main = "WD_Normalized")
lines(density(df_ZU_comp$WD_Normalized, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_ZU_norm$WS_Normalized, na.rm = TRUE), main = "WS_Normalized")
lines(density(df_ZU_comp$WS_Normalized, na.rm = TRUE), col = "red", lty = 3 )

plot(density(df_ZU_norm$pm10_Normalized, na.rm = TRUE), main = "pm10_Normalized")
lines(density(df_ZU_comp$pm10_Normalized, na.rm = TRUE), col = "red", lty = 3 )


plot(density(df_ZU_norm$pm2.5_Normalized, na.rm = TRUE), main = "pm2.5_Normalized")
lines(density(df_ZU_comp$pm2.5_Normalized, na.rm = TRUE), col = "red", lty = 3 )

par(mfrow = c(1,1))
plot(df_ZU_comp$Date, df_ZU_comp$pm2.5_Normalized)
par(mfrow = c(2,2))
p2_Vis

dev.off()
