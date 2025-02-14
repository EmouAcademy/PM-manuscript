library(ggplot2)
library(readr)
library(tidyverse)      # scale_y_break
library(here)
source(here("code/Colors.R"))
library(scales)
library(ggpubr)
library(ggbreak)
library(patchwork)
library(ggrepel)
####### input DATA 
data <- read.csv("data/processed/df.csv")
df <- read.csv("data/processed/df_filled.csv")
## add order and levels
data$Station.name <- factor(data$Station.name, 
                            levels = c("UB", "DZ",  "ZU", "SS"))  
levels(data$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')

df <- read.csv("data/processed/df_filled.csv")
df$Station.name <- factor(df$Station.name, 
                          levels = c("UB", "DZ",  "ZU", "SS"))  
levels(df$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')

data$Date <- as.Date(data$Date) 
df$Date <- as.Date(df$Date) 
df <- df |>
  filter(Year<2021)3
df <- df %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(12, 1, 2) ~ 'Winter',
      Month %in% c(3, 4, 5) ~ 'Spring',
      Month %in% c(6, 7, 8) ~ 'Summer',
      Month %in% c(9, 10, 11) ~ 'Autumn'
    )
  )
df$Season <- factor(df$Season, 
                    levels = c('Winter', 'Spring', 'Summer', 'Autumn')) 

# Calculate seasonal averages for each year and season
df_seasonal <- df %>%
  group_by(Station.name, Year, Season) %>%
  summarize(
    Seasonal_avg.WD = mean(WD_deN.f, na.rm = TRUE),     # average WD, WS, VIS
    Seasonal_avg.WS = mean(WS_deN.f, na.rm = TRUE),
    Seasonal_avg.VIS = mean(Visibility_deNormalized, na.rm = TRUE),
    Seasonal_avg.pm10 = mean(pm10_deN.f, na.rm = TRUE),     # average PM, PM2.5, r
    Seasonal_avg.pm2.5 = mean(pm2.5_deN.f, na.rm = TRUE),
    Seasonal_avg.r = Seasonal_avg.pm2.5/Seasonal_avg.pm10,
    .groups = 'drop'
  )

####################### this is the BEGIN of supplement figure 8:b CORREL MATRIX. ######################

df_time_lag <- df_seasonal

df_time_lag |>
  filter()

df_winter <- df_time_lag |>
  filter(Season == "Winter")
#  pivot_longer(cols=c(Seasonal_avg.WD, Seasonal_avg.WS, Seasonal_avg.VIS, Seasonal_avg.pm10, Seasonal_avg.pm2.5, Seasonal_avg.r), names_to = "name", values_to = "value")

df_spring <- df_time_lag |>
  filter(Season == "Spring")
  pivot_longer(cols=c(Seasonal_avg.WD, Seasonal_avg.WS, Seasonal_avg.VIS, Seasonal_avg.pm10, Seasonal_avg.pm2.5, Seasonal_avg.r), names_to = "name", values_to = "value")

df_summer <- df_time_lag |>
  filter(Season == "Summer")|>
  pivot_longer(cols=c(Seasonal_avg.WD, Seasonal_avg.WS, Seasonal_avg.VIS, Seasonal_avg.pm10, Seasonal_avg.pm2.5, Seasonal_avg.r), names_to = "name", values_to = "value")

df_seasonal_joins <- merge(df_winter, df_spring, by = c("Station.name", "Year"))             # bind_rows(), merge(... , by  =)
df_seasonal_joins_3 <- merge(df_seasonal_joins, df_summer, by = c("Station.name", "Year"))


df_corr <- df_seasonal_joins_3 |>
  select(Station.name, Season, Seasonal_avg.pm2.5.x, Seasonal_avg.pm2.5.y, Seasonal_avg.r.y, Seasonal_avg.r) |>
  rename("winter pm2.5" = Seasonal_avg.pm2.5.x,
         "spring pm2.5" = Seasonal_avg.pm2.5.y,
         "spring r" = Seasonal_avg.r.y,
         "summer r" = Seasonal_avg.r) |>
  filter(Station.name == "UB" | Station.name == "DZ") |>
  drop_na()


df_corr_UB <- df_corr %>%
  filter(Station.name == "UB") |>
  select(Station.name, Season, `winter pm2.5`, `spring r`, `spring pm2.5`, `summer r`) |>
  drop_na()

df_corr_DZ <- df_corr %>%
  filter(Station.name == "DZ") |>
  drop_na()
library(corrplot)
library(Hmisc)
library(Formula)
library(survival)
library(psych)
pvalues_UB <- rcorr(as.matrix(df_corr_UB[3:6]))
pvalues_UB$P[is.na(pvalues_UB$P)] <- 1.1
pvalues_DZ <- rcorr(as.matrix(df_corr_DZ[3:6]))
pvalues_DZ$P[is.na(pvalues_DZ$P)] <- 1.1
corr_matrix_UB <- cor(df_corr_UB[3:6]) 
corr_matrix_DZ <- cor(df_corr_DZ[3:6]) 
trace(corrplot, edit=TRUE)
### Then replace on line 443; add +0.25

png("submission/images/figure_9.png", res = 72, width = 1000, height = 1000, pointsize = 25)

corrplot::corrplot((corr_matrix_DZ), diag = T, col = COL2(n=5), method = 'ellipse', cl.length = 6, addCoef.col = 'lightgrey' , type = 'upper',
                   cl.pos = 'b',  tl.col="black", tl.pos = "lt", tl.srt=0, order = 'hclust',tl.offset = 1,
                   p.mat = as.matrix(pvalues_DZ$P),
                   addrect = 2,
                   insig = 'label_sig',  sig.level = c(0.001, 0.01, 0.05),
                   pch.cex = 1.9, pch.col = 'white', mar=c(0,0,1,1))
corrplot::corrplot((corr_matrix_UB), diag = F, col = COL2(n=5), method = 'circle', cl.length = 6, addCoef.col = 'grey' , type = 'lower',
                     cl.pos = 'n', tl.pos = "n", order = 'original',
                     p.mat = as.matrix(pvalues_UB$P),
                     insig = 'label_sig',  sig.level = c(0.001, 0.01, 0.05),
                     pch.cex = 1.9, pch.col = "white", mar=c(0,0,1,0),
                    add = TRUE)
dev.off()

####################### this is the END of supplement figure 8:b CORREL MATRIX. ######################



####################### this is BEGIN of supplement figure 9 ######################
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping) + 
    geom_point() + 
    #geom_smooth(method=lm, se= FALSE, alpha = 0.5, fill="red", color="red", ...) +
    geom_smooth(aes(colour = Station.name), method=glm, se= FALSE, size = 0.2) 
  p
}
  
df_corr_with_year <- df_seasonal_joins_3 |>
  select(Station.name, Year, Season, Seasonal_avg.pm2.5.x, Seasonal_avg.pm2.5.y, Seasonal_avg.r.y, Seasonal_avg.r) |>
  rename("winter pm2.5" = Seasonal_avg.pm2.5.x,
         "spring pm2.5" = Seasonal_avg.pm2.5.y,
         "spring r" = Seasonal_avg.r.y,
         "summer r" = Seasonal_avg.r) |>
  mutate(Year = case_when(Year >2013 ~ "II", TRUE ~ "I")) |>
  filter(Station.name == "UB" | Station.name == "DZ") |>
  drop_na()

library(GGally)
plot_20250204 <- GGally:: ggpairs(df_corr, columns = 3:6, axisLabels = "none", 
                 ggplot2::aes(colour = Station.name, alpha =0.5),
                 lower = list(continuous = wrap("cor", size = 2.5)),
                   upper = list(continuous = my_fn, display_grid = FALSE),
                 switch = "both") 
ggsave("submission/images/figure_9_supplement.png",plot_20250204, width = 5, height = 5, units = "in", dpi = 300)

####################### this is the END of supplement figure 9. ######################

