library(ggplot2)
library(readr)
library(missMDA)
library(factoextra)
library(FactoMineR)
library(ggrepel)
library(ggplot2)
library(mixOmics)
library(EMSC)
library(pls)
library(prospectr)
source(here("code/Colors.R"))

####### input DATA 
data <- read.csv("data/processed/df.csv")

## add order and levels
data$Station.name <- factor(data$Station.name, 
                            levels = c("UB", "DZ",  "ZU", "SS"))  
levels(data$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')
#### hourly data for UB&DZ
data$pm10[data$pm10 > 6] <- 4

#### DAILY data
df_PCA.h <- data |>
  select(, -c(1:2, 4:8) )


daily_data <- data |>
  group_by(Station.name, Date) |>
  summarise(across(where(is.numeric), ~ mean(.x,  by=c(Station.name, Date),na.rm = F)))
df_PCA <- daily_data |>
  #  filter(Station.name == 'Sainshand') |>
  # select(Month,Hour, Visibility, WD, WS, pm10, pm2.5,r)
  select(, -c(2:7) )

  monthly_data <- data |>
  group_by(Station.name, Year, Month) |>
  summarise(across(where(is.numeric), ~ mean(.x,  by=c(Station.name, Year, Month),na.rm = T)))
monthly_data <- as.data.frame(monthly_data)
df_PCA.m <- monthly_data |>
              select(, -c(2:6))

########################### Plotting
###### hourly PCA

res.pca.h = PCA(df_PCA.h, 
              scale.unit=TRUE, ncp=5, quanti.sup=8, quali.sup=1, graph=T)

g1 <- fviz_pca_ind(res.pca.h, geom.ind = "point",
                   col.ind = df_PCA.h$Station.name, # color by groups
                   palette = 'jco',
                   addEllipses = TRUE, ellipse.type = "convex",
                   legend.title = "Groups")+
  ggtitle("PMs - geographical-Raw Data")

g1


###### Daily PCA

res.pca.d = PCA(df_PCA, 
              scale.unit=TRUE, ncp=5, quanti.sup=8, quali.sup=1, graph=T)

g1 <- fviz_pca_ind(res.pca.d, geom.ind = "point",
                   col.ind = df_PCA$Station.name, # color by groups
                   palette = 'jco',
                   addEllipses = TRUE, ellipse.type = "convex",
                   legend.title = "Groups")+
  ggtitle("PMs - geographical-Raw Data")

g1



########## Monthly PCA
res.pca.m = PCA(df_PCA.m, 
              scale.unit=TRUE, ncp=5, quanti.sup=8, quali.sup=1, graph=T)

pca_month <- fviz_pca_ind(res.pca.m, geom.ind = "point",
                   col.ind = df_PCA_month$Station.name, # color by groups
                   palette = 'jco',
                   addEllipses = TRUE, ellipse.type = "convex",
                   legend.title = "Groups")+
  ggtitle("PMs - geographical-Raw Data")



########################### Output

ggsave("visuals/fig3_2_pca_a.png", res.pca.m, width=8, height=5, unit="in", dpi = 300)
ggsave("visuals/fig3_2_pca_b.png", pca_month, width=8, height=5, unit="in", dpi = 300)



####################################################
### MISSING data
library(tidyverse)
library(missMDA)
library(FactoMineR)

df_daily_UB <- df_month |>
  filter(Station.name == 'UB') |>
  select(,-c(2))
res <- MCA(df_daily_UB[1:55,6:12], quali.sup = 1:3)   #### perform MCA with NA as new CATEGORies

nb <- estim_ncpMCA(res) ### estimate the NUMBER of COMPONENTs
nb

imputeMCA(df_daily_UB[1:55,6:12], ncp = 5, quali.sup = 1:3)
