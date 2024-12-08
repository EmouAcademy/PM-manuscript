library(tidyverse)
library(missMDA)
library(factoextra)
library(FactoMineR)
library(ggrepel)
library(ggplot2)
library(mixOmics)
library(EMSC)
library(pls)
library(prospectr)

df_daily_entry_PCA <- df_daily |>
 # filter(Station.name == 'UB') |>
  # select(Month,Hour, Visibility, WD, WS, pm10, pm2.5,r)
  # select(, -c(2, 5) )       # select(Year, Month,Hour, Visibility, WD, WS, OPC, pm10, pm2.5,r)
  # select(, -c(2, 5, 10) )   # select(Year, Month,Hour, Visibility, WD, WS,      pm10, pm2.5,r)
   select(, -c(2) )        # select(                  Visibility, WD, WS, OPC, pm10, pm2.5,r)
  # select(, -c(2:6, 10) )    # select(                  Visibility, WD, WS,      pm10, pm2.5,r)

df_monthly_entry_PCA <- df_monthly_PMs |>
  # filter(Station.name == 'UB') |>
  # select(Month,Hour, Visibility, WD, WS, pm10, pm2.5,r)
  # select(, -c(2, 5) )       # select(Year, Month,Hour, Visibility, WD, WS, OPC, pm10, pm2.5,r)
  # select(, -c(2, 5, 10) )   # select(Year, Month,Hour, Visibility, WD, WS,      pm10, pm2.5,r)
  select(, -c(2:5) )        # select(                  Visibility, WD, WS, OPC, pm10, pm2.5,r)
# select(, -c(2:6, 10) )    # select(                  Visibility, WD, WS,      pm10, pm2.5,r)


#Run PCA
daily_res.pca = PCA(df_daily_entry_PCA, 
              scale.unit=TRUE, ncp=5,  quali.sup=1, graph=T)


monthly_res.pca = PCA(df_monthly_entry_PCA, 
              scale.unit=TRUE, ncp=5,  quali.sup=1:2, graph=T)
summary(monthly_res.pca)


df_pca <- prcomp(df_PCA, 
                 scale = T)
summary(df_pca)





colors = c("#CD0000", "#EE7600", "#FFFF00", "#008B45")
col <- as.factor(df02out$Station.name)
groups <- c("Greece","Italy","Portuga", "Spain")


g1_daily <- fviz_pca_ind(daily_res.pca, geom.ind = "point",
                   col.ind = df_daily$Station.name, # color by groups
                   palette = colors,
                   addEllipses = TRUE, ellipse.type = "convex",
                   legend.title = "Sites")+
  ggtitle("PMs - geographical-Raw Data")
g1_daily

g1_monthly <- fviz_pca_ind(monthly_res.pca, geom.ind = "point",
                         col.ind = df_monthly_entry_PCA$Station.name, # color by groups
                         palette = colors,
                         addEllipses = TRUE, ellipse.type = "convex",
                         legend.title = "Sites")+
  ggtitle("PMs - geographical-Monthly Data")
g1_monthly


g1_monthly_pca.biplot <- fviz_pca_biplot(monthly_res.pca, geom.ind = "point",
                           col.ind = "var", # color by groups
                           palette = colors,
                           addEllipses = TRUE, ellipse.type = "convex",
                           legend.title = "Sites")+
  ggtitle("PMs - geographical-Monthly Data")
g1_monthly_pca.biplot
####################################################
### miss data
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
