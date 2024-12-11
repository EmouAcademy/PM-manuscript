library(tidyverse)

df_PCA <- daily_data |>
#  filter(Station.name == 'Sainshand') |>
 # select(Month,Hour, Visibility, WD, WS, pm10, pm2.5,r)
 select(, c(1, 3:6, 33:37) ) |>
 mutate(r = pm2.5/pm10) |>
df_PCA[r_SS_PCA == 0] <- 0.001

#Run PCA
df_pca <- prcomp(df_PCA, 
                     scale = T)
summary(df_pca)



library(missMDA)
library(factoextra)
library(FactoMineR)
library(ggrepel)
library(ggplot2)
library(mixOmics)
library(EMSC)
library(pls)
library(prospectr)

colors = c("#CD0000", "#EE7600", "#FFFF00", "#008B45")
col <- as.factor(df02out$Station.name)
groups <- c("Greece","Italy","Portuga", "Spain")

oil.pca <- PCA(df_PCA,scale.unit = TRUE,ncp = 5,graph = TRUE)
summary(oil.pca)
g1 <- fviz_pca_ind(res.pca, geom.ind = "point",
                   col.ind = df_filling_output$Station.name, # color by groups
                  palette = 'jco',
                   addEllipses = TRUE, ellipse.type = "convex",
                  legend.title = "Groups")+
   ggtitle("PMs - geographical-Raw Data")

g1

res.pca = PCA(df_PCA, 
              scale.unit=TRUE, ncp=5, quanti.sup=11, quali.sup=1, graph=T)
