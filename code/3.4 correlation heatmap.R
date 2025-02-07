################## time-lag, seasonal cycle figure
### data 


# Define a function to generate plots
generate_plot <- function(data, x_var, y_var, col_var, size_var, title) {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(col_var), size = !!sym(size_var))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(title)
}
# Generate and save a plot
plot <- generate_plot(df_seasonal_joins, "Seasonal_avg.pm2.5.x", "Seasonal_avg.r.y", "Station.name", "Seasonal_avg.WS.y", "My Plot Title")
plot
ggsave("plots/my_plot.png", plot)


generate_and_save_plot <- function(data, x_var, y_var, title, file_path) {
  plot <- generate_plot(data, x_var, y_var, title)
  ggsave(file_path, plot)
}

generate_and_save_plot(df_seasonal_joins, "Seasonal_avg.pm2.5", "Seasonal_avg.r", "My Plot Title", "visuals/test.png")

----


df_time_lag |>
  filter()

df_winter <- df_time_lag |>
  filter(Season == "Winter")|>
#  pivot_longer(cols=c(Seasonal_avg.WD, Seasonal_avg.WS, Seasonal_avg.VIS, Seasonal_avg.pm10, Seasonal_avg.pm2.5, Seasonal_avg.r), names_to = "name", values_to = "value")

df_spring <- df_time_lag |>
  filter(Season == "Spring")|>
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

png("submission/images/figure_9.png", res = 72, width = 1000, height = 1000, pointsize = 25)

corrplot::corrplot((corr_matrix_DZ), diag = T, col = COL2(n=5), method = 'circle', cl.length = 6, addCoef.col = 'lightgrey' , type = 'upper',
                   cl.pos = 'b',  tl.col="black", tl.pos = "lt", tl.srt=0, order = 'hclust',tl.offset = 1,
                   p.mat = as.matrix(pvalues_DZ$P),
                   addrect = 2,
                   insig = 'label_sig',  sig.level = c(0.001, 0.01, 0.05),
                   pch.cex = 1.9, pch.col = 'white', mar=c(0,0,1,1))
corrplot::corrplot((corr_matrix_UB), diag = F, col = COL2(n=5), method = 'square', cl.length = 6, addCoef.col = 'grey' , type = 'lower',
                     cl.pos = 'n', tl.pos = "n", order = 'original',
                     p.mat = as.matrix(pvalues_UB$P),
                     insig = 'label_sig',  sig.level = c(0.001, 0.01, 0.05),
                     pch.cex = 1.9, pch.col = "white", mar=c(0,0,1,0),
                    add = TRUE)
dev.off()

plot_3 <- corrplot::corrplot((corr_matrix), diag = F, add = TRUE, col = COL2(n=5), type = 'lower', method = 'circle', order = 'AOE',
                      cl.pos = 'n', tl.col="black", 
                     tl.srt=45, 
                #  p.mat = as.matrix(pvalues), 
                     sig.level = 0.05, 
                     insig = "blank")




 ############ supplement figure
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping) + 
    geom_point() + 
    #geom_smooth(method=lm, se= FALSE, alpha = 0.5, fill="red", color="red", ...) +
    geom_smooth(aes(colour = Station.name), method=glm, se= FALSE, size = 0.2) 
  p
}
  
library(GGally)
plot_20250204 <- GGally:: ggpairs(df_corr, columns = 3:6, axisLabels = "none", 
                 ggplot2::aes(colour = Station.name, alpha =0.5),
                 lower = list(continuous = wrap("cor", size = 2.5)),
                   upper = list(continuous = my_fn, display_grid = FALSE),
                 switch = "both") 
ggsave("submission/images/figure_9_supplement.png",plot_20250204, width = 5, height = 5, units = "in", dpi = 300)

  df_corr <- df_corr |>  
    filter(Station.name == "UB") |>
  drop_na()
library(corrplot)
corr_matrix <- cor(df_corr[3:8]) 
library(reshape2)
corr_melted <- melt(corr_matrix)
corrplot(corr_matrix, method = "color", col = colorRampPalette(c("red", "white", "blue"))(200))


ggplot(corr_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +  # Creates the heatmap squares
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # Color scale
#  geom_text(aes(label = value)) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

