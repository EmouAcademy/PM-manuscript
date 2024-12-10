library(ggplot2)
library(readr)
library(ggbreak)      # scale_y_break
source(here("code/Colors.R"))

####### input DATA 
data <- read.csv("data/processed/df.csv")

## add order and levels
data$Station.name <- factor(data$Station.name, 
                            levels = c("UB", "DZ",  "ZU", "SS"))  
levels(data$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')
#### hourly data for UB&DZ


########################### Plotting










########################### Output


ggsave("visuals/fig3_2_pca.png", p3_daily_01, width=8, height=5, unit="in", dpi = 300)