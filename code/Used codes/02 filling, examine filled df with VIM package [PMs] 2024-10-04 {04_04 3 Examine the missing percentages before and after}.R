# 04_04 3 Examine the missing percentages before and after.R



## before df_dataTransform
## after  df_dataImputed1

df_dataTransform |>
  is.na() |>
  colSums()

df_dataImputed |>
  is.na() |>
  colSums()

#### Vim package: visualizing missing value
library(VIM)
df_dataTransform |>
  aggr(combined = TRUE, numbered = TRUE)

df_dataTransform |>
  select(Station.name, pm2.5_Normalized) |>
  spineMiss()

df_dataImputed |>
  select(Station.name, pm2.5_Normalized) |>
  spineMiss()

