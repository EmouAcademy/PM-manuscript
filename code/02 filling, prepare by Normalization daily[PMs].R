library(tidyverse)
library(ggplot2)



dataTransform <- df_cleaned_monthly_PMs |>
  select(Station.name,  Year, Month, Day, Hour, Visibility, WD, WS, OPC, pm10, pm2.5)
summary(dataTransform)
### change `Visibility and WD` variable "int" to "num"
dataTransform$Visibility <- as.numeric(dataTransform$Visibility)
dataTransform$WD <- as.numeric(dataTransform$WD)
 
df_UB <- df_cleaned_monthly_PMs |>
  filter(Station.name=="UB")
df_DZ <- df_cleaned_monthly_PMs |>
  filter(Station.name=="DZ")
df_SS <- df_cleaned_monthly_PMs |>
  filter(Station.name=="SS")
df_ZU <- df_cleaned_monthly_PMs |>
  filter(Station.name=="ZU")
### 1. Check skewness to detect whether it is positively skewed, or negatively skewed.
library(moments)
skewness(df_SS$Visibility, na.rm = TRUE) ### -1.661681, -1.344332, -0.881717, -4.724538, -1.401889
skewness(df_SS$WD, na.rm = TRUE)  ### -0.4325835, 0,002674744, -0.7307861, -0.8549079, -0.3170689
skewness(df_SS$WS, na.rm = TRUE)  ### 2.090158, 1.376805, 3.401697, 1.027718, 1.390244
skewness(df_SS$pm10, na.rm = TRUE)  ### -1.661681, 4.142555, 8.612916, 89.21587, 10.81432
skewness(df_SS$pm2.5, na.rm = TRUE)  ### 5.354227, 3.654768, 5.098534, 19.46194, 7.515332

### 2. Transform data to Normal distribution according to the skewness.
### |a. square-root for moderate skew:
        ## positively skewed data "sqrt(x)"; 
        ## negatively skewed data "sqrt(max(x+1) - x)";  
### ||b. square-root for greater skew:
        ## positively skewed data "log10(x)"; 
        ## negatively skewed data "log10(max(x+1) - x)";
### |||c. square-root for extreme skew:
        ## positively skewed data "1/(x)"; 
        ## negatively skewed data "1/(max(x+1) - x)"; 

df_UB_norm <- dataTransform |>
  filter(Station.name=="UB") |>
  mutate(Visibility_Normalized = sqrt(20001 - Visibility), # -1.344332, negatively skewed data "sqrt(max(x+1) - x)";  
         WD_Normalized = sqrt(WD),  # 0,002674744, positively skewed data "sqrt(x)"; 
         WS_Normalized = sqrt(WS),  # 1.376805, positively skewed data "sqrt(x)"; 
         pm10_Normalized = sqrt(pm10),  # 4.142555, positively skewed data "sqrt(x)"; 
         pm2.5_Normalized = sqrt(pm2.5) # 3.654768, positively skewed data "sqrt(x)"; 
          )

df_DZ_norm <- dataTransform |>
  filter(Station.name=="DZ") |>
  mutate(Visibility_Normalized = sqrt(20001 - Visibility), # -0.881717, negatively skewed data "sqrt(max(x+1) - x)";  
         WD_Normalized = sqrt(361 - WD), #-0.7307861, negatively skewed data "sqrt(max(x+1) - x)";  
         WS_Normalized = sqrt(WS), # 3.401697, positively skewed data "sqrt(x)"; 
         pm10_Normalized = sqrt(pm10), # 8.612916, positively skewed data "sqrt(x)"; 
         pm2.5_Normalized = sqrt(pm2.5)  # 5.098534, positively skewed data "sqrt(x)"; 
  )

df_SS_norm <- dataTransform |>
  filter(Station.name=="SS") |> 
  mutate(Visibility_Normalized = sqrt(20001 - Visibility),  #  -4.724538, negatively skewed data "sqrt(max(x+1) - x)";  
         WD_Normalized = sqrt(361 - WD), # -0.8549079, negatively skewed data "sqrt(max(x+1) - x)";  
         WS_Normalized = sqrt(WS), # 1.027718, positively skewed data "sqrt(x)"; 
         pm10_Normalized = sqrt(pm10), # 89.21587, positively GREATER skewed data "log10(x)"; 
         pm2.5_Normalized = sqrt(pm2.5)  # 19.46194, positively skewed data "sqrt(x)"; 
  )


df_ZU_norm <- dataTransform |>
  filter(Station.name=="ZU") |>
  mutate(Visibility_Normalized = sqrt(20001 - Visibility), # -1.401889, negatively skewed data "sqrt(max(x+1) - x)";
         WD_Normalized = sqrt(361 - WD),  # -0.3170689, negatively skewed data "sqrt(max(x+1) - x)";
         WS_Normalized = sqrt(WS),  # 1.390244, positively skewed data "sqrt(x)";  
         pm10_Normalized = sqrt(pm10), # 10.81432, positively skewed data "sqrt(x)"; 
         pm2.5_Normalized = sqrt(pm2.5) # 7.515332, positively skewed data "sqrt(x)"; 
  )

### combine transformed data into a SINGLE dataset

df_dataTransform <- df_UB_norm |> 
                    bind_rows(df_DZ_norm) |> 
                    bind_rows(df_SS_norm) |> 
                    bind_rows(df_ZU_norm)







