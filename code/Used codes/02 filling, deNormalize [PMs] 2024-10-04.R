######### 1. deNormalize dataset
#### UB
# DE-NORMALIZE
df_UB_comp_deNormalized <- df_UB_comp |>
  mutate(
    # Visibility_Normalized = sqrt(20001 - Visibility), # -1.344332, negatively skewed data "sqrt(max(x+1) - x)"; 
    Visibility_deNormalized = 20001 - Visibility_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    #  WD_Normalized = sqrt(WD),  # 0,002674744, positively skewed data "sqrt(x)"; 
    WD_deNormalized = WD_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    #  WS_Normalized = sqrt(WS),  # 1.376805, positively skewed data "sqrt(x)"; 
    WS_deNormalized = WS_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    #  pm10_Normalized = sqrt(pm10),  # 4.142555, positively skewed data "sqrt(x)"; 
    pm10_deNormalized = pm10_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    #  pm2.5_Normalized = sqrt(pm2.5) # 3.654768, positively skewed data "sqrt(x)"; 
    pm2.5_deNormalized = pm2.5_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
  )

#### DZ
# DE-NORMALIZE
df_DZ_comp_deNormalized <- df_DZ_comp |>
  mutate(
    # Visibility_Normalized = sqrt(20001 - Visibility), # -0.881717, negatively skewed data "sqrt(max(x+1) - x)"; 
    Visibility_deNormalized = 20001 - Visibility_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    # WD_Normalized = sqrt(361 - WD), #-0.7307861, negatively skewed data "sqrt(max(x+1) - x)";
    WD_deNormalized = 361 - WD_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";    
    
    #  WS_Normalized = sqrt(WS), # 3.401697, positively skewed data "sqrt(x)"; 
    WS_deNormalized = WS_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    # pm10_Normalized = sqrt(pm10), # 8.612916, positively skewed data "sqrt(x)"; 
    pm10_deNormalized = pm10_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    #  pm2.5_Normalized = sqrt(pm2.5)  # 5.098534, positively skewed data "sqrt(x)"; 
    pm2.5_deNormalized = pm2.5_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
  )

#### SS
# DE-NORMALIZE
df_SS_comp_deNormalized <- df_SS_comp |>
  mutate(
    #Visibility_Normalized = sqrt(20001 - Visibility),  #  -4.724538, negatively skewed data "sqrt(max(x+1) - x)";        Visibility_deNormalized = 20001-Visibility_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    # WD_Normalized = sqrt(361 - WD), # -0.8549079, negatively skewed data "sqrt(max(x+1) - x)";  
    WD_deNormalized = 361 - WD_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    # WS_Normalized = sqrt(WS), # 1.027718, positively skewed data "sqrt(x)"; 
    WS_deNormalized = WS_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    # pm10_Normalized = sqrt(pm10), # 89.21587, positively GREATER skewed data "log10(x)"; 
    pm10_deNormalized = pm10_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    # pm2.5_Normalized = sqrt(pm2.5)  # 19.46194, positively skewed data "sqrt(x)"; 
    pm2.5_deNormalized = pm2.5_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
  )

#### ZU
df_ZU_comp_deNormalized <- df_ZU_comp |>
  mutate(
    # Visibility_Normalized = sqrt(20001 - Visibility), # -1.401889, negatively skewed data "sqrt(max(x+1) - x)";
    Visibility_deNormalized = 20001 - Visibility_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    #  WD_Normalized = sqrt(361 - WD),  # -0.3170689, negatively skewed data "sqrt(max(x+1) - x)";
    WD_deNormalized = 361 - WD_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    #  WS_Normalized = sqrt(WS),  # 1.390244, positively skewed data "sqrt(x)";  
    WS_deNormalized = WS_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    #  pm10_Normalized = sqrt(pm10), # 10.81432, positively skewed data "sqrt(x)"; 
    pm10_deNormalized = pm10_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
    
    #  pm2.5_Normalized = sqrt(pm2.5) # 7.515332, positively skewed data "sqrt(x)"; 
    pm2.5_deNormalized = pm2.5_Normalized^2,  # 0,002674744, positively skewed data "sqrt(x)";
  )






# --------------------------- END of the GOAL ---------------------------

