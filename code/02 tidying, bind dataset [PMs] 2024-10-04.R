### 2. combine imputed data into a SINGLE dataset
df_dataImputed <- df_UB_comp_deNormalized |> 
  bind_rows(df_DZ_comp_deNormalized) |> 
  bind_rows(df_SS_comp_deNormalized) |> 
  bind_rows(df_ZU_comp_deNormalized)

### 3. alter variable name; so that do not duplicate with the input data
df_dataImputed1 <- df_dataImputed |>
  rename(
    Visibility_Imputed = Visibility_Normalized,
    WD_Imputed = WD_Normalized,
    WS_Imputed = WS_Normalized,
    pm10_Imputed = pm10_Normalized,
    pm2.5_Imputed = pm2.5_Normalized
  )




### Combine 2 datasets into a single dataset
## - df_dataTransform
## - df_imputed

# Version 1. cbind, rbind
df_combined_01 <- cbind(df_dataTransform, df_dataImputed1)

# Version 2. [dplyr:: bind_rows or bind_cols] Bind any number of data frames by row, making a longer result. 
# This is similar to do.call(rbind, dfs), 
# but the output will contain all columns that appear in any of the inputs.
df_combined <- df_dataTransform |> 
  bind_cols(df_dataImputed1) 
