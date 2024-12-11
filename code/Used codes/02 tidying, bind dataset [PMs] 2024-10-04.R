### 2. combine imputed data into a SINGLE dataset
df_dataImputed <- df_UB_comp_deNormalized |> 
  bind_rows(df_DZ_comp_deNormalized) |> 
  bind_rows(df_SS_comp_deNormalized) |> 
  bind_rows(df_ZU_comp_deNormalized)




### Combine 2 datasets into a single dataset
## - df_dataTransform
## - df_imputed

# Version 1. cbind, rbind
df_combined_01 <- cbind(df_dataTransform, df_dataImputed)

# Version 2. [dplyr:: bind_rows or bind_cols] Bind any number of data frames by row, making a longer result. 
# This is similar to do.call(rbind, dfs), 
# but the output will contain all columns that appear in any of the inputs.
df_combined <- df_dataTransform |> 
  bind_cols(df_dataImputed) 



