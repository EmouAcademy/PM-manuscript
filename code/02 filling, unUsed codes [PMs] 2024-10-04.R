
df_UB_amelia <- amelia(df_UB_norm, cs = "Station.name")
plot(df_SS_amelia)

########## long form with pivot_longer
df_UB_norm_LF <- df_UB_norm |>
  pivot_longer(cols = c(Visibility_Normalized, WD_Normalized, 
                        WS_Normalized) , 
               names_to = "Meteorological_variables", 
               values_to = "Normalized_values" ) 

df_DZ_norm_LF <- df_DZ_norm |>
  pivot_longer(cols = c(Visibility_Normalized, WD_Normalized, 
                        WS_Normalized) , 
               names_to = "Meteorological_variables", 
               values_to = "Normalized_values" )

df_SS_norm_LF <- df_SS_norm |>
  pivot_longer(cols = c(Visibility_Normalized, WD_Normalized, 
                        WS_Normalized) , 
               names_to = "Meteorological_variables", 
               values_to = "Normalized_values" )

df_ZU_norm_LF <- df_ZU_norm |>
  pivot_longer(cols = c(Visibility_Normalized, WD_Normalized, 
                        WS_Normalized) , 
               names_to = "Meteorological_variables", 
               values_to = "Normalized_values" )

p1 <- ggplot(data=df_UB_norm_LF, aes(Normalized_values,
                                     group=Meteorological_variables, fill=Meteorological_variables)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
p1
#######
x = -10:10
y = dnorm(x, mean = 0, sd = 3)
df.norm = data.frame('x' = x, 'y' = y)
random = data.frame('x' = rnorm(1000, mean = 0, sd = 3))