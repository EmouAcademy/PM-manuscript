library(mice)
######### 1. Compute mice to fill missing data
#### UB
df_UB_mice_imp <- mice(df_UB_norm)
df_UB_comp <- complete(df_UB_mice_imp)

#### DZ
df_DZ_mice_imp <- mice(df_DZ_norm)
df_DZ_comp <- complete(df_DZ_mice_imp)


#### SS
df_SS_mice_imp <- mice(df_SS_norm)
df_SS_comp <- complete(df_SS_mice_imp)

#### ZU
df_ZU_mice_imp <- mice(df_ZU_norm)
df_ZU_comp <- complete(df_ZU_mice_imp)


# --------------------------- END of the GOAL ---------------------------


