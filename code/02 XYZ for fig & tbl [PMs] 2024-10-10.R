##########    DATA PREPARE#######################################################
# 1. df_imported_PMs
### a. df_imported_hourly_PMS
### b. df_imported_daily_PMs
### c. df_imported_annual_PMs
### d. df_imported_monthly_PMs
# 2. df_tidy_PMs
### a. df_clean_hourly_PMS
### b. df_tidy_daily_PMs
### c. df_tidy_annual_PMs
### d. df_tidy_monthly_PMs
# 3. df_clean_PMs or df_mined_PMs
### a.   df_clean_hourly_PMS
### b. df_clean_daily_PMs
### c. df_clean_annual_PMs
### d. df_clean_monthly_PMs
# 4. df_filled_PMs
### a. df_filled_hourly_PMS
### b. df_filled_daily_PMs
### c. df_filled_annual_PMs
### d. df_filled_monthly_PMs
# 5. df_analyzed_PMs
### a. df_analyzed_hourly_PMS
### b. df_analyzed_daily_PMs
### c. df_analyzed_annual_PMs
### d. df_analyzed_monthly_PMs
###########################################################################
path_out = "/Users/munkhtsetseg/WORK/Research/Data/2 Areas/02 df, XYZ/"
CurrentDate <- Sys.Date()

# 1. df_imported_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_imported_PMS_",CurrentDate,".csv"),
               sep = ''), row.names = T)
### a. df_imported_hourly_PMS
     write.csv(df_ , 
               paste(path_out,paste0("df_imported_hourly_PMS_",CurrentDate,".csv"),
               sep = ''), row.names = T)     
### b. df_imported_daily_PMs
     write.csv(df_ , 
      paste(path_out,paste0("df_imported_daily_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)     
     
### c. df_imported_annual_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_imported_annual_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)     
     
     ### d. df_imported_monthly_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_imported_monthly_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)     
     
# 2. df_tidy_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_tidy_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)     
     
### a. df_tidy_hourly_PMS     
     write.csv(df_ , 
     paste(path_out,paste0("df_tidy_hourly_PMS_",CurrentDate,".csv"),
           sep = ''), row.names = T)     

### b. df_tidy_daily_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_tidy_daily_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)     
     
### c. df_tidy_annual_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_tidy_annual_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)          
### d. df_tidy_monthly_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_tidy_monthly_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)     
# 3. df_clean_PMs or df_mined_PMs
     write.csv(df_clean, 
               paste(path_out,paste0("df_clean_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)     
### a.   df_cleaned_hourly_PMS
     write.csv(df_ , 
               paste(path_out,paste0("df_cleaned_hourly_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)     
### b. df_cleaned_daily_PMs
     write.csv(df_cleaned_daily_PMs , 
               paste(path_out,paste0("df_cleaned_daily_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### c. df_cleaned_annual_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_cleaned_annual_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### d. df_cleaned_monthly_PMs
     write.csv(df_cleaned_monthly_PMs , 
               paste(path_out,paste0("df_cleaned_monthly_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
# 4. df_filled_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_filled_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### a. df_filled_hourly_PMS
     write.csv(df_ , 
               paste(path_out,paste0("df_filled_hourly_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### b. df_filled_daily_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_filled_daily_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### c. df_filled_annual_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_filled_annual_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### d. df_filled_monthly_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_filled_monthly_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
# 5. df_analyzed_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_analyzed_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### a. df_analyzed_hourly_PMS
     write.csv(df_ , 
               paste(path_out,paste0("df_analyzed_hourly_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### b. df_analyzed_daily_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_analyzed_daily_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### c. df_analyzed_annual_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_analyzed_annual_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  
### d. df_analyzed_monthly_PMs
     write.csv(df_ , 
               paste(path_out,paste0("df_analyzed_monthly_PMS_",CurrentDate,".csv"),
                     sep = ''), row.names = T)  

###############################################################################     
 

#### DAILY data
df_cleaned_daily_PMs <- df_clean |>
  group_by(Station.name, Date) |>
  summarise(across(where(is.numeric), ~ mean(.x,  by=c(Station.name, Date),na.rm = F)))

       
##### Monthly data
df_cleaned_monthly_PMs <- df_clean |>
  group_by(Station.name, Year, Month) |>
  summarise(across(where(is.numeric), ~ mean(.x, by=c(Station.name, Year, Month), na.rm = TRUE)))



##### Hourly data
df_hour_mean <- data |>
  group_by(Station.name, Hour) |>
  summarise(across(where(is.numeric), ~ mean(.x, by=Hour, na.rm = TRUE)))



long_data <- data %>% 
  pivot_longer(
    cols = c("pm10","pm2.5"),
    names_to = "PMs",
    names_prefix = "",
    values_to = "pm",
    values_drop_na = F
  )

long_daily_data <- daily_data %>% 
  pivot_longer(
    cols = c("pm10","pm2.5"),
    names_to = "PMs",
    names_prefix = "",
    values_to = "pm",
    values_drop_na = F
  )

long_daily_data |>
  ggplot(aes(Month, pm, fill= PMs, group= Month, shape = Station.name)) +
  geom_boxplot() +
  facet_wrap(PMs~Station.name, scales =  'free_y', ncol=1) +
  labs(x = "Date", y = "PM10")

######## Add DATETIME
data <- df02out %>% 
  mutate(Datetime = make_datetime(Year, Month, Day, Hour))
df02in <- df02in %>% 
  mutate(Datetime = make_datetime(Year, Month, Day, Hour))
# glimpse(df1)

########## levels

data$Station.name <- factor(data$Station.name, 
                            levels = c("UB", "Dalanzadgad",  "Zamynuud", "Sainshand"))  
levels(data$Station.name) <- c('UB', 'DZ', 'ZU', 'SS')



####### DATA 

