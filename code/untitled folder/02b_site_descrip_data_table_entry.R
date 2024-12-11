# install.packages("rempsyc")
library(rempsyc)

pkgs <- c("flextable", "broom", "report", "effectsize")
install_if_not_installed(pkgs)

nice_table(
  meas_info[1:3,],
  title = c("Table 1", "Motor Trend Car Road Tests"),
  note = c(
    "The data was extracted from the 1974 Motor Trend US magazine.",
    "* p < .05, ** p < .01, *** p < .001"
  )
)

library(gtsummary)

meas_info |>
  tbl_summary(by = "Site")

library(tidyverse)
meas_info_long <- meas_info |> 
                   pivot_longer(
                   cols = starts_with("Site"), 
                    names_to = "week", 
                   values_to = "Site"
                                     )
stats.table <- nice_mod(
  data = meas_info,
  response = "Site",
  predictor = "gear",
  moderator = "wt"
)
stats.table
nice_table(stats.table)

names(meas_info) <- c("Site", "Location", "Data type", "Measurement height", "Data period", "Instrumetn", "range")
meas_info[, 10:11] <- test[, 10:11] / 10
nice_table(test)


meas_info <- data.frame(
  Site = c("Ulaanbaatar", "Dalanzadgad", "Sainshand", "Zamyn Uud"),
#  Type = c("Urban", "Gobi", "Gobi", "Gobi"),
  Coordinate = c("47.92°N, 106.92°E", "43.57°N,
104.42°E", "44.87°N, 110.12°E", "43.72°N, 111.90°E"),
  Elevation = c("1350 m", "1470 m", "947 m", "967 m"),
  stringsAsFactors = T
) 

Table_NA_by_Station$Station.name[Table_NA_by_Station$Station.name == "UB"] <- "Ulaanbaatar"
Table_NA_by_Station$OPC[Table_NA_by_Station$OPC == "0"] <- "-"

observation_number <- Table_NA_by_Station |>
  arrange((Station.name))

meas_info <- meas_info |>
  arrange(Site)

data <- cbind.data.frame(meas_info, observation_number)

meas_info <- data |>
  select(-Station.name) |>
  arrange(desc(OPC))
 
meas_info |>
  dplyr::mutate(pm2.5_scaled = round(PM2.5/Total_observation * 100, digits = 2),
                pm10_scaled = PM10/Total_observation * 100) %>%
  dplyr::mutate(observ_unscaled = Total_observation) %>%
  gt() %>%
  gt_plt_bar_pct(column = pm2.5_scaled, scaled = TRUE) %>%
  gt_plt_bar_pct(column = observ_unscaled, scaled = FALSE, fill = "blue", background = "lightblue") %>%
  cols_align("center", contains("scale")) %>%
  cols_width(4 ~ px(125),
             5 ~ px(125))
