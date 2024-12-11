library(gt)
data()

flight_tbl <- gt(data = meas_info, groupname_col = "Site_type") %>%
  tab_spanner(
    label = "Location",
    columns = c(Coordinate, Elevation)) %>%
  cols_label(WS_WD= "WS&WD") %>%
  tab_footnote(
    footnote = "Data measurement height - 15 m;",
    locations = cells_row_groups(groups = 1)) %>%
  tab_footnote(
    footnote = "Data measurement height - 3 m.",
    locations = cells_row_groups(groups = 2)) %>%
  tab_footnote(
    footnote = "Range: Dust, aerosol, cloud 0–18 km. Lidar L2S-SMII, Shibata, National Institute for Environmental Studies (NIES), Japan;",
    locations = cells_column_labels(columns = Lidar )) %>%
  tab_footnote(
    footnote = "Range: Aerosol number counts;",
    locations = cells_column_labels(columns = OPC)) %>%
  tab_footnote(
    footnote = "Measurement range: 0–60 m s$-1$; 0-365 degrees. Instrument model: Wind speed and direction PGWS-100, Gill, England;",
    locations = cells_column_labels(columns = WS_WD)) %>%
  tab_footnote(
    footnote = "Range: 10–20000 m. Visibility meter PWD10, Vaisala, Finland;",
    locations = cells_column_labels(columns = VIS)) %>%
  tab_spanner(
    label = "Data",
    columns = c(PM2.5, PM10)) %>%
#  tab_stubhead(label = "Site") %>%
  tab_options(table_body.hlines.width = 0, row_group.as_column = TRUE) %>%
  cols_align(align = "center") %>%
  tab_footnote(
    footnote = "Range: 0.003–100 mg m$^-3$, Flow rate: 20 L m-1, Suction rate: 2 L m-1. Measured by Kosa monitor ES-640, TDK Co. LTD, Japan;",
    locations = cells_column_labels(columns = c(PM2.5, PM10) )) %>%
  gt_theme_538() %>% 
  tab_header(title = "Table 1. Measured data") %>%
  tab_options(footnotes.font.size=12, footnotes.multiline = F)
flight_tbl
##################################

flight_tbl <- meas_info %>%
  dplyr::mutate(pm2.5_scaled = round((Total_observation-PM2.5)/Total_observation * 100, digits = 2),
                pm10_scaled = (Total_observation-PM10)/Total_observation * 100) %>%
  gt() %>%
  tab_spanner(
    label = "Location",
    columns = c(Coordinate, Elevation)) %>%
  cols_label(WS_WD= "WS&WD", Total_observation = "Total", 
             pm2.5_scaled = "pm2.5", 
             pm10_scaled = "pm10") %>%
#  tab_footnote(
#    footnote = "Data measurement height - 15 m;",
#    locations = cells_row_groups(groups = 1)) %>%
#  tab_footnote(
#    footnote = "Data measurement height - 3 m.",
#    locations = cells_row_groups(groups = 2)) %>%
##  tab_footnote(
##    footnote = "Range: Dust, aerosol, cloud 0–18 km. Lidar L2S-SMII, Shibata, National Institute for Environmental Studies (NIES), Japan;",
##    locations = cells_column_labels(columns = Lidar )) %>%
##  tab_row_group(label = "Gobi", rows = which(Type == "Gobi")) %>%
##  tab_footnote(
##    footnote = "All equipments were installed at 15 m;",
##    locations = cells_body(columns = Type, rows = 1)) %>%
  tab_footnote(
    footnote = "Equipment height: 15 meter at urban site (Ulaanbaatar), 2 meter at Gobi sites (Dalanzadgad, Sainshand and Zamyn Uud);",
    locations = cells_column_labels(columns = Total_observation)) %>%
  tab_spanner(
    label = "Measured and collected data",
    columns = c(Total_observation, WS_WD, VIS, OPC, PM2.5, PM10)) %>%
  gt_plt_bar_pct(column = pm2.5_scaled, labels =TRUE, scaled = TRUE,   fill = "lightblue",
                 background = "#e1e1e1") %>%
  gt_plt_bar_pct(column = pm10_scaled, labels =TRUE, fill = "lightblue", scaled = TRUE) %>%
  tab_footnote(
    footnote = "Optical Particle Counter;",
    locations = cells_column_labels(columns = OPC)) %>%
  tab_footnote(
    footnote = "Measurement range: 0–60 m/s; 0-365 degrees. Instrument model: Wind speed and direction PGWS-100, Gill, England;",
    locations = cells_column_labels(columns = WS_WD)) %>%
  tab_footnote(
    footnote = "Range: 10–20 000 m. Visibility meter PWD10, Vaisala, Finland;",
    locations = cells_column_labels(columns = VIS)) %>%
  tab_spanner(
    label = "Missing data",
    columns = c(pm2.5_scaled, pm10_scaled)) %>%
  #  tab_stubhead(label = "Site") %>%
  tab_options(table_body.hlines.width = 0, row_group.as_column = TRUE) %>%
  cols_align(align = "center") %>%
  tab_footnote(
    footnote = "Range: 0.003–100 mg/m3, Flow rate: 20 L/m, Suction rate: 2 L/ m. Measured by Kosa monitor ES-640, TDK Co. LTD, Japan;",
    locations = cells_column_labels(columns = c(PM2.5, PM10) )) %>%
  gt_theme_538() %>% 
  cols_width(
   starts_with("C") ~ px(125),
   starts_with("E") ~ px(80),
   starts_with("S") ~ px(75),
   starts_with("W") ~ px(64),
    everything() ~ px(60)
  ) %>% 
  tab_header(title = "Table 1. Measured data") %>%
  tab_options(footnotes.font.size=11, footnotes.multiline = F, 
              table.font.size =13,
#              column_labels.font.size =13,
#              source_notes.font.size=10
              ) 
flight_tbl

#gtsave(flight_tbl, file = "/Users/munkhtsetseg/WORK/Research/Data/03_tables/table_data_NIES.docx")
gtsave(flight_tbl, file = "/Users/munkhtsetseg/WORK/Research/Data/03_tables/table_data_NIES.tex")
gtsave(flight_tbl, file = "/Users/munkhtsetseg/WORK/Research/Data/03_tables/table_data_NIES.png")
gtsave(flight_tbl, file = "/Users/munkhtsetseg/WORK/Research/Data/03_tables/table_data_NIES.pdf")
################ Flextable
library(tidyverse)  
library(flextable)
set_flextable_defaults(
  font.family = "Arial", font.size = 10, 
  border.color = "gray")

flextable(head(cars)) %>% 
  bold(part = "header") %>% 
  add_footer_lines("The 'cars' dataset")

ggplot2::diamonds |> 
  with(table(cut, color)) |> 
  as_flextable()

library(ggplot2)
library(gtExtras)
mtcars %>%
  dplyr::group_by(cyl) %>%
  # must end up with list of data for each row in the input dataframe
  dplyr::summarize(mpg_data = list(mpg), .groups = "drop") %>%
  gt() %>%
  gt_plt_dist(mpg_data)

