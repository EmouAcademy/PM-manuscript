custom_color_scale <- function(){
  scale_color_manual(
             #      breaks = c("UB", "DZ", "SS", "ZU"),
                   values = c('#937f52','#c4b485', '#b88d2d','#dc9b09')
              #     labels = c("UB", "DZ", "SS", "ZU")
             )
}


seasonal_color_scale <- function(){
  scale_color_manual(
    #      breaks = c("UB", "DZ", "SS", "ZU"),
    values = c('#a6611a','#dfc27d', '#80cdc1','#018571')
    #     labels = c("UB", "DZ", "SS", "ZU")
  )
}