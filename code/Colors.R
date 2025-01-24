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


p1_VIS <- p1_VIS  +
  annotate("text", x = rep(c(2015,0 ,0 ), 3), y = rep(c(15000, 0,0 ),3), label = "hghjg", color = "blue", size = 2, hjust = 0)
tst <- c("Winter visibility drops (UB, post-2014)","3", "2", "1")

p1_VIS <- p1_VIS + annotate( "text",
                             
                             # the new dataframe for annotating text 
                             x= annot_df$x, y=annot_df$y, 
                             label=annot_df$label 
)

# creating a dataframe for annotating text 
annot_df <- data.frame(
  x = c(2015, 2015, 2015),
  y = c(10000, 15000, 10000),
  label = c("bottom-left", "top-left", "top-right")
)


# creating facet from Gender and Price variable 
plt <- plt + facet_grid(Gender ~ Price) 

# annotating the graph with the custom label made  
plt + geom_text( 
  
  # the new dataframe for annotating text 
  data = ann_dat_text, 
  label=ann_dat_text$label 
)
