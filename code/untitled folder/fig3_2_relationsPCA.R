install.packages("MASS")
install.packages("factoextra")
install.packages("ggplot2")

# Load Libraries 
library(MASS) 
library(factoextra)
library(ggplot2)


#Import biopsy data 
data(biopsy)
dim(biopsy)

#Structure of Data 
str(biopsy)
summary(biopsy)


#Delete Cases with Missingness  
biopsy_nomiss <- na.omit(biopsy)
daily_data_to_miss <- daily_data[, c(-2,-1, -6, -14, -17)]
daily_data_sample <- na.omit(daily_data_to_miss)
daily_data_sample[is.na(daily_data_sample)] <- 0
infs <- apply(daily_data_sample, 2, is.infinite)
rowswithinf <- rowSums(infs) > 0
#df[infs]  #handle infinite rows
#df[rowswithinf, ]  #Handle infinite rows
daily_data_sample <- daily_data_sample[!rowswithinf, ]

# Handle columns that almost only have 1 value:
nzv <- sapply(daily_data_sample, var) < 10^-13
daily_data_sample <- daily_data_sample[, !nzv]

#Exclude Categorical Data
biopsy_sample <- biopsy_nomiss[,-c(1,11)]

#Run PCA
biopsy_pca <- prcomp(biopsy_sample, 
                     scale = TRUE)
daily_data_pca <- prcomp(log(daily_data_sample+1), 
                     scale = TRUE)

str(daily_data_sample)

#Summary of Analysis 
summary(biopsy_pca)
summary(daily_data_pca)

#Elements of PCA object 
names(biopsy_pca)

#Std Dev of Components 
biopsy_pca$sdev

#Eigenvectors 
biopsy_pca$rotation

#Std Dev and Mean of Variables 
biopsy_pca$center
biopsy_pca$scale

#Principal Component Scores
biopsy_pca$x


#Scree Plot of Variance 
fviz_eig(biopsy_pca, 
         addlabels = TRUE,
         ylim = c(0, 70))

#Biplot with Default Settings
fviz_pca_biplot(biopsy_pca)

#Biplot with Labeled Variables
fviz_pca_biplot(biopsy_pca,
                label="var")

#Biplot with Colored Groups
fviz_pca_biplot(biopsy_pca,
                label="var",
                habillage = biopsy_nomiss$class)

#Biplot with Customized Colored Groups and Variables
fviz_pca_biplot(biopsy_pca,
                label="var",
                habillage = biopsy_nomiss$class, 
                col.var = "black") +
  scale_color_manual(values=c("orange", "purple"))
