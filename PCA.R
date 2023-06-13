## Final Project
# IT 270
# Omar Qureshi

electDF <- read.csv('county_statistics.csv', header = T, stringsAsFactors = F)
electDF <- electDF[,-1]

##Clean Data
dim(electDF)
head(electDF)
summary(electDF)

electDF = electDF[complete.cases(electDF),] #keeps only complete observations
summary(electDF)

#original df had 4867 obs, now 3046 obs.

## Outlier Check
#no outliers


##### PCA #####
library(FactoMineR)
library(factoextra)
source("http://blogs.5eanalytics.com/RCode/PCA_functions.R")

#remove categorical variables from DF for PCA
electDF_pca = electDF[, -c(1,2)]

pca_result = PCA(electDF_pca, scale.unit=T, graph=T, ncp=15) 
summary(pca_result)
pca_result$eig
pca_result$var
pca_result$var$coord

#screeplot criterion
fviz_screeplot(pca_result, ncp=15)

# Get the communalities
communality(pca_result)

#Nicely display the PC's
display_pc(pca_result,cutoff = .2)
display_pc(pca_result)
pca_result$var

#varimax Rotation and place back 
loadings.pcarot= varimax(pca_result$var$coord)$loadings
pca_result$var$coord = loadings.pcarot
plot(pca_result, choix  ="var")
display_pc(pca_result)
display_pc(pca_result, cutoff = .43)



