library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(Hmisc)
library(mice)
library(miceadds)
library(corrplot)
library(lubridate)

# library(tidyr)
# library(cluster)
# library(rgl)
# library(maptools)
# library(ggpubr)
# library(viridis)
# library(tidyverse)
# library(scatterplot3d)
# library(factoextra)
# library(fpc)
# library(NbClust)
# library(reshape2)
# library(scales)
# library(caret)
# library(ROCR)
# library(pROC)
# library(rlist)
# library(corrplot)
# library(ggcorrplot)
# library(DMwR)
# library(ggthemes)
# library(e1071)
# library(maps)
# library(RColorBrewer)
# library(rgl)



data_dir = "data"

### Loading data
data=read.csv(paste(data_dir,"cleaned_data.csv",sep="/"), header = TRUE, sep = ",",na.strings = c("NA", "nan","#NAME?"))

bjmap <- get_map(location =c(left=115.9133, bottom=39.6966, right=116.8252, top=40.1831))
beijingMap <- ggmap(bjmap)

### Data Preprocessing

#reno condition can't be 0 - change to NA 
data$renovationCondition[data$renovationCondition == 0] <- NA

#split floor to have height info 
temp <- as.data.frame(str_split_fixed(data$floor, " ", 2))
colnames(temp) <- c("floorType", "floorLevel")
data <- cbind(data, temp)
data$floorLevel=as.integer(data$floorLevel)

#remove unused columnns
data <- data[, !colnames(data) %in% c("url","id", "floor", "DOM")]

#type conversion
data$tradeTime = ymd(data$tradeTime)
data$drawingRoom = as.integer(data$drawingRoom)
data$bathRoom = as.integer(data$bathRoom)
data$district=as.factor(data$district)
data$constructionTime = as.integer(data$constructionTime)

data$buildingType = factor(mapvalues(data$buildingType, from = c(1, 2, 3, 4), to = c("tower", "bungalow", "plate-tower combination", "plate")))
data$renovationCondition = factor(mapvalues(data$renovationCondition, from = c(1, 2, 3, 4), to = c("other", "rough", "simplicity", "hardcover")))
data$buildingStructure = factor(mapvalues(data$buildingStructure, from = c(1, 2, 3, 4, 5, 6), to = c("unknown", "mixed", "brick and wood", "brick and concrete", "steel", "steel-concrete composite")))
data$elevator = factor(mapvalues(data$elevator, from = c(0, 1), to = c("no", "yes")))
data$fiveYearsProperty = factor(mapvalues(data$fiveYearsProperty, from = c(0, 1), to = c("no", "yes")))
data$subway = factor(mapvalues(data$subway, from = c(0, 1), to = c("no", "yes")))

#ladderRatio probably has outliers

boxplot(data$ladderRatio, las = 1, ylim=c(0,2))

#we can assume everything above a 1:1 ratio is probably an outlier or incorrect

data$ladderRatio[data$ladderRatio > 1] <- NA

#what to do with NAs?

colSums(is.na(data))

#use mice imputation
numeric_cols = sapply(data, is.numeric)
data_num_only=data[,numeric_cols]
cor(data_num_only)
corrplot(cor(data_num_only), method = "circle")

set.seed(12345)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(data[, !names(data) %in% c('Lng', 'Lat', 'Cid', 'followers', 'totalPrice', 'price')], pred = quickpred(data[, !names(data) %in% c('Lng', 'Lat', 'Cid', 'followers', 'totalPrice', 'price')], mincor=0.1, minpuc=0.3)) 

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot distributions
par(mfrow=c(1,2))
hist(data$DOM, freq=F, main='Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$DOM, freq=F, main='MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

hist(as.numeric(data$buildingType), freq=F, main='Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(as.numeric(mice_output$buildingType), freq=F, main='MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

hist(data$ladderRatio, freq=F, main='Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$ladderRatio, freq=F, main='MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

#doesn't work, don't run
hist(as.numeric(data$communityAverage), freq=F, main='Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(as.numeric(mice_output$communityAverage), freq=F, main='MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

hist(data$constructionTime, freq=F, main='Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$constructionTime, freq=F, main='MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

#replace with imputed values

data$DOM <- mice_output$DOM
data$buildingType <- mice_output$buildingType
data$ladderRatio <- mice_output$ladderRatio
data$communityAverage <- mice_output$communityAverage
data$constructionTime <- mice_output$constructionTime

#save the imputation
write.csv(as.data.frame(mice_output), file="data/mice_output.csv")
write.csv(as.data.frame(data), file="data/imputed_data.csv")
