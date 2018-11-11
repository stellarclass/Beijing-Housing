library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(Hmisc)
library(mice)

# library(tidyr)
# library(cluster)
# library(lubridate)
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
data=read.csv(paste(data_dir,"original_data.csv",sep="/"), header = TRUE, sep = ",",na.strings = c("nan","#NAME?"))

bjmap <- get_map(location =c(left=115.9133, bottom=39.6966, right=116.8252, top=40.1831))
beijingMap <- ggmap(bjmap)

### Data Preprocessing

#factorize everything
for(col in c("buildingType","kitchen","district","bathRoom", "livingRoom", "drawingRoom", "constructionTime", "renovationCondition", "elevator", "fiveYearsProperty", "subway", "district")) {
  data[,col] = as.factor(data[,col])
}

#first, let's take a look at NA

colSums(is.na(data))

#looks like there's a consistent number of NA values for some columns so let's take a closer look
NAdata <- which(is.na(data$subway))
data[NAdata, ]

#looks like some values were swapped! let's fix this up
#thanks to having the URLs, we can see how things were mixed up
#looks like there is no room data input for the NA rows 

data$subway[NAdata] <- data$buildingStructure[NAdata]

#convert to numbers - reinforced = 6, hybrid = 1

BS6 <- which(data$floor == "Reinforced Concrete Structure")
BS1 <- which(data$floor == "Hybrid Structure")
data$buildingStructure[BS6] <- 6
data$buildingStructure[BS1] <- 1

data$floor[NAdata] <- data$drawingRoom[NAdata]
data$drawingRoom[NAdata] <- NA

data$elevator[NAdata] <- data$constructionTime[NAdata]
data$constructionTime[NAdata] <- data$bathRoom[NAdata]
data$bathRoom[NAdata] <- NA

data$fiveYearsProperty[NAdata] <- data$renovationCondition[NAdata]
data$renovationCondition[NAdata] <- data$ladderRatio[NAdata]
data$ladderRatio[NAdata] <- data$buildingType[NAdata]
data$buildingType[NAdata] <- data$kitchen[NAdata]
data$kitchen[NAdata] <- NA

data$livingRoom[NAdata] <- NA

#reno condition can't be 0 - change to NA 
data$renovationCondition[data$renovationCondition == 0] <- NA

#split floor to have height info 
temp <- as.data.frame(str_split_fixed(data$floor, " ", 2))
colnames(temp) <- c("floorType", "floorLevel")
data <- cbind(data, temp)

#factor elevator, fiveyearsproperty, subway and change to yes/no
data$elevator = factor(mapvalues(data$elevator, from = c(0, 1), to = c("no", "yes")))
data$fiveYearsProperty = factor(mapvalues(data$fiveYearsProperty, from = c(0, 1), to = c("no", "yes")))
data$subway = factor(mapvalues(data$subway, from = c(0, 1), to = c("no", "yes")))

#ladderRatio probably has outliers

boxplot(data$ladderRatio, las = 1, ylim=c(0,2))

#we can assume everything above a 1:1 ratio is probably an outlier or incorrect

data$ladderRatio[data$ladderRatio > 1] <- NA

#remove unused columnns
data <- data[, !colnames(data) %in% c("url","id", "floor")]

#what to do with NAs?

colSums(is.na(data))

#ladder, rooms use mean/median?
data$ladderRatio=impute(data$ladderRatio, mean)
data$livingRoom=impute(data$livingRoom, mean)
data$drawingRoom=impute(data$drawingRoom, mean)
data$kitchen=impute(data$kitchen, mean)
data$bathRoom=impute(data$bathRoom, mean)

#buildingtype, community, DOM use mice imputation
set.seed(12345)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(data[, !names(data) %in% 'totalPrice'], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)
