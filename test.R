library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(Hmisc)

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
data=read.csv(paste(data_dir,"new.csv",sep="/"), header = TRUE, sep = ",")

bjmap <- get_map(location =c(left=115.9133, bottom=39.6966, right=116.8252, top=40.1831))
beijingMap <- ggmap(bjmap)

### Data Preprocessing

#format columns because it has chinese characters encoded that we don't need
data$drawingRoom <- as.numeric(gsub("\\D", "", data$drawingRoom))
data$constructionTime <- as.numeric(gsub("\\D", "", data$constructionTime))
data$bathRoom <- as.numeric(gsub("\\D", "", data$bathRoom))
data$livingRoom <- as.numeric(gsub("\\D", "", data$livingRoom))

#construction time has 0 and 1 listed for year of construction - incorrect so change them to NA
data$constructionTime[data$constructionTime < 2] <- NA

#bathroom has what appears to be years - change to NA
data$bathRoom[data$bathRoom > 1000] <- NA

#building type has decimal places when it should not - change to NA
data$buildingType[data$buildingType < 1] <- NA

#you can't have like 20 drawing rooms in one house - change to NA
data$drawingRoom[data$drawingRoom > 10] <- NA

#split floor to have height info 
temp <- as.data.frame(str_split_fixed(data$floor, " ", 2))
colnames(temp) <- c("floorType", "floorLevel")
data <- cbind(data, temp)

#factor elevator, fiveyearsproperty, subway and change to yes/no
data$elevator = factor(mapvalues(data$elevator, from = c(0, 1), to = c("no", "yes")))
data$fiveYearsProperty = factor(mapvalues(data$fiveYearsProperty, from = c(0, 1), to = c("no", "yes")))
data$subway = factor(mapvalues(data$subway, from = c(0, 1), to = c("no", "yes")))

#factorize
for(col in c("buildingType","kitchen","district","bathRoom", "livingRoom", "drawingRoom", "constructionTime")) {
  data[,col] = as.factor(data[,col])
}

#ladderRatio probably has outliers

boxplot(data$ladderRatio, las = 1, ylim=c(0,2))

#we can assume everything above a 1:1 ratio is probably an outlier or incorrect

data$ladderRatio[data$ladderRatio > 1] <- NA

#remove unused columnns
data <- data[, !colnames(data) %in% c("url","id", "floor")]

#what to do with NAs?

colSums(is.na(data))

#DOM, ladder, community, living, drawing, bath use mean/median?

#construction, buildingtype, elevator, subway, fiveyears use knn imputation?