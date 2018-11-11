 library(plyr)
 library(dplyr)
 library(ggplot2)
 library(ggmap)
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
# library(Hmisc)
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

#remove unused columnns
data <- data[, !colnames(data) %in% c("url","id", "Cid")]

#format columns because it has chinese characters encoded that we don't need
data$floor <- as.factor(as.numeric(gsub("\\D", "", data$floor)))
data$drawingRoom <- as.factor(as.numeric(gsub("\\D", "", data$drawingRoom)))
data$constructionTime <- as.factor(as.numeric(gsub("\\D", "", data$constructionTime)))
data$bathRoom <- as.factor(as.numeric(gsub("\\D", "", data$bathRoom)))
data$livingRoom <- as.factor(as.numeric(gsub("\\D", "", data$livingRoom)))

#construction time has 0 and 1 listed for year of construction - incorrect so change them to NA
levels(data$constructionTime)[levels(data$constructionTime)=="0"] <- NA
levels(data$constructionTime)[levels(data$constructionTime)=="1"] <- NA

#bathroom has what appears to be years - change to NA
levels(data$bathRoom)[levels(data$bathRoom)=="1990"] <- NA
levels(data$bathRoom)[levels(data$bathRoom)=="1994"] <- NA
levels(data$bathRoom)[levels(data$bathRoom)=="1996"] <- NA
levels(data$bathRoom)[levels(data$bathRoom)=="2000"] <- NA
levels(data$bathRoom)[levels(data$bathRoom)=="2003"] <- NA
levels(data$bathRoom)[levels(data$bathRoom)=="2004"] <- NA
levels(data$bathRoom)[levels(data$bathRoom)=="2005"] <- NA
levels(data$bathRoom)[levels(data$bathRoom)=="2006"] <- NA
levels(data$bathRoom)[levels(data$bathRoom)=="2011"] <- NA

#building type has decimal places when it should not - change to NA
levels(data$buildingType)[levels(data$buildingType)=="0.048"] <- NA
levels(data$buildingType)[levels(data$buildingType)=="0.125"] <- NA
levels(data$buildingType)[levels(data$buildingType)=="0.25"] <- NA
levels(data$buildingType)[levels(data$buildingType)=="0.333"] <- NA
levels(data$buildingType)[levels(data$buildingType)=="0.375"] <- NA
levels(data$buildingType)[levels(data$buildingType)=="0.429"] <- NA
levels(data$buildingType)[levels(data$buildingType)=="0.5"] <- NA
levels(data$buildingType)[levels(data$buildingType)=="0.667"] <- NA


#factor elevator, fiveyearsproperty, subway and change to yes/no
data$elevator = factor(mapvalues(data$elevator, from = c(0, 1), to = c("no", "yes")))
data$fiveYearsProperty = factor(mapvalues(data$fiveYearsProperty, from = c(0, 1), to = c("no", "yes")))
data$subway = factor(mapvalues(data$subway, from = c(0, 1), to = c("no", "yes")))

#factor building type, kitchen
data$buildingType <- as.factor(data$buildingType)
data$kitchen <- as.factor(data$kitchen)
data$district <- as.factor(data$district)

#ladderRAtio probably has outliers?

summary(data)
summary(data$buildingType)
