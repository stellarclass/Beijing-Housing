library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(Hmisc)
library(mice)
library(lubridate)
library(clustMixType)

library(rlist)
library(rgl)
library(scatterplot3d)
# library(tidyr)
# library(cluster)

# library(maptools)
# library(ggpubr)
# library(viridis)
# library(tidyverse)

library(factoextra)
# library(fpc)
 library(NbClust)
# library(reshape2)
# library(scales)
 library(caret)
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


#clustering

data_clust <- data[, !colnames(data) %in% c("Lng", "Lat", "Cid", "district")]

set.seed(1234)

kproto_clusters = list()
for (i in seq(2,8,1)) {
  kproto_clusters = list.append(kproto_clusters,kproto(data_clust,i,lambda = 1))
}

wss = c()
for(kc in kproto_clusters) {
  wss = c(wss,kc$tot.withinss)
}
plot(seq(2,8,1),wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

kproto_selection = kproto_clusters[[3]]

#3D plot
pc <-princomp(data_clust, cor=TRUE, scores=TRUE)
plot3d(pc$scores[,1:3], col=kproto_selection$cluster, main="k-means clusters")

data3 <- data
data3$cluster_id = as.factor(kproto_selection$cluster)
bj_map <- data.frame(data3$price, data3$Lat, data3$Lng, data3$cluster_id)
colnames(bj_map) <- c('price', 'lat', 'lon', 'cluster_id')
sbbox <- make_bbox(lon = data3$Lng, lat = data3$Lat, f = 0.05)
my_map <- get_map(location = sbbox, maptype = "roadmap", scale = 2, color="color", zoom = 10)
ggmap(my_map) +
  geom_point(data=bj_map, aes(x = lon, y = lat, color = cluster_id), 
             size = 0.5, alpha = 1) +
  xlab('Longitude') +
  ylab('Latitude') +
  ggtitle('Place Holder')

kproto_results <- data3 %>%
  dplyr::select(-price,-Lng,-Lat,-Cid,-district) %>%
  group_by(cluster_id) %>%
  do(the_summary = summary(.))
print(kproto_results$the_summary)

# cluster 1:
# not on market for very long (15 days average but 1 day median!) - high demand!
# price is about 350-400
# tends to be pretty small - bachelor type (1b), 60-70 m^^2
# not bungalows!
# built in the 90s
# elevator 50/50
# tend to be owned for >5 years
# near a subway
# tends to be in the middle of the building
# 
# cluster 2:
# on the market for a while - 34 average, 12 median
# expensive! about 600-700!
# 60-70 m^2 but tend to be 2b - quite cramped
# tend to be plate type building
# built in the 90s
# even elevator distr, slightly more owned >5y
# near a subway!
# even floor dist except for bottom
# 
# cluster 3:
# not on market for too long, but slightly longer than cluster 1 - 1 median 17 mean
# pretty cheap - 300-350
# slightly bigger than cluster 1, but only a little bit - 2b 70-80 m^2
# not bungalows
# slightly newer - built in '00s
# has elevators, near subway, owned >5 years
# more even dist of floors, but tend not to be on ground floor, solidly middle level
# 
# cluster 4:
# sells very quickly - 1 median, 10 mean!
# the cheapest - 200-250
# very slightly bigger than cluster 3 - 80-90 m^2 but roughly same number of rooms
# def not bungalows
# also built in '00s but slightly newer
# even distr of elevators, whether near subway or not, but tends to be no
# slightly more 5y owned
# even dist of floors, slightly less on bottom
#
# based on locations in the map, looks like cluster 4 is definitely farther from the centre - more suburban living
# cluster 3 also outside of city centre - outskirts of the downtown
# 1 and 2 are the downtown properties
# cluster 2 taking the longest to sell because it's in prime real estate but they are small and expensive