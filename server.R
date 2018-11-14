#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(dplyr)
library(plyr)
library(ggpubr)
library(tidyverse)
library(rlist)
library(corrplot)
library(ggcorrplot)
library(DMwR)
library(ggthemes)
library(e1071)
library(viridis)
library(tidyr)
library(cluster)
library(ggmap)
library(maps)
library(reshape2)
library(scales)
library(lubridate)
library(rgeos)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(rgl)
library(knitr)
library(rworldmap)
library(gridExtra)
library(caret)
library(psych)
library(leaflet)
library(clustMixType)
library(rlist)
library(rgl)
library(factoextra)
library(NbClust)


### data initial loading
model_dir = "models"
data_dir = "data"
map_dir = "map"

### load map
# bjmap <- get_map(location =c(left=115.9133, bottom=39.6966, right=116.8252, top=40.1831))
# beijingMap <- ggmap(bjmap)


### loading data
data_initial=read.csv(paste(data_dir,"new.csv",sep="/"), header = TRUE, sep = ",")
data=read.csv(paste(data_dir,"imputed_data.csv",sep="/"), header = TRUE, sep = ",")
xgb_best = readRDS(paste(model_dir,"xgb.model",sep="/"))
load(paste(model_dir,"kproto.rda",sep="/"))

### processing data
#remove unused columnns
dataVisualize <- data_initial[, !colnames(data_initial) %in% c("id", "Cid")]
dataVisualize$floor <- as.factor(as.numeric(gsub("\\D", "", dataVisualize$floor)))
dataVisualize$drawingRoom <- as.factor(as.numeric(gsub("\\D", "", dataVisualize$drawingRoom)))
dataVisualize$constructionTime <- as.factor(as.numeric(gsub("\\D", "", dataVisualize$constructionTime)))
dataVisualize$bathRoom <- as.factor(as.numeric(gsub("\\D", "", dataVisualize$bathRoom)))
dataVisualize$livingRoom <- as.factor(as.numeric(gsub("\\D", "", dataVisualize$livingRoom)))

#construction time has 0 and 1 listed for year of construction - incorrect so change them to NA
levels(dataVisualize$constructionTime)[levels(dataVisualize$constructionTime)=="0"] <- NA
levels(dataVisualize$constructionTime)[levels(dataVisualize$constructionTime)=="1"] <- NA

#bathroom has what appears to be years - change to NA
levels(dataVisualize$bathRoom)[levels(dataVisualize$bathRoom)=="1990"] <- NA
levels(dataVisualize$bathRoom)[levels(dataVisualize$bathRoom)=="1994"] <- NA
levels(dataVisualize$bathRoom)[levels(dataVisualize$bathRoom)=="1996"] <- NA
levels(dataVisualize$bathRoom)[levels(dataVisualize$bathRoom)=="2000"] <- NA
levels(dataVisualize$bathRoom)[levels(dataVisualize$bathRoom)=="2003"] <- NA
levels(dataVisualize$bathRoom)[levels(dataVisualize$bathRoom)=="2004"] <- NA
levels(dataVisualize$bathRoom)[levels(dataVisualize$bathRoom)=="2005"] <- NA
levels(dataVisualize$bathRoom)[levels(dataVisualize$bathRoom)=="2006"] <- NA
levels(dataVisualize$bathRoom)[levels(dataVisualize$bathRoom)=="2011"] <- NA

#building type has decimal places when it should not - change to NA
levels(dataVisualize$buildingType)[levels(dataVisualize$buildingType)=="0.048"] <- NA
levels(dataVisualize$buildingType)[levels(dataVisualize$buildingType)=="0.125"] <- NA
levels(dataVisualize$buildingType)[levels(dataVisualize$buildingType)=="0.25"] <- NA
levels(dataVisualize$buildingType)[levels(dataVisualize$buildingType)=="0.333"] <- NA
levels(dataVisualize$buildingType)[levels(dataVisualize$buildingType)=="0.375"] <- NA
levels(dataVisualize$buildingType)[levels(dataVisualize$buildingType)=="0.429"] <- NA
levels(dataVisualize$buildingType)[levels(dataVisualize$buildingType)=="0.5"] <- NA
levels(dataVisualize$buildingType)[levels(dataVisualize$buildingType)=="0.667"] <- NA


#factor elevator, fiveyearsproperty, subway and change to yes/no
dataVisualize$elevator = factor(mapvalues(dataVisualize$elevator, from = c(0, 1), to = c("no", "yes")))
dataVisualize$fiveYearsProperty = factor(mapvalues(dataVisualize$fiveYearsProperty, from = c(0, 1), to = c("no", "yes")))
dataVisualize$subway = factor(mapvalues(dataVisualize$subway, from = c(0, 1), to = c("no", "yes")))

#factor building type, kitchen
dataVisualize$buildingType <- as.factor(dataVisualize$buildingType)
dataVisualize$kitchen <- as.factor(dataVisualize$kitchen)
dataVisualize$district <- as.factor(dataVisualize$district)

#fill in DOM value
dataVisualize$DOM<- ifelse(is.na(dataVisualize$DOM),median(dataVisualize$DOM,na.rm=T),dataVisualize$DOM)

#converting numeric to categorical
#building type
makeBuildingType <- function(x){
  if(!is.na(x)){
    if(x==1){
      return('Tower')
    }
    else if (x==2){
      return('Bungalow')
    }
    else if (x==3){
      return('Mix_Plate_Tower')
    }
    else if (x==4){
      return('Plate')
    }
    else return('wrong_coded')
  }
  else{return('missing')}
}
dataVisualize$buildingType <- sapply(dataVisualize$buildingType, makeBuildingType)
dataVisualize <- data.frame(dataVisualize %>% filter(buildingType != 'wrong_coded' & buildingType !='missing'))

#building condition
makeRenovationCondition <- function(x){
  if(x==1){
    return('Other')
  }
  else if (x==2){
    return('Rough')
  }
  else if (x==3){
    return('Simplicity')
  }
  else if (x==4){
    return('Hardcover')
  }
}
dataVisualize$renovationCondition <- sapply(dataVisualize$renovationCondition, makeRenovationCondition)

# Building Structure
makeBuildingStructure <- function(x){
  if(x==1){
    return('Unknown')
  }
  else if (x==2){
    return('Mix')
  }
  else if (x==3){
    return('Brick_Wood')
  }
  else if (x==4){
    return('Brick_Concrete')
  }
  else if (x==5){
    return('Steel')
  }
  else if (x==6){
    return('Steel_Concrete')
  }
}
dataVisualize$buildingStructure <- sapply(dataVisualize$buildingStructure, makeBuildingStructure)

# Adding 2 attributes Month and Year for Selling Time
dataVisualize$tradeYear <- year(dmy(dataVisualize$tradeTime))
dataVisualize$tradeMonth <- month(dmy(dataVisualize$tradeTime))

### Function declare
df3 <- data.frame(dataVisualize %>% na.omit())
df3$buildingType <- as.factor(df3$buildingType)
df3$buildingStructure <- as.factor(df3$buildingStructure)
df3$elevator <- as.factor(df3$elevator)
df3$fiveYearsProperty <- as.factor(df3$fiveYearsProperty)
df3$subway <- as.factor(df3$subway)
df3$district <- as.factor(df3$district)
df3$renovationCondition <- as.factor(df3$renovationCondition)
df3$tradeTimeTs <- as.Date(df3$tradeTime, format = "%Y-%m-%d")
df3$monthlyTradeTS <- as.Date(paste0(df3$tradeYear,'-',df3$tradeMonth,'-01'))

makeFeatureCatEDA <- function(x){

    mypalette <-'Paired'
    mycols <- 2
    
    mybox <- df3 %>% ggplot(aes_string(x,'price')) + geom_boxplot(aes_string(color=x)) + 
      scale_color_brewer(name='', palette=mypalette) + theme_minimal(12) + 
      theme(axis.title =element_blank(), legend.position='None') + 
      labs(title='Average Price Per Square') + coord_flip()
  
    grid.arrange(mybox, ncol=1)
}

### clustered data
data3 <- data
data3$cluster_id = as.factor(kproto_selection$cluster)
bj_map <- data.frame(data3$price, data3$Lat, data3$Lng, data3$cluster_id)
colnames(bj_map) <- c('price', 'lat', 'lon', 'cluster_id')

#### Shiny app
shinyServer(function(input, output,session) {
  
  ##### Beijing Overview
  output$beijingAll <- renderPlot({
    if (input$allOption == "Volume"){
      year_data <- subset(dataVisualize,!(dataVisualize$tradeYear < 2011))
      all_group <- group_by(year_data, district, tradeYear)
      beijing_by_volume <- dplyr::summarise(all_group, n=n())
      ggplot(aes(x = tradeYear , y = n, fill = district), data = beijing_by_volume) +
        geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
        coord_flip() +
        xlab('Year') +
        ylab('Trading Volume') +
        theme_minimal() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
    } else if (input$allOption == "Value") {
      year_data <- subset(dataVisualize,!(dataVisualize$tradeYear < 2011))
      all_group <- group_by(year_data, district, tradeYear)
      beijing_by_value <- dplyr::summarise(all_group, value=sum(totalPrice)/1000)
      ggplot(aes(x = tradeYear , y = value, fill = district), data = beijing_by_value) +
        geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
        coord_flip() +
        xlab('Year') +
        ylab('Trading Value (1000 Yuan)') +
        theme_minimal() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
    } else {
        all_group <- group_by(dataVisualize, tradeYear)
        beijing_by_average_price <- dplyr::summarise(all_group, average=mean(totalPrice))
        ggplot(aes(x=tradeYear, y=average), data =beijing_by_average_price) +
        geom_line(size=1.5) +
            ylab('Year') +
            xlab('Average Total Price (Yuan)') +
            theme_grey() +
            theme(plot.title = element_text(size = 16),
                  axis.title = element_text(size = 12, face = "bold")) 
      
    }
  })
  
  ### District Compare
  output$districtCompare <- renderPlot({
    compareData <- subset(dataVisualize, dataVisualize$tradeYear == input$yearCompare)
    if (input$compareOption == "Volume"){
      all_group <- group_by(compareData, district, tradeYear)
      beijing_by_volume <- dplyr::summarise(all_group, n=n())
      ggplot(aes(x = district , y = n, fill = n), data = beijing_by_volume) +
        geom_bar(stat = 'identity', width = 0.5) +
        xlab('District') +
        ylab('Trading Volume') +
        theme_minimal() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
    } else if (input$compareOption == "Value") {
      all_group <- group_by(compareData, district)
      beijing_by_value <- dplyr::summarise(all_group, value=sum(totalPrice)/1000)
      ggplot(aes(x = district , y = value, fill = value), data = beijing_by_value) +
        geom_bar(stat = 'identity', width = 0.5) +
        xlab('District') +
        ylab('Trading Value (1000 Yuan)') +
        theme_minimal() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
    } else {
      mypalette <- colorRampPalette(brewer.pal(12,'Paired'))(13)
      mycols <- 3
      mybox <- compareData %>% ggplot(aes_string('district','price')) + geom_boxplot(aes_string(color='district')) + 
        scale_color_manual(name='',values=mypalette) + theme_minimal(12) + 
        theme(axis.title =element_blank(), legend.position='None') + 
        coord_flip()
      
      grid.arrange(mybox, ncol=1)
    }
    
  })
  
  ### Beijing Clustering
  output$beijingClustering <- renderPlot({
    sbbox <- make_bbox(lon = data3$Lng, lat = data3$Lat, f = 0.05)
    my_map <- get_map(location = sbbox, maptype = "roadmap", scale = 2, color="color", zoom = 10)
    ggmap(my_map) +
      geom_point(data=bj_map, aes(x = lon, y = lat, color = cluster_id), 
                 size = 0.5, alpha = 1) +
      xlab('Longitude') +
      ylab('Latitude') 
  })
  
  output$clusterSummary <- renderPrint({
    kproto_results <- subset(data3, data3$cluster_id == input$cluster) %>%
      dplyr::select(-price,-Lng,-Lat,-Cid,-district, -tradeTime) %>%
      do(the_summary = summary(.))
    print(kproto_results$the_summary)
  })
  
  
  ### Beijing Map
  output$priceChart <- renderPlot({
    if (input$byDistrict == 0){
      plot_data <- df3
    }else {
      plot_data <- subset(df3, df3$district == input$byDistrict)
    }
    plot_data %>% filter(tradeYear>2009) %>% group_by(monthlyTradeTS) %>% 
      dplyr::summarise(count=n(), mean = mean(price)) %>% 
      ggplot(aes(x=monthlyTradeTS, y= mean)) + 
      geom_line(size=2, alpha=.25, color='steelblue') + geom_point(aes(size=count), alpha=.75, colour = 'skyblue1') +
      theme_minimal(14) + theme(axis.title =element_blank()) + 
      labs(title='Average Price Over Time') + 
      scale_radius(range=c(1,10))
    
  })
  
  output$beijingMap <- renderLeaflet({
    if (input$byDistrict == 0){
      map_data <- subset(dataVisualize, dataVisualize$tradeYear == input$byYear & dataVisualize$price >= input$priceRange[1] & dataVisualize$price <= input$priceRange[2])
    }else {
      map_data <- subset(dataVisualize, dataVisualize$tradeYear == input$byYear & dataVisualize$district == input$byDistrict & dataVisualize$price >= input$priceRange[1] & dataVisualize$price <= input$priceRange[2])
    }
    if (input$colourBy == "District"){
      pal <- colorFactor(
        palette = colorRampPalette(brewer.pal(12,'Paired'))(13),
        domain = dataVisualize$district
      )
      m <- leaflet(map_data) %>%  
        addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                                            attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
        addCircleMarkers(~Lng, ~Lat, 
                         popup = paste("Price: ", map_data$totalPrice * 10, "k (",map_data$price, " * ", map_data$square , ")" , "<br>",
                                       "Trade date: ", map_data$tradeTime , "<br>",
                                       "District: ", map_data$district," - Community Average: ", map_data$communityAverage, "<br>",
                                       "Build Type: ", map_data$buildingType, "<br>",
                                       "Built Year: ", map_data$constructionTime, "<br>", 
                                       "Rooms:", map_data$drawingRoom, " living rooms| ", map_data$livingRoom, " bedrooms| ", map_data$bathRoom, " bathrooms| ", map_data$kitchen, " kitchens <br>",
                                       "<a href=", map_data$url, ">View details</a>"),
                         weight = 4, radius = 5, color = ~ pal(district), stroke = F, fillOpacity = 0.5) 
    }else {
      pal <- colorNumeric(
        palette = "Blues",
        domain = dataVisualize$price
      )
      m <- leaflet(map_data) %>%  
        addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                 attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
        addCircleMarkers(~Lng, ~Lat, 
                         popup = paste("Price: ", map_data$totalPrice * 10, "k (",map_data$price, " * ", map_data$square , ")" , "<br>",
                                       "Trade date: ", map_data$tradeTime , "<br>",
                                       "District: ", map_data$district," - Community Average: ", map_data$communityAverage, "<br>",
                                       "Build Type: ", map_data$buildingType, "<br>",
                                       "Built Year: ", map_data$constructionTime, "<br>", 
                                       "Rooms:", map_data$drawingRoom, " living rooms| ", map_data$livingRoom, " bedrooms| ", map_data$bathRoom, " bathrooms| ", map_data$kitchen, " kitchens <br>",
                                       "<a href=", map_data$url, ">View details</a>"),
                         weight = 4, radius = 5, color = ~ pal(price), stroke = F, fillOpacity = 0.5) 

    }

  })
  
  ### Price Analysis
  output$priceAnalysis <- renderPlot({
    if (input$byAttribute == "Building Structure"){
      makeFeatureCatEDA('buildingStructure')
    } else if (input$byAttribute == "Building Type"){
      makeFeatureCatEDA('buildingType')
    } else if (input$byAttribute == "Renovation Condition"){
      makeFeatureCatEDA('renovationCondition')
    } else if (input$byAttribute == "Has Elevator?"){
      makeFeatureCatEDA('elevator')
    } else if (input$byAttribute == "Near Subway?"){
      makeFeatureCatEDA('subway')
    } else if (input$byAttribute == "5 Years Owner Property"){
      makeFeatureCatEDA('fiveYearsProperty')
    }
  })
  
  output$priceColleration <- renderPlot({
    corrplot(cor(
      df3 %>% select_if(is.numeric) %>% select(-Lng, -Lat) ,use = "pairwise.complete.obs",
      method='pearson')
      ,method='ellipse',
      tl.cex = 1,
      col = viridis::viridis(50),
      tl.col='black')
  }, width = 700, height = 650)
  
})
