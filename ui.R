#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(rgl)
library(leaflet)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Beijing Housing Exploring", tabName = "beijing", icon = icon("th")),
    menuItem("Beijing Housing Clustering", icon = icon("dashboard"), tabName = "neighbourhoodClustering", badgeColor = "green"),
    menuItem("Beijing Housing Prediction", icon = icon("dashboard"), tabName = "housePricePrediction", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "beijing",
            ## Section 1
            fluidRow(
              box(title = "Report", width = 12, solidHeader = TRUE, status = "warning",
                  strong("Please click ",
                         tags$a(href='report.html',' here', target='blank'), " to view Beijing Housing project's report.")
              ),
              box(title = "Beijing Housing Overview", width = 12, solidHeader = TRUE, status = "primary",
                  box(
                    title = "Beijing Housing", width = 6, height = 650, status = "primary",
                    selectInput("allOption", 
                                strong("Observe by:"), 
                                choices = list("Volume", "Value", "Average Total Price"),
                                selected = "Volume"),
                    plotOutput("beijingAll")
                  ),
                  
                  box(
                    title = "District Comparison", width = 6, height = 650, status = "warning",
                    selectInput("compareOption", 
                                strong("Observe by:"), 
                                choices = list("Volume", "Value", "Average Price Per Square"),
                                selected = "Volume"),
                    sliderInput("yearCompare", "Year:", 2008, 2018, 2017),
                    plotOutput("districtCompare")
                    
                  )
              )
            ),
            
            ## Section 2
            fluidRow(
              box(title = "Beijing Housing Trading Map", width = 12, solidHeader = TRUE, status = "primary",
                  box(
                    width = 5, height = 1000,
                    sliderInput("byYear", "Year:", 2008, 2018, 2017),
                    sliderInput("priceRange", "Square Price Range:",
                                min = 0, max = 160000,
                                value = c(0,160000)),
                    selectInput("colourBy","Colour By:",
                                c("District", "Price")),
                    selectInput("byDistrict","District:", 
                                c(0:13)),
                    plotOutput("priceChart")
                    
                  ),
                  box(
                    width = 7,height = 1000,
                    title = "Beijing Map",  background = "black",
                    leafletOutput("beijingMap", height = 900)
                  )
              )
            ),
            
            ## Section 3
            fluidRow(
              ## Row 1
              box(title = "Beijing Price Analysis", width = 12, solidHeader = TRUE, status = "primary",
                  box(
                    title = "Categorical Attributes", width = 6, height = 750, status = "warning",
                    selectInput("byAttribute", 
                                strong("Observe by:"), 
                                choices = list("Building Structure","Building Type", "Renovation Condition", "Has Elevator?", "Near Subway?", "5 Years Owner Property"),
                                selected = "Building Structure"),
                    plotOutput("priceAnalysis")
                  ),
                  box(
                    title = "Numberic Attributes", width = 6, height = 750, status = "warning",
                    plotOutput("priceColleration")
                  )
              )
            )
    ),
    
    tabItem(tabName = 'neighbourhoodClustering',
            fluidRow(
              box(title = "Beijing Housing Clustering", width = 12, solidHeader = TRUE, status = "warning",
                  box(
                    title ="Number of Clusters", width = 3,
                    sliderInput("clusterNo", "Top n:", 3, 6, 3)
                  ),
                  box(title = "Toronto Criminal Map By Neighbourhoods", width = 12, solidHeader = TRUE, status = "primary",
                      box(
                        title = "Manual Clustering Map", width = 4, height = 450, status = "warning",
                        plotOutput("manualMap")
                      ),
                      
                      box(
                        title = "kMean Clustering Map", width = 4, height = 450, status = "warning",
                        plotOutput("kMeanMap")
                      ),
                      
                      box(
                        title = "Hierarchical Clustering Map", width = 4, height = 450, status = "warning",
                        plotOutput("hierarchicalMap")
                      )
                  ),
                  box(
                    title = "kMean Clustering", width = 12, height = 620, solidHeader = TRUE, status = "primary",
                    box(
                      title = "Determine number of clusters", width = 3,height = 550, status = "warning",
                      plotOutput("kMeanElbow")
                    ),
                    box(
                      title = "2D kMean Clustering", width = 4,height = 550, status = "warning",
                      plotOutput("2DkMeanCluster")
                    ),
                    box(
                      title = "3D kMean Clustering", width = 5,height = 550, status = "warning",
                      rglwidgetOutput("3DkMeanCluster")
                    )
                  ),
                  box(
                    title = "Hierarchical Clustering", width = 12, height = 620, solidHeader = TRUE, status = "primary",
                    box(
                      title = "Hiearchical Cluster Diagram", width = 3,height = 550, status = "warning",
                      plotOutput("clusterDiagram")
                    ),
                    box(
                      title = "2D Hierarchical Clustering", width = 4,height = 550, status = "warning",
                      plotOutput("2DHierarchicalCluster")
                    ),
                    box(
                      title = "3D Hierarchichal Clustering", width = 5,height = 550, status = "warning",
                      rglwidgetOutput("3DHierarchicalCluster")
                    )
                  )
              )
            )
    ),
    tabItem(tabName = 'housePricePrediction',
            fluidRow(
              box(title = "Beijing Housing Price Prediction", width = 12, solidHeader = TRUE, status = "warning"

              )
            )
    )
    
  )
)


# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(disable = TRUE),
  sidebar,
  body
)




