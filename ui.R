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
              ),
              box(title = "Beijing Price Analysis", width = 12, solidHeader = TRUE, status = "primary",
                  box(
                    title = "Categorical Attributes", width = 4, height = 1000, status = "warning",
                    selectInput("byAttribute", 
                                strong("Observe by:"), 
                                choices = list("Building Structure","Building Type", "Renovation Condition", "Has Elevator?", "Near Subway?", "5 Years Owner Property"),
                                selected = "Building Structure"),
                    plotOutput("priceAnalysis")
                  ),
                  box(
                    title = "Numberic Attributes", width = 8, height = 1000, status = "warning",
                    plotOutput("priceColleration")
                  )
              )
              
            ),
            ## Section 2
            fluidRow(
              box(title = "Beijing Housing Clustering", width = 12, solidHeader = TRUE, status = "primary",
                  box(width = 12,
                      #plotOutput("beijingClustering", height = 900)
                      imageOutput("beijingClustering", height = 900)
                  ),
                  box(title = "Clustering Information", width = 12, solidHeader = TRUE, status = "warning",
                      selectInput("cluster","Cluster:", 
                                  c(1:4)),
                      collapsible = TRUE,
                      verbatimTextOutput("clusterSummary")
                  )
              )
              
            )
    ),
    
    tabItem(tabName = 'housePricePrediction',
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
              ),
              box(title = "Prediction Beijing Housing Price", width = 12, solidHeader = TRUE, status = "warning",
                  box(
                    title = "Model Performances",
                    width = 6, height = 500,
                    plotOutput("modelPerformances")
                    
                  ),
                  box(
                    title = "Tuning XGB",
                    width = 6, height = 500,
                    plotOutput("modelXGB")
                    
                  ),
                  box(
                    width = 12,height = 600,
                    title = "Prediction v.s Ground Truth", solidHeader = TRUE,status = "warning",
                    selectInput("model","Select Model:",
                                c("XGB", "RPart", "GLM")),
                    plotOutput("groundTruth")
                  ),
                  box(
                    title = "Interesting Important Features", width = 12, solidHeader = TRUE, status = "warning",
                    box(
                      title = "XGB Important Features", width = 6, status = "warning",
                      #plotOutput("XGBFeatures")
                      DT:: dataTableOutput("XGBFeatures")
                    ),
                    box(
                      title = "GLM Important Features", width = 6, status = "warning",
                      DT:: dataTableOutput("GLMFeatures")
                    )
                  )
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




