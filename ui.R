library(shiny)
library(rCharts)
library(ggplot2)
library(plyr)
library(UsingR)
library(data.table)
library(magrittr)
library(markdown)

#options(RCHART_LIB = "morris")

shinyUI(fluidPage(
  headerPanel("Capital Bikeshare"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      conditionalPanel(
        'input.dataset === "Time Series"', 
        dateRangeInput("inDateRange", "Date range input:")
        ),
      
      conditionalPanel(
        'input.dataset === "Statistics"', 
        selectInput(inputId = "stackscale",
                    label = "Select scale",
                    choices = c("Year", "Season", "Month", "Hour", "Weekday",
                                "Workingday", "Holiday", "Weather"),
                    selected = "Year")),
      
      conditionalPanel(
        'input.dataset === "Statistics"',   
        selectInput(inputId = "stacktype",
                    label = "Select Chart Type",
                    choices = c("Vertical" = "multiBarChart",
                                "Horizontal" = "multiBarHorizontalChart"),
                    selected = "multiBarChart")),

      conditionalPanel(
        'input.dataset === "Table"', 
        selectInput("yr", "Year:",
                    c("All", "2011", "2012")),
        selectInput("season", "Season:", 
                    c("All", "Spring", "Summer", "Fall", "Winter")),
        selectInput("mnth", "Month:",
                    c("All", "1", "2", "3", "4", "5", "6", "7", "8",
                      "9", "10", "11", "12")),
        selectInput("hr", "Hour:",
                    c("All", as.character(0:23))),
        selectInput("wkday", "Weekday:",
                    c("All", c("Sunday", "Monday", "Tuesday", "Wendesday",
                               "Thrusday", "Friday", "Saturday"))),
        selectInput("hday", "Holiday:",
                    c("All", "Yes", "No")),
        selectInput("wkingday", "Workingday:",
                    c("All", "Yes", "No")),
        selectInput("weather", "Weather:",
                    c("All", c("Clear", "Cloudy", "Light Rain", "Heavy Rain")))
      ), ## conditionalPanel

      conditionalPanel(
        'input.dataset === "Analysis"',   
        checkboxGroupInput('show_vars', 'Predictor for Model 1:',
                           choices = c("Year", "Season", "Month", "Hour", "Weekday",
                                       "Workingday", "Holiday", "Weather", "Temperature",
                                       "Feeling Temp"),
                           selected = c("Season"))
      ),
      
      conditionalPanel(
        'input.dataset === "Analysis"',   
        checkboxGroupInput('show_vars_2', 'Predictor for Model 2:',
                           choices = c("Year", "Season", "Month", "Hour", "Weekday",
                                       "Workingday", "Holiday", "Weather", "Temperature",
                                       "Feeling Temp"),
                           selected = c("Year", "Month"))
      )
  ), ## sidebarPanel
  
    mainPanel(
      tabsetPanel(
        id = "dataset",
        tabPanel("About", includeMarkdown("include.md")),
        tabPanel("Time Series", h4("Number of Renters", align = "center"), 
                 showOutput("bytime", "morris")),
        tabPanel("Statistics", h4("Number of Renters", align = "center"),
                 showOutput("stackedchart", "nvd3")),
        tabPanel("Analysis",
                 fluidRow(
                   column(12,
                          p("This page compare 2 different models. You
                            can specify the predictors used in regression."),
                   verbatimTextOutput("Modelcomp")   
                 )),
                 fluidRow(
                   column(12, wellPanel(h4("Coefficients of Model 1"),
                     verbatimTextOutput("trendModel")
                   )),
                   
                   column(12, wellPanel(h4("Coefficients of Model 2"),
                     verbatimTextOutput("trendModel2")
                   )))),
        tabPanel("Table", DT::dataTableOutput("table"),
                 downloadButton("downloadData", "Download"))
      ) ## tabsetPanel
    ) ## mainPanel
  ) # sidebarLayout
))


