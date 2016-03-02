library(shiny)
library(rCharts)
library(magrittr)
library(reshape2)
library(ggplot2)
library(data.table)
library(markdown)
library(dplyr)

options(RCHART_LIB = 'polycharts')

## https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

# Data Pre-processing

dt.hr <- read.csv("hour.csv") %>% .[, -1]
dt.day <- read.csv("day.csv") %>% .[, -1]

colnames(dt.day) <- c("Date", "Season", "Year", "Month", "Holiday", "Weekday", 
                     "Workingday", "Weather", "Temperature", "Feeling Temp",
                     "Humidity", "WindSpeed", "Non-Member", "Member", "Total")
                     

dt.hr <- cbind(dt.hr, paste(dt.hr$hr, ":00:00", sep = ""))
dt.hr <- cbind(dt.hr, paste(dt.hr$dteday, dt.hr[, 17]))
dt.hr <- cbind(dt.hr, strptime(dt.hr[, 18], format = "%Y-%m-%d %H:%M:%S"))
dt.hr <- dt.hr[,c(18, 3, 2, 4, 5, 7, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1)]

colnames(dt.hr) <- c("Date", "Year", "Season", "Month", "Hour", "Weekday", 
                     "Holiday", "Workingday", "Weather", "Temperature",
                     "Feeling Temp","Humidity", "WindSpeed", "Non-Member",
                     "Member", "Total", "Day")

dt.table <- dt.hr

for (i in 1:4) {
  list_season <- c("Spring", "Summer", "Fall", "Winter")
  dt.table$Season[dt.table$Season == i] <- list_season[i]
}

for (i in 0:1) {
  list_yr <- c(2011, 2012)
  dt.table$Year[dt.table$Year == i] <- list_yr[i + 1]
}

for (i in 0:1) {
  list_holiday <- c("No", "Yes")
  dt.table$Holiday[dt.table$Holiday == i] <- list_holiday[i + 1]
}

for (i in 0:1) {
  list_holiday <- c("No", "Yes")
  dt.table$Workingday[dt.table$Workingday == i] <- list_holiday[i + 1]
}

for (i in 0:6) {
  list_weekday <- c("Sunday", "Monday", "Tuesday", "Wendesday",
                    "Thrusday", "Friday", "Saturday")
  dt.table$Weekday[dt.table$Weekday == i] <- list_weekday[i + 1]
}

for (i in 1:4) {
  list_weathersit <- c("Clear", "Cloudy", "Light Rain", "Heavy Rain")
  dt.table$Weather[dt.table$Weather == i] <- list_weathersit[i]
}


shinyServer(function(input, output, session) {

  aggregate_by_time <- function(dt, time) {
      if (time == "Date") {
        dt <- dt.day %>% dplyr::select(matches("Date|Non-Member|Member"))      
      } else {
        dt <- dt[,c(-1, -17)] %>% group_by_(time) %>% 
          summarise_each(funs(sum), c(get("Non-Member"), Member))
      }
      return(dt)
  }

  dt.group <- reactive({
    if (input$scale == "Date") {
      data <- dt.day %>% dplyr::select(matches("Date|Non-Member|Member")) 
    } else {
      data <- aggregate_by_time(dt.hr, input$scale)
    }
    return(data)
  })

  dt.stackgroup <- reactive({
    dt <- aggregate_by_time(dt.hr, input$stackscale)
    dt <- melt(dt, measure.vars = c("Member", "Non-Member"))
    if (input$stackscale == "Year") {
      dt[, 1] <- c("2011", "2012")
    } else if (input$stackscale == "Season") {
      dt[, 1] <- c("Spring", "Summer", "Fall", "Winter")
    } else if (input$stackscale == "Weekday") {
      dt[, 1] <- c("Sunday", "Monday", "Tuesday", "Wendesday",
                   "Thrusday", "Friday", "Saturday")
    } else if (input$stackscale == "Holiday") {
      dt[, 1] <- c("No", "Yes")
    } else if (input$stackscale == "Workingday") {
      dt[, 1] <- c("No", "Yes")
    } else if (input$stackscale == "Weather") {
      dt[, 1] <- c("Clear", "Cloudy", "Light Rain", "Heavy Rain")
    }
    return(dt)
  })
  
  observe({
    x <- input$controller
    updateDateRangeInput(session, "inDateRange",
                         label = paste("Date range (2011 to 2012)"),
                         start = paste("2011-01-01", sep=""),
                         end = paste("2012-12-", x, sep=""),
                         min = paste("2011-01-01", sep=""),
                         max = paste("2012-12-31", sep=""))
  })

  output$bytime <- renderChart({
    dt <- dt.day %>% dplyr::select(matches("Date|Non-Member|Member")) 
    dt <- dt[, 1] %>% as.Date %>% as.numeric %>% cbind(dt, .)
    dt <- dt[dt[, 4] >= input$inDateRange[1] & dt[, 4] <= input$inDateRange[2], ]
    dt <- transform(dt, Date = as.character(Date))
    p1 <- mPlot(x = "Date", y = c("Member", "Non.Member"),
                type = "Line", data = dt, pointSize = 0, lineWidth = 1)
    p1$set(lineColors = c("green", "blue"))
    p1$set(dom = "bytime")
    return(p1)
  })  
    
  plotStack <- reactive({
    paste("value ~ ", input$stackscale)	
  }) 

  output$stackedchart <- renderChart({
    dt <- dt.stackgroup()
    n1 <- nPlot(as.formula(plotStack()), group = "variable", data = dt, 
                type = input$stacktype, dom = "stackedchart")
    n1$chart(stacked = input$stacktype)
    n1$chart(color = c("green", "blue"))
    n1$set(width = 800, height = 400)
    return(n1)
  })

  output$downloadData <- downloadHandler(
    filename = "UCI.capitalbike.csv", 
    content = function(file) {
      write.csv(dt.table, file, row.names = FALSE)
    }
  )
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- dt.table
    if (input$yr != "All") {
      data <- data[data$Year == input$yr, ]
    }
    if (input$season != "All") {
      data <- data[data$Season == input$season, ]
    }
    if (input$mnth != "All") {
      data <- data[data$Month == input$mnth, ]
    }
    if (input$hr != "All") {
      data <- data[data$Hour == input$hr, ]
    }
    if (input$wkday != "All") {
      data <- data[data$Weekday == input$wday, ]
    }
    if (input$hday != "All") {
      data <- data[data$Holiday == input$hday, ]
    }
    if (input$wkingday != "All") {
      data <- data[data$Workingday == input$wkingday, ]
    }
    if (input$weather != "All") {
      data <- data[data$Weather == input$weather, ]
    }
    data <- data[, -17]
  }))

  dt.reg <- reactive({
    Var <- length(input$show_vars)
    out <- NULL
    for (i in seq_len(Var)) {
      if (i == 1) {
        if (input$show_vars[i] == "Temperature") {
          out <- paste0(out, as.character(input$show_vars[i]))
        } else if (input$show_vars[i] == "Feeling Temp") {
          out <- paste0(out, "`",
                        as.character(input$show_vars[i]), "`")
        } else {
          out <- paste0(out, "as.factor(", 
                        as.character(input$show_vars[i]), ")")
        }    
      } else {
        if (input$show_vars[i] == "Temperature") {
          out <- paste0(out, "+", as.character(input$show_vars[i]))
        } else if (input$show_vars[i] == "Feeling Temp") {
          out <- paste0(out, "+", "`",
                        as.character(input$show_vars[i]), "`")
        } else {
          out <- paste0(out, "+", "as.factor(",
                        as.character(input$show_vars[i]), ")")
        }    
      }
    }
    out <- paste0(out, "-1")
    return(out)
  }) 

  regMember <- reactive({
    paste0("Member ~ ", dt.reg())	
  }) 
  
  regNonmember <- reactive({
    paste("`Non-Member` ~ ", dt.reg())	
  })
  
  regTotal <- reactive({
    paste("Total ~ ", dt.reg())	
  }) 

  dt.reg_2 <- reactive({
    Var <- length(input$show_vars_2)
    out <- NULL
    for (i in seq_len(Var)) {
      if (i == 1) {
        if (input$show_vars_2[i] == "Temperature") {
          out <- paste0(out, as.character(input$show_vars_2[i]))
        } else if (input$show_vars_2[i] == "Feeling Temp") {
          out <- paste0(out, "`",
                        as.character(input$show_vars_2[i]), "`")
        } else {
            out <- paste0(out, "as.factor(", 
                          as.character(input$show_vars_2[i]), ")")
        }    
      } else {
        if (input$show_vars_2[i] == "Temperature") {
          out <- paste0(out, "+", as.character(input$show_vars_2[i]))
        } else if (input$show_vars_2[i] == "Feeling Temp") {
          out <- paste0(out, "+", "`",
                        as.character(input$show_vars_2[i]), "`")
        } else {
            out <- paste0(out, "+", "as.factor(",
                      as.character(input$show_vars_2[i]), ")")
        }    
      }
    }
    out <- paste0(out, "-1")
    return(out)
  }) 
  
  regMember_2 <- reactive({
    paste0("Member ~ ", dt.reg_2())	
  }) 
  
  regNonmember_2 <- reactive({
    paste("`Non-Member` ~ ", dt.reg_2())	
  })
  
  regTotal_2 <- reactive({
    paste("Total ~ ", dt.reg_2())	
  }) 

  output$trendModel <- renderPrint({
    model.T <- lm(as.formula(regTotal()), data = dt.hr)
    summary(model.T)$coefficients %>% round(., 2)
  })
  
  output$trendModel2 <- renderPrint({
    model.T_2 <- lm(as.formula(regTotal_2()), data = dt.hr)
    summary(model.T_2)$coefficients %>% round(., 2)
  })
  
  output$Modelcomp <- renderPrint({
    model.T <- lm(as.formula(regTotal()), data = dt.hr)
    model.T_2 <- lm(as.formula(regTotal_2()), data = dt.hr)
    p.T <- anova(model.T, model.T_2)$`Pr(>F)`[2]
    if (length(input$show_vars) <= length(input$show_vars_2)) {
      if (p.T <= 0.05) {
        paste("Model 2 is significantly better than Model 1!")
      } else {
        paste("Use Model 1 is just fine!")
      }
    } else {
      if (p.T <= 0.05) {
        paste("Model 1 is significantly better than Model 2!")
      } else {
        paste("Use Model 2 is just fine!")
      }
    }
  })
  
})



