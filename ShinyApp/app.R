# CTA stations data
# This app takes CTA station data, from UIC, O'Hare airport, and
# and gives visualizations on riders within each year, month, day, day of the week
# Author: Wayne Kao

#TODO:
# Make it look better
#Background color
#Menu column spacing
# Fix min max on date
# Fix labels on graph
# Fix coloring on graph


#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(gridExtra)
library(dplyr)


# assume all of the tsv files in this directory are data of the same kind that I want to visualize
# read.table(file="uic.tsv", quote="", sep="\t", header=TRUE)
# ohare <- read.table(file="ohare.tsv", quote="", sep="\t", header=TRUE)
# cermak <- read.table(file="cermak.tsv", quote="", sep="\t", header=TRUE)

myfiles <- list.files(pattern="*.csv", full.names=TRUE)
dataStations <- do.call(rbind, lapply(myfiles, read.csv, header = FALSE))
colnames(dataStations) <- c("", "station_id", "stationname", "date", "daytype", "rides", "STOP_ID", "Location")
dataStations$date <- mdy(dataStations$date)
dataStations$rides <- as.numeric(gsub(",", "", dataStations$rides))


#stations <- c("UIC-Halsted", "O'Hare Airport", "54th/Cermak")
#graphs <- c("All Years", "Each Day", "Each Month", "Each Day of Week", "Table")
years <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)

# Create the shiny dashboard
#ui <- shinyUI(
#  navbarPage("CTA Riders", position = "fixed-bottom",
#             tabPanel("Plot",
#                      mainPanel(
#                        column(2, style = "background-color: #00FF00;",
#                        ),
#                        column(8,
#                               fluidRow(
#                                 box
#                                 (
#                                   title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
#                                   plotOutput("hist2", width = "70%", height = 2000)
#                                 )
#                               )
#                        )
#                      )
#             ),
#             tabPanel("About",
#                      fluidPage(
#                        fluidRow(style="font-size: 40px; padding-bottom: 15%",
#                                 h1("CTA Rides Data"),
#                                 h4("Author: Wayne Kao"),
#                                 h4("Dataset: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
#                                 div("The data was taken from the city of chicago page. This app was written to compare the amount of riders from 2001-2021
#                                from three stations: UIC-Halsted, O'Hare Airport, and 54th/Cermak. For UIC-Halsted, the coloring of the graph coresponds
#                                more to the UIC school year and timings of the year versus O'Hare and 54th/Cermak looks at more towards overall year
#                                based on the season. The data goes up to November 2021 so December of 2021 is missing in this dataset. You are able
#                                to switch graphs between looking at all the riders at a particular station with the following criteria:
#                                all years from 2001-2021, or all riders categorized by days, months, day of the week with a particular year.
#                                You are also given an option to view all graphs in a table like structure.")
#                        )
#                      )
#             ),
#             tags$style(type="text/css",
#                        '.navbar{
#                font-size: 20px;
#             }')
#  )
#)
ui <- shinyUI(
  navbarPage("CTA Riders", position = "fixed-bottom",
             tabPanel("Plot",
                      fluidPage(style="background-color: lightblue",
                                column(1, style = "height:1620px;background-color: orange",
                                       column(12,
                                              dateInput("date1", "Date:", value = "2021-08-23"),

                                              fluidRow(class = "dayRow",
                                                       actionButton("previousday", "Previous Day"),
                                                       actionButton("nextday", "Next Day"),
                                              ),

                                              radioButtons("order", "Order By:",
                                                           c("Alphabetical" = "alpha",
                                                             "Minimum to Maximum" = "minmax")
                                              )
                                       )
                                ),
                                column(11,style = "height:200px;",
                                       fluidRow(class = "myRow1",
                                                column(4,style = "height:200px;background-color: yellow",
                                                       box(
                                                         title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("stopsByDate", width = "100%", height = 1400)
                                                       )
                                                ),
                                                column(4,style = "height:200px;background-color: blue",
                                                       dataTableOutput("table_all_station")
                                                ),
                                                column(4,style = "height:200px;background-color: green",
                                                       leafletOutput("mymap"),
                                                       p(),
                                                       actionButton("recalc", "New points")
                                                ),
                                       )
                                ),

                                # fluidRow(class = "myRow2",
                                #         column(6,div(style = "height:100px;background-color: green;", "Bottomleft")),
                                #         column(6,div(style = "height:150px;background-color: red;", "Bottomright"))),
                                tags$head(tags$style(
                                  " .myRow1{
                                    height:1620px;
                                  }
                                  .container-fluid{
                                    background-color: lightblue;
                                  }")
                                )

                      )
             ),
             tabPanel("Plot2",
                      fluidPage(style="background-color: lightblue",
                                column(1,
                                       column(12,
                                          selectInput("select_station", "Select Station", c("UIC-Halsted","O'Hare Airport"), selected = "UIC-Halsted"),
                                          selectInput("select_year", "Select Year", years, selected = 2010)
                                       )
                                ),
                                column(8,
                                       fluidRow(class = "s2r1",
                                                column(9,
                                                       box(
                                                         title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("all_station_years", width = "100%", height = 700)
                                                       )
                                                ),#table_specific_station
                                                column(3,style = "height:200px;background-color: blue",
                                                       dataTableOutput("table_specific_station")
                                                )
                                       ),
                                       fluidRow(class = "s2r2",
                                                column(4,
                                                       box(
                                                         title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("station_each_day", width = "100%", height = 700)
                                                       )
                                                ),
                                                column(4,
                                                       box(
                                                         title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("station_each_month", width = "100%", height = 700)
                                                       )
                                                ),
                                                column(4,
                                                       box(
                                                         title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("station_each_day_of_week", width = "100%", height = 700)
                                                       )
                                                )
                                       ),
                                ),
                                column(3,style = "height:200px;background-color: green",

                                ),
                                tags$head(tags$style(
                                  " .myRow1{
                                    height:1620px;
                                  }
                                  .container-fluid{
                                    background-color: lightblue;
                                  }")
                                )

                      )
             ),
             tabPanel("Plot3",
                      fluidPage(style="background-color: lightblue",
                                column(1, style = "height:1620px;background-color: orange",
                                       column(12,
                                              dateInput("t_date1", "Date 1:", value = "2021-08-23"),
                                              dateInput("t_date2", "Date 2:", value = "2021-07-22"),
                                              radioButtons("t_order", "Order By:",
                                                           c("Alphabetical" = "alpha",
                                                             "Minimum to Maximum" = "minmax")
                                              )
                                       )
                                ),
                                column(8,
                                       fluidRow(class = "s3r1",
                                                column(6,
                                                       box(
                                                         title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("stopsDate1", width = "100%", height = 700)
                                                       )
                                                ),
                                                column(6,
                                                       dataTableOutput("table_all_station_d1")
                                                )
                                       ),
                                       fluidRow(class = "s3r2",
                                                column(6,
                                                       box(
                                                         title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("stopsDate2", width = "100%", height = 700)
                                                       )
                                                ),
                                                column(6,
                                                       dataTableOutput("table_all_station_d2")
                                                ),
                                       )
                                ),
                                column(3,
                                    # leafletOutput("mymap"),
                                    # p(),
                                    # actionButton("recalc", "New points")
                                ),
                                tags$head(tags$style(
                                  " .myRow1{
                                    height:1620px;
                                  }
                                  .container-fluid{
                                    background-color: lightblue;
                                  }")
                                )

                      )
             ),
             tabPanel("About",
                      fluidPage(
                        fluidRow(style="font-size: 40px; padding-bottom: 15%",
                                 h1("CTA Rides Data"),
                                 h4("Author: Wayne Kao"),
                                 h4("Dataset: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
                                 div("The data was taken from the city of chicago page. This app was written to compare the amount of riders from 2001-2021
          from three stations: UIC-Halsted, O'Hare Airport, and 54th/Cermak. For UIC-Halsted, the coloring of the graph coresponds
          more to the UIC school year and timings of the year versus O'Hare and 54th/Cermak looks at more towards overall year
          based on the season. The data goes up to November 2021 so December of 2021 is missing in this dataset. You are able
          to switch graphs between looking at all the riders at a particular station with the following criteria:
          all years from 2001-2021, or all riders categorized by days, months, day of the week with a particular year.
          You are also given an option to view all graphs in a table like structure.")
                        )
                      )
             ),
             tags$style(type="text/css",
                        '.navbar{
    font-size: 20px;
    }')
  )
)

server <- function(input, output, session) {
  monthLimit <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  monthLabel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  weekDayLimit <- c(1,2,3,4,5,6,7)
  weekDayLabel <- c("Sun","Mon", "Tue", "Wed", "Thur", "Fri", "Sat")
  #-----------Utility-------------
  #This returns the ordering option for radio button
  #1 = order alphabetically
  #2 = order by minimum and maximum
  orderOpt <- function(){
    return(switch(input$order,
                  alpha = 1,
                  minmax = 2,
                  1))
  }

  orderOpt2 <- function(){
    return(switch(input$t_order,
                  alpha = 1,
                  minmax = 2,
                  1))
  }

  #-----------Functions for Aggregate Data-------------
  justOneStopReactive_2 <- reactive({subset(dataStations, dataStations$stationname == 'Austin-Forest Park' & year(dataStations$date) == '2001')})
  allStopsByDate <- reactive({subset(dataStations, dataStations$date == ymd(input$date1))}) #Aggregates all stations by specific date
  allStopsDate1 <- reactive({subset(dataStations, dataStations$date == ymd(input$t_date1))}) #Aggregates all stations by specific date
  allStopsDate2 <- reactive({subset(dataStations, dataStations$date == ymd(input$t_date2))}) #Aggregates all stations by specific date
  specificStation <- reactive({subset(dataStations, year(ymd(dataStations$date)) == input$select_year & dataStations$stationname == input$select_station)})
  allyearsStation <- reactive({subset(dataStations, dataStations$stationname == input$select_station)})

  #Returns rides for all stations for current date chosen
  allStopsNoDup <- function(){
    merge <- allStopsByDate()
    merge <- merge[!duplicated(merge$stationname), ] #removes duplicate
    return(merge)
  }

  #Returns rides for all stations for current date chosen
  allStops1 <- function(){
    merge <- allStopsDate1()
    merge <- merge[!duplicated(merge$stationname), ] #removes duplicate
    return(merge)
  }

  #Returns rides for all stations for current date chosen
  allStops2 <- function(){
    merge <- allStopsDate2()
    merge <- merge[!duplicated(merge$stationname), ] #removes duplicate
    return(merge)
  }

  #Returns table with only needed columns
  simpleData <- function(data){
    orderOption <- orderOpt()
    frame <- data.frame(data$stationname, data$date, data$rides)
    if(orderOption == 1){
      frame <- frame[order(data$stationname),]
    }
    else{
      frame <- frame[order(data$rides),]
    }

    return(frame)
  }

  simpleData2 <- function(data){
    orderOption <- orderOpt2()
    frame <- data.frame(data$stationname, data$date, data$rides)
    if(orderOption == 1){
      frame <- frame[order(data$stationname),]
    }
    else{
      frame <- frame[order(data$rides),]
    }

    return(frame)
  }


  simpleSpecificStation <- function(data){
    frame <- data.frame(data$stationname, data$date, data$rides)
    return(frame)
  }

  #-----------Functions for graphs-------------
  #This returns a graph of all stops for a specific date
  graph_stopsByDate <- function(){

    d_graph <- allStopsNoDup()
    g <- NULL
    i_order = orderOpt()

    if(i_order == 2)
      g <- ggplot(data=d_graph, aes(x=reorder(factor(d_graph$stationname),d_graph$rides) , y=d_graph$rides))
    else
      g <- ggplot(data=d_graph, aes(x=factor(d_graph$stationname), y=d_graph$rides))

    g <- g + geom_bar(stat="identity") + labs(x = "Month", y = "Rides", title = "teehee", fill="Month") +
      theme(axis.text.x = element_text(angle = 90, size = 12), axis.text.y = element_text(size = 15))

    return(g)
  }

  graph_stopsDate1 <- function(){

    d_graph <- allStops1()
    g <- NULL
    i_order = orderOpt2()

    if(i_order == 2)
      g <- ggplot(data=d_graph, aes(x=reorder(factor(d_graph$stationname),d_graph$rides) , y=d_graph$rides))
    else
      g <- ggplot(data=d_graph, aes(x=factor(d_graph$stationname), y=d_graph$rides))

    g <- g + geom_bar(stat="identity") + labs(x = "Month", y = "Rides", title = "teehee", fill="Month") +
      theme(axis.text.x = element_text(angle = 90, size = 12), axis.text.y = element_text(size = 15))

    return(g)
  }

  graph_stopsDate2 <- function(){

    d_graph <- allStops2()
    g <- NULL
    i_order = orderOpt2()

    if(i_order == 2)
      g <- ggplot(data=d_graph, aes(x=reorder(factor(d_graph$stationname),d_graph$rides) , y=d_graph$rides))
    else
      g <- ggplot(data=d_graph, aes(x=factor(d_graph$stationname), y=d_graph$rides))

    g <- g + geom_bar(stat="identity") + labs(x = "Month", y = "Rides", title = "teehee", fill="Month") +
      theme(axis.text.x = element_text(angle = 90, size = 12), axis.text.y = element_text(size = 15))

    return(g)
  }

  graph_allStopsYear <- function(){
    p_station <- input$select_station
    p_year <- input$select_year
    g_data <- allyearsStation()
    g<- ggplot(data=g_data, aes(factor(year(ymd(date))), rides, fill=factor(year(ymd(date))))) +
      geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = paste(p_station,"Rides per Year"), fill="Years") +
      theme(axis.text=element_text(size=6))

    return(g)
  }

  graph_eachDay <- function(){
    p_station <- input$select_station
    p_year <- input$select_year
    g_data <- specificStation()
    #in aes: , fill=season
    b<-ggplot(data=g_data, aes(g_data$date, g_data$rides) ) +
      geom_bar(stat="identity") +
      labs(x = paste("Days in", p_year), y = "Rides", title = paste(p_station,"Rides per Day in",p_year))
    #+scale_fill_manual(labels=seasons_label, values=seasons_color)
    return(b)
  }

  graph_eachMonth <- function(){
    p_station <- input$select_station
    p_year <- input$select_year
    g_data <- specificStation()
    #in aes: , fill=season
    c<-ggplot(data=g_data, aes(factor(month(ymd(date))), rides)) +
      geom_bar(stat="identity") +
      scale_x_discrete(limit = factor(monthLimit),labels=monthLabel)
    #+scale_fill_manual(labels=seasons_label, values=seasons_color)
    return(c)
  }

  graph_eachDayOfWeek <- function(){
    p_station <- input$select_station
    p_year <- input$select_year
    g_data <- specificStation()
    d<-ggplot(data=g_data, aes(factor(wday(date)), rides, fill=factor(wday(date)))) +
      geom_bar(stat="identity") +
      labs(x = paste("Weekdays in", p_year), y = "Rides", title = paste(p_station,"Rides per Weekday in", p_year), fill="Weekday") +
      scale_x_discrete(limit = factor(weekDayLimit), labels=weekDayLabel) +
      scale_fill_discrete(name = "Weekday", labels = weekDayLabel)
    return(d)
  }
  #-----------OnActionEvents----------------
  #Scene 1
  #Plot of some date
  output$stopsByDate <- renderPlot({graph_stopsByDate()})

  # Next day button
  observeEvent(input$nextday, {
    updateDateInput(session, "date1",
                    value = ymd(input$date1)+1
                    # min   = paste("2013-04-", x-1, sep=""),
                    # max   = paste("2013-04-", x+1, sep="")
    )
  })

  # Previous day button
  observeEvent(input$previousday, {
    updateDateInput(session, "date1",
                    value = ymd(input$date1)-1
                    # min   = paste("2013-04-", x-1, sep=""),
                    # max   = paste("2013-04-", x+1, sep="")
    )
  })

  output$table_all_station <- renderDataTable(simpleData(allStopsNoDup()),
                                              options = list(
                                                pageLength = 35
                                              )
  )
  output$table_all_station_d1 <- renderDataTable(simpleData2(allStops1()),
                                              options = list(
                                                pageLength = 17
                                              )
  )
  output$table_all_station_d2 <- renderDataTable(simpleData2(allStops2()),
                                              options = list(
                                                pageLength = 17
                                              )
  )

  output$hist2 <- reactive({renderPlot({
    justOneStop_2 <- justOneStopReactive_2()

    ggplot(data=justOneStop_2, aes(wday(ymd(justOneStop_2$date)),  justOneStop_2$rides, fill=factor(month(ymd(justOneStop_2$date))))) +
      geom_bar(stat="identity") + labs(x = "Month", y = "Rides", title = "teehee", fill="Month")
  })})

  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })


  #Scene 2
  output$all_station_years <- renderPlot({
    graph_allStopsYear()
  })

  output$station_each_day <- renderPlot({
    graph_eachDay()
  })

  output$station_each_month <- renderPlot({
    graph_eachMonth()
  })

  output$station_each_day_of_week <- renderPlot({
    graph_eachDayOfWeek()
  })
  output$table_specific_station <- renderDataTable(simpleSpecificStation(specificStation()),
                                              options = list(
                                                pageLength = 17
                                              )
  )

  #Scene 3
  output$stopsDate1 <- renderPlot({graph_stopsDate1()})

  output$stopsDate2 <- renderPlot({graph_stopsDate2()})


}

shinyApp(ui = ui, server = server)
