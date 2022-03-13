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

print("===================NEW=====================")

myfiles <- list.files(pattern="*.csv", full.names=TRUE)
dataStations <- do.call(rbind, lapply(myfiles, read.csv, header = FALSE))
colnames(dataStations) <- c("", "station_id", "stationname", "date", "daytype", "rides", "STOP_ID", "Location")
dataStations$date <- mdy(dataStations$date)
dataStations$rides <- as.numeric(gsub(",", "", dataStations$rides))
markerFrame <- NULL
markerFrame3 <- NULL

dtdate <- subset(dataStations, dataStations$date == "2021-08-23")
dtdate <- dtdate[!duplicated(dtdate$stationname), ] #removes duplicate

dtdate3 <- subset(dataStations, dataStations$date == "2021-07-22")
dtdate3 <- dtdate3[!duplicated(dtdate$stationname), ] #removes duplicate
merged_2 <- merge(x=dtdate, y=dtdate3, by="stationname")


#leaflet data
coordinates <- dataStations[!duplicated(dataStations$stationname), ]
lat = c()
long = c()
for(row in 1:length(coordinates[,1])){
  curr_coord = coordinates[row,]$Location
  size <- nchar(as.character(curr_coord))
  coord <- substr(curr_coord, 2, size)
  size <- nchar(as.character(coord))
  coord <- substr(coord, 1, size-1)
  coord_arr <- strsplit(coord, split=",")
  coord_frame<-matrix(unlist(coord_arr[1]), ncol=1)
  lat <- append(lat, coord_frame[1,])
  long <- append(long,coord_frame[2,])

}

coordinates$latitude <- lat
coordinates$longitude <- long

leafData <- data.frame(coordinates$stationname, coordinates$latitude, coordinates$longitude)
colnames(leafData) <- c("stationname", "lat", "long")
#########

markerFrame <- merge(x=dtdate, y=leafData, by="stationname")
markerFrame3 <- merge(x=merged_2, y=leafData, by="stationname")

#markerFrame2
colnames(dtdate3) <- c()

years <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
stationNamesDropDown <- unique(dataStations$stationname)

ui <- shinyUI(
  navbarPage("CTA Riders", position = "fixed-bottom",
             tabPanel("By Date",
                      fluidPage(style="background-color: lightblue",
                                column(1, style = "height:1620px;background-color: orange",
                                       column(12,
                                              br(),br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                              br(),br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
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
                                column(8,
                                        fluidRow(
                                               box(
                                                 title = " ", solidHeader = TRUE, status = "primary", width = 12,
                                                 plotOutput("stopsByDate", width = "100%", height = 700)
                                               )
                                        ),
                                       br(),
                                        fluidRow(
                                               dataTableOutput("table_all_station")
                                        )


                                ),
                                column(3,
                                       leafletOutput("mymap", height=1520, width="100%")
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
             tabPanel("By Station",
                      fluidPage(style="background-color: lightblue",
                                column(1, style = "height:1620px;background-color: orange",
                                       column(12,
                                              br(),br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                              br(),br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                          selectInput("select_station", "Select Station", stationNamesDropDown, selected = "UIC-Halsted"),
                                          selectInput("select_year", "Select Year", years, selected = 2010)
                                       )
                                ),
                                column(8,
                                       fluidRow(class = "s2r1",
                                                column(4,
                                                       box(
                                                         title = " ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("station_each_day", width = "100%", height = 700)
                                                       )
                                                ),
                                                column(4,
                                                       box(
                                                         title = " ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("station_each_month", width = "100%", height = 700)
                                                       )
                                                ),
                                                column(4,
                                                       box(
                                                         title = " ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("station_each_day_of_week", width = "100%", height = 700)
                                                       )
                                                )
                                       ),
                                       br(),
                                       fluidRow(class = "s2r2",
                                                column(9,
                                                       box(
                                                         title = " ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("all_station_years", width = "100%", height = 700)
                                                       )
                                                ),#table_specific_station

                                                column(3,
                                                       dataTableOutput("table_specific_station")
                                                )
                                       ),
                                ),
                                column(3,
                                       leafletOutput("mymap2", height=1520, width="100%")
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
             tabPanel("Compare",
                      fluidPage(style="background-color: lightblue",
                                column(1, style = "height:1620px;background-color: orange",
                                       column(12,
                                              br(),br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                              br(),br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                              dateInput("t_date1", "Date 1:", value = "2021-08-23"),
                                              br(),
                                              dateInput("t_date2", "Date 2:", value = "2021-07-22"),
                                              radioButtons("t_order", "Order By:",
                                                           c("Alphabetical" = "alpha",
                                                             "Minimum to Maximum" = "minmax")
                                              )
                                       )
                                ),
                                column(8,
                                       fluidRow(class = "s3r1",
                                                column(9,
                                                       box(
                                                         title = "Graph using Date 1: ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("stopsDate1", width = "100%", height = 700)
                                                       )
                                                ),
                                                br(),
                                                column(3,
                                                       dataTableOutput("table_all_station_d1")
                                                )
                                       ),
                                       br(),
                                       fluidRow(class = "s3r2",
                                                column(9,
                                                       box(
                                                         title = "Graph using Date 2: ", solidHeader = TRUE, status = "primary", width = 12,
                                                         plotOutput("stopsDate2", width = "100%", height = 700)
                                                       )
                                                ),
                                                br(),
                                                column(3,
                                                       dataTableOutput("table_all_station_d2")
                                                ),
                                       )
                                ),
                                column(3,
                                    leafletOutput("mymap3", height=1520, width="100%")

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
                                 h4("Author: Wayne Kao and Raphael Genova"),
                                 h4("Dataset: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f and https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme"),
                                 div("The data was taken from the city of chicago page. This app was written to compare the amount of riders from Jan 1, 2001- November 21, 2021 along with the locations of all CTA stops by longitude and latitude.
          from all CTA stations. Each page has its own unique functionality to fit the proper visualizations and format. By default, the site goes to the 'By Date' tab. The 'By Date' tab contains a bar graph that has all of the ridership
          of each stop at a particular date and there is also a table that gives more detail about what is displayed on the bar graph. This tab also contains a leaflet which shows geographically where each CTA stop is (which is indicated
          by a blue point). The 'By Station' tab allows the use to compare between two stations at specific years. The bar graphs give ridership numbers per Day, Month, and Weekday. The 'Compare' tab contains two bar graphs and tables each
          graph/table set represents a particular date the user would like to explore. The graphs and tables display the total ridership of each stop at that particular day.")
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
  allStopsByDate <- reactive({subset(dataStations, dataStations$date == ymd(input$date1))}) #Aggregates all stations by specific date
  allStopsDate1 <- reactive({subset(dataStations, dataStations$date == ymd(input$t_date1))}) #Aggregates all stations by specific date
  allStopsDate2 <- reactive({subset(dataStations, dataStations$date == ymd(input$t_date2))}) #Aggregates all stations by specific date
  specificStation <- reactive({subset(dataStations, year(ymd(dataStations$date)) == input$select_year & dataStations$stationname == input$select_station)})
  allyearsStation <- reactive({subset(dataStations, dataStations$stationname == input$select_station)})

  #Returns a label on the
  leafRides <- function(station, date){
    print(date)
    print(station)
    temp <- subset(dataStations, dataStations$stationname == station & dataStations$date == date)
    print(temp)
    ridesLabel <- temp$rides
    label <- paste(sep="<br/>", station, ridesLabel)
    return(label)
  }

  #Returns rides for all stations for current date chosen
  allStopsNoDup <- function(){
    merge <- allStopsByDate()
    merge <- merge[!duplicated(merge$stationname), ] #removes duplicate
    return(merge)
  }

  leafMakerFrame <- function(){
    d_temp <- allStopsNoDup()
    d_temp <- d_temp[!duplicated(d_temp$stationname), ] #removes duplicate
    m_temp <- merge(x=d_temp, y=leafData, by="stationname")
    return(m_temp)
  }

  leafMakerFrame3 <- function(){
    d_temp <- allStopsDate1()
    d_temp <- d_temp[!duplicated(d_temp$stationname), ] #removes duplicate

    d_temp2 <- allStopsDate2()
    d_temp2 <- d_temp2[!duplicated(d_temp2$stationname), ] #removes duplicate
    merged_temp <- merge(x=d_temp, y=d_temp2, by="stationname")

    m_temp <- merge(x=merged_temp, y=leafData, by="stationname")
    return(m_temp)
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

    titleBuild <- paste("Total Number of Rides per Station in", input$date1)
    g <- g + geom_bar(stat="identity") + labs(x = "Station Names", y = "Total # of Rides", title = titleBuild, fill="Station Names") +
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

    titleBuild <- paste("Total Number of Rides per Station in", input$t_date1)
    g <- g + geom_bar(stat="identity", fill="#9933FF") + labs(x = "Station Names", y = "Total # of Rides", title = titleBuild, fill="Station Names") +
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

    titleBuild <- paste("Total Number of Rides per Station in", input$t_date2)
    g <- g + geom_bar(stat="identity", fill="lightgreen") + labs(x = "Station Names", y = "Total # of Rides", title = titleBuild, fill="Station Names") +
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
      geom_bar(stat="identity", fill="orange") +
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
      geom_bar(stat="identity", fill="lightpink") +
      labs(x = paste("Months in", p_year), y = "Rides", title = paste(p_station,"Rides per Month in",p_year)) +
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
    #scale_fill_manual(labels = c("Weekend", "Work Day", "Work Day", "Work Day", "Work Day", "Work Day", "Weekend"), values=c("darkblue", "#33FFFF", "#33FFFF", "#33FFFF", "#33FFFF", "#33FFFF", "darkblue"))
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

  observeEvent(input$date1, {
    markerFrame <- leafMakerFrame()
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addCircleMarkers(lng=as.numeric(markerFrame$long), lat=as.numeric(markerFrame$lat),
                       popup=paste(sep="<br/>", markerFrame$stationname, markerFrame$rides))
  })

  observeEvent(input$t_date2, {
    markerFrame3 <- leafMakerFrame3()
    leafletProxy("mymap3") %>%
      clearMarkers() %>%
      addCircleMarkers(lng=as.numeric(markerFrame3$long), lat=as.numeric(markerFrame3$lat),
                       popup=paste(sep="<br/>", markerFrame3$stationname, markerFrame3$rides.x, markerFrame3$rides.y))
  })

  observeEvent(input$t_date1, {
    markerFrame3 <- leafMakerFrame3()
    leafletProxy("mymap3") %>%
      clearMarkers() %>%
      addCircleMarkers(lng=as.numeric(markerFrame3$long), lat=as.numeric(markerFrame3$lat),
                       popup=paste(sep="<br/>", markerFrame3$stationname, markerFrame3$rides.x, markerFrame3$rides.y))
  })

  output$table_all_station <- renderDataTable(simpleData(allStopsNoDup()),
                                              colnames = c('Station Name', 'Date', 'Rides'),
                                              options = list(
                                                pageLength = 17
                                              )
  )
  output$table_all_station_d1 <- renderDataTable(simpleData2(allStops1()),
                                                 colnames = c('Station Name', 'Date', 'Rides'),
                                              options = list(
                                                pageLength = 17
                                              )
  )
  output$table_all_station_d2 <- renderDataTable(simpleData2(allStops2()),
                                                 colnames = c('Station Name', 'Date', 'Rides'),
                                              options = list(
                                                pageLength = 17
                                              )
  )

  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng=-87.6298, lat = 41.8681, zoom = 13) %>%
      addCircleMarkers(lng=as.numeric(markerFrame$long), lat=as.numeric(markerFrame$lat),
                       popup=paste(sep="<br/>", markerFrame$stationname, markerFrame$rides))
  })

  output$mymap3 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng=-87.6298, lat = 41.8681, zoom = 13) %>%
      addCircleMarkers(lng=as.numeric(markerFrame3$long), lat=as.numeric(markerFrame3$lat),
                       popup=paste(sep="<br/>", markerFrame3$stationname, markerFrame3$rides.x, markerFrame3$rides.y))
  })

  output$mymap2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng=-87.6298, lat = 41.8681, zoom = 13) %>%
      addCircleMarkers(lng=as.numeric(leafData$long), lat=as.numeric(leafData$lat),
                       popup=paste(sep="<br/>", leafData$stationname))
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
                                                   colnames = c('Station Name', 'Date', 'Rides'),
                                              options = list(
                                                pageLength = 17
                                              )
  )

  #Scene 3
  output$stopsDate1 <- renderPlot({graph_stopsDate1()})

  output$stopsDate2 <- renderPlot({graph_stopsDate2()})


}

shinyApp(ui = ui, server = server)
