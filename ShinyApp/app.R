# CTA stations data
# This app takes CTA station data, from UIC, O'Hare airport, and
# and gives visualizations on riders within each year, month, day, day of the week
# Author: Wayne Kao

#TODO:
# Test on Shiny app


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


# assume all of the tsv files in this directory are data of the same kind that I want to visualize
# read.table(file="uic.tsv", quote="", sep="\t", header=TRUE)
# ohare <- read.table(file="ohare.tsv", quote="", sep="\t", header=TRUE)
# cermak <- read.table(file="cermak.tsv", quote="", sep="\t", header=TRUE)

myfiles <- list.files(pattern="*.csv", full.names=TRUE)
dataStations <- do.call(rbind, lapply(myfiles, read.csv, header = FALSE))
colnames(dataStations) <- c("", "station_id", "stationname", "date", "daytype", "rides", "STOP_ID", "Location")
dataStations$date <- mdy(dataStations$date)

stations <- c("UIC-Halsted", "O'Hare Airport", "54th/Cermak")
graphs <- c("All Years", "Each Day", "Each Month", "Each Day of Week", "Table")
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
        column(2, style = "height:1620px;background-color: orange"),
        column(10,style = "height:200px;",
          fluidRow(class = "myRow1",
            column(4,style = "height:200px;background-color: yellow",
                   box(
                     title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("hist2", width = "100%", height = 1400)
                   )
            ),
            column(4,style = "height:200px;background-color: blue"),
            column(4,style = "height:200px;background-color: green",
              box(
               title = "Graph 1: ", solidHeader = TRUE, status = "primary", width = 12,
               plotOutput("hist1", width = "100%", height = 1400)
              )
            ),
          )
        ),

        # fluidRow(class = "myRow2",
        #         column(6,div(style = "height:100px;background-color: green;", "Bottomleft")),
        #         column(6,div(style = "height:150px;background-color: red;", "Bottomright"))),
        tags$head(tags$style(
          ".myRow1{height:1620px;}")
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

server <- function(input, output) {
  justOneStopReactive_2 <- reactive({subset(dataStations, dataStations$stationname == 'Austin-Forest Park' & year(dataStations$date) == '2001')})

  output$hist1 <- renderPlot({
    justOneStop_2 <- justOneStopReactive_2()

    ggplot(data=justOneStop_2, aes(wday(ymd(justOneStop_2$date)),  justOneStop_2$rides, fill=factor(month(ymd(justOneStop_2$date))))) +
      geom_bar(stat="identity") + labs(x = "Month", y = "Rides", title = "teehee", fill="Month")
  })

  output$hist2 <- renderPlot({
    justOneStop_2 <- justOneStopReactive_2()

    ggplot(data=justOneStop_2, aes(wday(ymd(justOneStop_2$date)),  justOneStop_2$rides, fill=factor(month(ymd(justOneStop_2$date))))) +
      geom_bar(stat="identity") + labs(x = "Month", y = "Rides", title = "teehee", fill="Month")
  })
}

shinyApp(ui = ui, server = server)
