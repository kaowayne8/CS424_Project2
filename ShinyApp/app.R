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
uic <- read.table(file="uic.tsv", quote="", sep="\t", header=TRUE)
ohare <- read.table(file="ohare.tsv", quote="", sep="\t", header=TRUE)
cermak <- read.table(file="cermak.tsv", quote="", sep="\t", header=TRUE)


stations <- c("UIC-Halsted", "O'Hare Airport", "54th/Cermak")
graphs <- c("All Years", "Each Day", "Each Month", "Each Day of Week", "Table")
years <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)

# Create the shiny dashboard
ui <- shinyUI(
  navbarPage("CTA Riders", position = "fixed-bottom",
             tabPanel("Plot",
                      fluidPage(
                        fluidRow(style="padding-top: 5%",
                          #UIC Halsted (left) chart
                          column(1),
                          column(5,
                                 plotOutput("left_plot", height = 750)
                          ),
                          #O'Hare (right) chart
                          column(5,
                                 plotOutput("right_plot", height = 750)
                          ),
                          column(1)
                        ),

                        #fluidRow( style="padding-top: 2%; padding-bottom: 18%",
                          #UIC Halsted (left) control panel
                         # column(6, align="center",
                          #      selectInput("select_left", "Select Station", stations, selected="UIC-Halsted"),
                           #     selectInput("select_left_graph", "Select Graph", graphs, selected="All Years"),
                            #    selectInput("select_left_year", "Select Year", years, selected="2021")

                          #),
                          #O'Hare (right) control panel
                          #column(6, align="center",
                           #      selectInput("select_right", "Select Station", stations, selected="O'Hare Airport"),
                            #     selectInput("select_right_graph", "Select Graph", graphs, selected="All Years"),
                             #    selectInput("select_right_year", "Select Year", years, selected="2021")
                          #)
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



}

shinyApp(ui = ui, server = server)
