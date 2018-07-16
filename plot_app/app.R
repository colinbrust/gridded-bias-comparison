#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(readr)
source("Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/R/plot_mesonet.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Compaison of Gridded and Mesonet Data from 2017-01-01 to 2018-01-01"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(

      sidebarPanel(

        selectInput("station", "Mesonet Station:",
                    c("Conrad" = "conradmt",
                      "Corvallis" = "corvalli",
                      "E Bar L" = "ebarllob",
                      "Havre" = "havrenmt",
                      "Huntley's" = "huntleys",
                      "Kalispell" = "kalispel",
                      "Moccasin" = "moccasin",
                      "Sidney" = "sidneymt")),

        selectInput("variable", "Variable:",
                    c("Maximum Temperature" = "tmax",
                      "Minimum Temperature" = "tmin",
                      "Precipitation" = "ppt"))
      ),

      mainPanel(
        plotly::plotlyOutput("datePlot"),
        verbatimTextOutput("event")
      )
      # Show a plot of the generated distribution
      # mainPanel(
      #    plotOutput("datePlot")
      # )
   )
)

dat <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/extracts/all_20170101_20180101.csv" %>%
  readr::read_csv(col_types = readr::cols())

# Define server logic required to draw a histogram
server <- function(input, output) {



   output$datePlot <- plotly::renderPlotly({

    raw_time_plot(dat, "2017-01-01", "2018-01-01",
                  input$variable, input$station)
   })

   output$event <- renderPrint({
     d <- event_data("plotly_hover")
     if (is.null(d)) "Hover on a point!" else d
   })
}

# Run the application
shinyApp(ui, server)

