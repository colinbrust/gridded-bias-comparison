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

  tabsetPanel(

    #### Mesonet Comparison ####
    tabPanel("Mesonet Comparison",

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
                        "Precipitation" = "ppt")),

          radioButtons("type", "Plot Type:",
                             c("Date vs Value" = "raw_time_plot",
                               "Date vs Deviation from Mesonet" = "time_plot",
                               "Mesonet vs Gridded Data" = "direct_plot"))
        ), # end sidebarPanel

        mainPanel(
          plotly::plotlyOutput("mesPlot")
        ) #end mainPanel

      ) # end sidebar layout

    ), # end first tabPanel

    #### Mesonet Variable Comparison ####
    tabPanel("Mesonet Variable Comparison",
       # Sidebar with a slider input for number of bins
       sidebarLayout(

         sidebarPanel(

           selectInput("variable2", "Variable:",
                       c("Maximum Temperature" = "tmax",
                         "Minimum Temperature" = "tmin",
                         "Precipitation" = "ppt")),

           selectInput("var", "Comparison Variable:",
                        c("Elevation" = "Elevation",
                          "Slope" = "Slope",
                          "Aspect" = "Aspect",
                          "Landform" = "Landform"))
         ), # end sidebarPanel

         mainPanel(
           plotly::plotlyOutput("varPlot")
         ) #end mainPanel

       ) # end sidebar layout

    )

  ) # end tabsetPanel
)

#### read data ####
dat <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/extracts/all_20170101_20180101.csv" %>%
  readr::read_csv(col_types = readr::cols())

analysis_dates <- seq(lubridate::as_date("2017-01-01"),
                      lubridate::as_date("2018-01-01"),
                      by = "days") %>%
  head(-1)

#### server function ####
server <- function(input, output) {

  output$mesPlot <- plotly::renderPlotly({


    plot_type <- switch(input$type,
                        raw_time_plot = raw_time_plot,
                        time_plot = time_plot,
                        direct_plot = direct_plot)

    plot_type(dat, analysis_dates,
              input$variable, input$station)
   })

  output$varPlot <- plotly::renderPlotly({

    var_plot(dat, analysis_dates,
             input$variable2, input$var)

  })


}

# Run the application
shinyApp(ui, server)
