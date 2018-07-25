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
                               "Mesonet vs Gridded Data" = "direct_plot")),

          checkboxInput("current", "Show Live Data", FALSE)
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
                          "Landform" = "Landform")),

           checkboxInput("current2", "Show Live Data", FALSE)
         ), # end sidebarPanel

         mainPanel(
           plotly::plotlyOutput("varPlot")
         ) #end mainPanel

       ) # end sidebar layout

    )

  ) # end tabsetPanel
)

#### read data ####
dat1 <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/extracts/test.csv" %>%
  readr::read_csv(col_types = readr::cols()) %>%
  dplyr::mutate(station = factor(station),
                dataset = factor(dataset))

dat2 <- list.files("Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/extracts",
                   full.names = T,
                   pattern = "mes_grid") %>% readr::read_csv() %>%
  dplyr::mutate(date = lubridate::as_datetime(date)) %>%
  dplyr::mutate(date = dplyr::if_else(dataset == "prism", true = date - 86400,
                                      false = date))

#### server function ####
server <- function(input, output, session) {

  observe({

    if(input$current) {

      updateSelectInput(session, "station",
                        choices = list("Ft. Keogh" = "arskeogh",
                                   "Benton Lake" = "bentlake",
                                   "Argenta" = "blm1arge",
                                   "Virginia City" = "blm2virg",
                                   "BLM MCCA" = "blm3mcca",
                                   "BLM Kidd" = "blm5kidd",
                                   "Churchill" = "churchil",
                                   "Conrad" = "conradmt",
                                   "Corvallis" = "corvalli",
                                   "Crow Agency" = "crowagen",
                                   "E Bar L" = "ebarllob",
                                   "Ft. Benton" = "ftbentcb",
                                   "Havre" = "havrenmt",
                                   "Huntley's" = "huntleys",
                                   "Kalispell" = "kalispel",
                                   "Lubrecht" = "lubrecht",
                                   "Moccasin" = "moccasin",
                                   "Molt West" = "moltwest",
                                   "Rapple J" = "rapplejen",
                                   "Reed Point" = "reedpoin",
                                   "Sidney" = "sidneymt",
                                   "Suat" = "suatnasa",
                                   "Turek Ranch" = "turekran"))
    } else{

      updateSelectInput(session, "station",
                        choices = list("Conrad" = "conradmt",
                                        "Corvallis" = "corvalli",
                                        "E Bar L" = "ebarllob",
                                        "Havre" = "havrenmt",
                                        "Huntley's" = "huntleys",
                                        "Kalispell" = "kalispel",
                                        "Moccasin" = "moccasin",
                                        "Sidney" = "sidneymt"))
    }

  })


  output$mesPlot <- plotly::renderPlotly({

    if(input$current) use_dat <- dat2
    else use_dat <- dat1


    plot_type <- switch(input$type,
                        raw_time_plot = raw_time_plot,
                        time_plot = time_plot,
                        direct_plot = direct_plot)

    plot_type(use_dat,
              input$variable,
              input$station)
   })

  output$varPlot <- plotly::renderPlotly({

    if(input$current) use_dat <- dat2
    else use_dat <- dat1

    var_plot(use_dat,
             input$variable2,
             input$var)

  })


}

# Run the application
shinyApp(ui, server)
