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
library(kableExtra)
source("Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/R/plot_mesonet.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Comparison of Gridded Data and Mesonet Data"),

      # Sidebar with a slider input for number of bins
      sidebarLayout(

        sidebarPanel(

          selectInput("station", "Mesonet Station:",
                      c("Conrad ARC" = "conradmt",
                        "Corvallis ARC" = "corvalli",
                        "Clearwater SW" = "ebarllob",
                        "Havre ARC" = "havrenmt",
                        "Huntley ARC" = "huntleys",
                        "Kalispell ARC" = "kalispel",
                        "Moccasin ARC" = "moccasin",
                        "Sidney ARC" = "sidneymt")),

          selectInput("variable", "Variable:",
                      c("Maximum Temperature" = "tmax",
                        "Minimum Temperature" = "tmin",
                        "Precipitation" = "ppt")),

          radioButtons("type", "Plot Type:",
                             c("Date vs Value" = "raw_time_plot",
                               "Date vs Deviation from Mesonet" = "time_plot",
                               "Mesonet vs Gridded Data" = "direct_plot",
                               "Cumulative Plot" = "cumsum_plot")),

          radioButtons("agg_type", "Day Definition Method",
                       c("Local Midnight" = "floor",
                       "UTC Noon" = "ceiling")),

          checkboxInput("current", "Show Live Data", FALSE)
        ), # end sidebarPanel

        mainPanel(
          plotly::plotlyOutput("mesPlot"),
          tableOutput("err_table")
        ) #end mainPanel

      ) # end sidebar layout

)

#### read data ####
dat_2017 <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/extracts/mes_grid_2017.csv" %>%
  readr::read_csv(col_types = readr::cols()) %>%
  dplyr::filter(date >= lubridate::as_date("2017-01-01"))

error_2017 <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/error/error_2017.csv" %>%
  readr::read_csv(col_types = readr::cols())

dat_current <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/extracts/mes_grid_current.csv" %>%
  readr::read_csv(col_types = readr::cols()) %>%
  dplyr::filter(date >= lubridate::as_date("2017-01-01"))

error_current <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/error/error_current.csv" %>%
  readr::read_csv(col_types = readr::cols())

#### server function ####
server <- function(input, output, session) {

  observe({

    if(input$current) {

      updateSelectInput(session, "station",
                        choices = list("Fort Keogh ARS N" = "arskeogh",
                                   "Benton Lake W" = "bentlake",
                                   "Argenta BLM" = "blm1arge",
                                   "Virginia City BLM" = "blm2virg",
                                   "McCartney Mtn BLM" = "blm3mcca",
                                   "Kidd BLM" = "blm5kidd",
                                   "Churchill" = "churchil",
                                   "Conrad ARC" = "conradmt",
                                   "Corvallis ARC" = "corvalli",
                                   "Crow Agency E" = "crowagen",
                                   "Clearwater SW" = "ebarllob",
                                   "Fort Benton E" = "ftbentcb",
                                   "Havre ARC" = "havrenmt",
                                   "Huntley ARC" = "huntleys",
                                   "Kalispell ARC" = "kalispel",
                                   "Lubrecht Forest" = "lubrecht",
                                   "Moccasin ARC" = "moccasin",
                                   "Molt W" = "moltwest",
                                   "Rapleje N" = "raplejen",
                                   "Reed Point NE" = "reedpoin",
                                   "Sidney ARC" = "sidneymt",
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

    if(input$current) use_dat <- dat_current
    else use_dat <- dat_2017


    plot_type <- switch(input$type,
                        raw_time_plot = raw_time_plot,
                        time_plot = time_plot,
                        direct_plot = direct_plot,
                        cumsum_plot = cumsum_plot)

    plot_type(use_dat,
              input$variable,
              input$station,
              input$agg_type)


   })

  output$err_table <- function() {

    if(input$current) use_err <- error_current
    else use_err <- error_2017

    if (input$agg_type == "floor") {

      use_err <- use_err %>%
        dplyr::select(-contains("ce"))

    } else if (input$agg_type == "ceiling") {

      use_err <- use_err %>%
        dplyr::select(-contains("fl"))
    }

    use_err %>%
      dplyr::filter(variable == input$variable,
                    station == input$station) %>%
      dplyr::select(-variable, -station) %>%
      magrittr::set_colnames(c("Dataset",
                               "Mean Absolute Error",
                               "Pearson's R",
                               "Mean Bias",
                               "Median Bias")) %>%
      knitr::kable(caption = "Error Statistics for Gridded Data vs Mesonet Data") %>%
        kableExtra::kable_styling(full_width = T,
                                  bootstrap_options = c("striped", "hover", "condensed"),
                                  position = "center")
  }


}

# Run the application
shinyApp(ui, server)
