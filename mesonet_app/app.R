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
library(gridExtra)
source("./scripts/plot_mesonet.R")
source("./scripts/new_error_functionsv2.R")
source("./scripts/error_analysis.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Comparison of Gridded Data and Mesonet Data"),

  tabsetPanel(

    #### Mesonet Gridded Comparison ####
    tabPanel(
      "Mesonet-Gridded Comparison",

      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "station", "Mesonet Station:",
            c(
              "Conrad ARC" = "conradmt",
              "Corvallis ARC" = "corvalli",
              "Clearwater SW" = "ebarllob",
              "Havre ARC" = "havrenmt",
              "Huntley ARC" = "huntleys",
              "Kalispell ARC" = "kalispel",
              "Moccasin ARC" = "moccasin",
              "Sidney ARC" = "sidneymt"
            )
          ),

          selectInput(
            "variable", "Variable:",
            c(
              "Maximum Temperature" = "tmax",
              "Minimum Temperature" = "tmin",
              "Precipitation" = "ppt"
            )
          ),

          radioButtons(
            "type", "Plot Type:",
            c(
              "Date vs Value" = "raw_time_plot",
              "Date vs Deviation from Mesonet" = "time_plot",
              "Mesonet vs Gridded Data" = "direct_plot",
              "Cumulative Plot" = "cumsum_plot"
            )
          ),

          radioButtons(
            "agg_type", "Day Definition Method",
            c(
              "Local Midnight" = "floor",
              "UTC Noon" = "ceiling"
            )
          ),

          checkboxInput("current", "Show Live Data", FALSE)
        ), # end sidebarPanel

        mainPanel(
          plotly::plotlyOutput("mesPlot"),
          tableOutput("err_table")
        ) # end mainPanel
      ) # end sidebar layout
    ), # end first tab panel


    #### T-Tests (initial) ####
    # tabPanel(
    #   "Mesonet-Gridded T-Test Results",
    #
    #   sidebarLayout(
    #     sidebarPanel(
    #       selectInput(
    #         "variable2", "Variable:",
    #         c(
    #           "Maximum Temperature" = "tmax",
    #           "Minimum Temperature" = "tmin",
    #           "Precipitation" = "ppt"
    #         )
    #       ),
    #
    #       radioButtons(
    #         "year", "Year:",
    #         c(
    #           "2017" = 2017,
    #           "2018" = 2018
    #         )
    #       ),
    #
    #       selectInput("datasets", "Datasets to Compare:", ""),
    #
    #       numericInput("win", "Window (# of Days)",
    #         value = 10,
    #         min = 10, max = 50, step = 1
    #       ),
    #
    #       actionButton("button", "Run T-Test (this will take a few seconds)")
    #     ), # end sidebarPanel
    #
    #     mainPanel(
    #       plotOutput("errorPlot")
    #     ) # end mainPanel
    #   )
    # ),# end tabpanel
    #### Temperature T-Tests ####
    tabPanel(
      "Temperature T-Test Results",

      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "variable2", "Variable:",
            c(
              "Maximum Temperature" = "tmax",
              "Minimum Temperature" = "tmin"
            )
          ),

          selectInput(
            "dataset1", "First Dataset to Compare",
            c(
              "Daymet" = "daymet",
              "gridMet" = "gridmet",
              "PRISM" = "prism"
            )),

          selectInput("dataset2", "Second Dataset to Compare", ""),

          radioButtons(
            "metric", "Error Metric to Test:",
            c(
              "Bias" = "bias",
              "Absolute Error" = "abs_error"
            )
          ),

          radioButtons(
            "test", "Test Type:",
            c(
              "T-Test" = "t",
              "Kolmogorov-Smirnov Test" = "ks",
              "Mann-Whitney Test" = "mw"
            )
          ),

          numericInput("win", "Window (# of Days)",
                       value = 1,
                       min = 1, max = 50, step = 1
          ),

          actionButton("button1", "Run Test (this will take a few seconds)")
        ), # end sidebarPanel

        mainPanel(
          plotOutput("ttesttmp")
        ) # end mainPanel
      ) # end sidebarLayout
    ), # end tabPanel

    #### Precip T-Test ####
    tabPanel(
      "Precip T-Test Results",

      sidebarLayout(
        sidebarPanel(

          selectInput("datasetsPpt", "Datasets to Compare:", ""),

          numericInput("winPpt", "Window (# of Days)",
                       value = 10,
                       min = 10, max = 50, step = 1
          ),

          actionButton("buttonPpt", "Run T-Test (this will take a few seconds)")
        ), # end sidebarPanel

        mainPanel(
          plotOutput("ttestppt")
        ) # end mainPanel
      ) # end sidebarLayout
    ) # end tabPanel
  ) # end tabsetpanel
) # end fluidpage

#### read data ####
dat_2017 <- "./data/mes_grid_2017.csv" %>%
  readr::read_csv(col_types = readr::cols()) %>%
  dplyr::filter(date >= lubridate::as_date("2017-01-01"))

error_2017 <- "./data/error_2017.csv" %>%
  readr::read_csv(col_types = readr::cols())

dat_current <- "./data/mes_grid_current.csv" %>%
  readr::read_csv(col_types = readr::cols()) %>%
  dplyr::filter(date >= lubridate::as_date("2017-01-01"))

error_current <- "./data/error_2018.csv" %>%
  readr::read_csv(col_types = readr::cols())



#### server function ####
server <- function(input, output, session) {
  observe({
    if (input$current) {
      updateSelectInput(session, "station",
        choices = list(
          "Fort Keogh ARS N" = "arskeogh",
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
          "Turek Ranch" = "turekran"
        )
      )
    } else {
      updateSelectInput(session, "station",
        choices = list(
          "Conrad ARC" = "conradmt",
          "Corvallis ARC" = "corvalli",
          "Clearwater SW" = "ebarllob",
          "Havre ARC" = "havrenmt",
          "Huntley ARC" = "huntleys",
          "Kalispell ARC" = "kalispel",
          "Moccasin ARC" = "moccasin",
          "Sidney ARC" = "sidneymt"
        )
      )
    }

    if (input$type == "cumsum_plot") {
      updateSelectInput(session, "variable",
        choices = list("Precipitation" = "ppt")
      )
    } else {
      updateSelectInput(session, "variable",
        choices = list(
          "Maximum Temperature" = "tmax",
          "Minimum Temperature" = "tmin",
          "Precipitation" = "ppt"
        )
      )
    }
  })


  output$mesPlot <- plotly::renderPlotly({
    if (input$current) {
      use_dat <- dat_current
    } else {
      use_dat <- dat_2017
    }


    plot_type <- switch(input$type,
      raw_time_plot = raw_time_plot,
      time_plot = time_plot,
      direct_plot = direct_plot,
      cumsum_plot = cumsum_plot
    )

    plot_type(
      use_dat,
      input$variable,
      input$station,
      input$agg_type
    )
  })

  output$err_table <- function() {
    if (input$current) {
      use_err <- error_current
    } else {
      use_err <- error_2017
    }

    if (input$agg_type == "floor") {
      use_err <- use_err %>%
        dplyr::select(-contains("ce"))
    } else if (input$agg_type == "ceiling") {
      use_err <- use_err %>%
        dplyr::select(-contains("fl"))
    }

    use_err %>%
      dplyr::filter(
        variable == input$variable,
        station == input$station
      ) %>%
      dplyr::select(
        -variable, -station, -Landform,
        -Aspect, -Slope, -full_name, -Elevation
      ) %>%
      magrittr::set_colnames(c(
        "Dataset",
        "Mean Absolute Error",
        "Pearson's R",
        "Mean Bias",
        "Median Bias"
      )) %>%
      knitr::kable(caption = "Error Statistics for Gridded Data vs Mesonet Data") %>%
      kableExtra::kable_styling(
        full_width = T,
        bootstrap_options = c("striped", "hover", "condensed"),
        position = "center"
      )
  }

  observe({

    items = list("Daymet" = "daymet",
                 "gridMet" = "gridmet",
                 "PRISM" = "prism")

    dataset_check = list("daymet", "gridmet", "prism")

    updateSelectInput(
      session, "dataset2",
      choices = items[which(dataset_check != input$dataset1)]
    )
  })

  error_plot_vals <- eventReactive(input$button1, {
    list(input$dataset1, input$dataset2, input$metric,
         input$test, input$variable2, input$win)
  })

  output$ttesttmp <- renderPlot({

    dat <- significance_test(
      error_plot_vals()[[1]], error_plot_vals()[[2]], error_plot_vals()[[3]],
      error_plot_vals()[[4]], error_plot_vals()[[5]], error_plot_vals()[[6]]
      )



    gridExtra::grid.arrange(
      grobs = list(plot_bias(dat),
                   bias_box(dat, error_plot_vals()[[4]]),
                   plot_abs(dat),
                   abs_box(dat, error_plot_vals()[[4]])),
      ncol = 2,
      nrow = 2
    )

    # gridExtra::grid.arrange(
    #   grobs = list(plot_bias(dat),
    #                plot_abs(dat),
    #                plot_test(dat)),
    #   ncol = 1,
    #   nrow = 3
    # )


  }, height = 800, width = 1050)
  # observe({
  #   if (input$year == 2017) {
  #     if (input$variable2 == "ppt") {
  #       updateSelectInput(session, "datasets",
  #         choices = list(
  #           "CHIRPS vs Daymet" = "chirps-daymet",
  #           "CHIRPS vs gridMet" = "chirps-gridmet",
  #           "CHIRPS vs PRISM" = "chirps-prism",
  #           "Daymet vs gridMet" = "daymet-gridmet",
  #           "Daymet vs PRISM" = "daymet-prism",
  #           "gridMet vs PRISM" = "gridmet-prism"
  #         )
  #       )
  #     } else {
  #       updateSelectInput(session, "datasets",
  #         choices = list(
  #           "Daymet vs gridMet" = "daymet-gridmet",
  #           "Daymet vs PRISM" = "daymet-prism",
  #           "gridMet vs PRISM" = "gridmet-prism"
  #         )
  #       )
  #     }
  #   } else if (input$year == 2018) {
  #     updateSelectInput(session, "datasets",
  #       choices = list("gridMet vs PRISM" = "gridmet-prism")
  #     )
  #   }
  # })
  #
  # error_plot_vals <- eventReactive(input$button, {
  #   list(input$variable2, input$year, input$win, input$datasets)
  # })
  #
  # output$errorPlot <- renderPlot({
  #   out_dat <- plot_t_test(
  #     error_plot_vals()[[1]],
  #     error_plot_vals()[[2]],
  #     error_plot_vals()[[3]]
  #   )
  #
  #   out_index <- which(names(out_dat) == error_plot_vals()[[4]])
  #
  #   gridExtra::grid.arrange(
  #     grobs = out_dat[[out_index]],
  #     ncol = 1,
  #     nrow = 2
  #   )
  # })
}

# Run the application
shinyApp(ui, server)
