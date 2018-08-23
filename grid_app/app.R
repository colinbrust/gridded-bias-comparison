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

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  shinyjs::useShinyjs(),

  titlePanel("Inter-Model Comparison of Gridded Temperature and Precipitation Products in Montana"),

  tabsetPanel(
    #### Box, Density and Correlation Plots ####
    tabPanel(
      "Box, Density, and Correlation Plots",

      sidebarLayout(
        sidebarPanel(
          selectInput(
            "type", "Plot Type:",
            c(
              "Boxplot" = "box",
              "Density Plot" = "den",
              "Correllogram" = "corr"
            )
          ),

          selectInput(
            "time", "Temporal Scale:",
            c(
              "Annual" = "Annual",
              "Seasonal" = "Seasonal",
              "Monthly" = "Monthly"
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
            "stat", "Statistic:",
            c(
              "Normal" = "Normal",
              "Standard Deviation" = "SD"
            )
          ),

          checkboxInput("dev", "Show Deviation from Ensemble Mean", FALSE)
        ), # end of sidebarPanel

        mainPanel(plotOutput("boxPlot"))
      ) # end of sidebarLayout
    ), # end first tabPanel

    #### elevation plots ####
    tabPanel(
      "Elevation Plots",

      sidebarLayout(
        sidebarPanel(
          selectInput(
            "type2", "Elevation Plot Type:",
            c(
              "Deviation from Ensemble Average" = "devT",
              "Raw Dataset Values" = "devF",
              "Elevation w/ Map of Climate Divisions" = "maps"
            )
          ),

          radioButtons(
            "stat2", "Statistic:",
            c(
              "Normal" = "Normal",
              "Standard Deviation" = "SD"
            )
          ),

          selectInput("variable2", "Variable:", ""),

          selectInput(
            "cd", "Climate Division:",
            c(
              "Central" = "CENTRAL",
              "North Central" = "NORTH CENTRAL",
              "Northeastern" = "NORTHEASTERN",
              "South Central" = "SOUTH CENTRAL",
              "Southeastern" = "SOUTHEASTERN",
              "Southwestern" = "SOUTHWESTERN",
              "Western" = "WESTERN"
            )
          )
        ), # end of sidebarPanel

        mainPanel(plotOutput("elevPlot"))
      ) # end of sidebarLayout
    ),

    #### maps ####
    tabPanel(
      "Maps",

      sidebarLayout(
        sidebarPanel(
          selectInput(
            "time3", "Temporal Scale:",
            c(
              "Annual" = "Annual",
              "Seasonal" = "Seasonal",
              "Monthly" = "Monthly"
            )
          ),

          selectInput("tscale", "Time Period:", ""),

          selectInput(
            "variable3", "Variable:",
            c(
              "Maximum Temperature" = "tmax",
              "Minimum Temperature" = "tmin",
              "Precipitation" = "ppt"
            )
          ),

          radioButtons(
            "stat3", "Statistic:",
            c(
              "Normal" = "Normal",
              "Standard Deviation" = "SD"
            )
          ),

          checkboxInput("dev3", "Show Deviation from Ensemble Mean", FALSE)
        ), # end of sidebarPanel

        mainPanel(plotOutput("mapPlot"))
      ) # end of sidebarLayout
    )
  )
))

#### read data and create necessary functions ####
base <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/figures"

get_cd_reg <- function(fnames, input_current) {
  fnames[fnames %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split(pattern = "_") %>%
    lapply(tail, 2) %>%
    lapply(head, 1) %>%
    magrittr::equals(input_current) %>%
    which(. == T)]
}

get_cd_map <- function(fnames, input_current) {
  fnames[fnames %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split(pattern = "_") %>%
    lapply(tail, 1) %>%
    magrittr::equals(input_current) %>%
    which(. == T)]
}


#### server function ####
server <- function(input, output, session) {


  #### Format data for box, density and correlation plots ####
  observeEvent(input$type, {
    if (input$type == "corr") {
      shinyjs::disable("dev")
    } else {
      shinyjs::enable("dev")
    }
  })

  output$boxPlot <- renderImage({
    if (input$dev) {
      dev_name <- "devT"
    } else {
      dev_name <- "devF"
    }

    if (input$type == "corr") {
      new_path <- paste(base, input$type, input$stat, input$time, sep = "/") %>%
        list.files(full.names = T, pattern = ".png") %>%
        grep(pattern = input$variable, ., value = T)
    } else {
      new_path <- paste(base, input$type, dev_name, input$stat, input$time, sep = "/") %>%
        list.files(full.names = T, pattern = ".png") %>%
        grep(pattern = input$variable, ., value = T)
    }

    return(list(
      src = new_path,
      contentType = "image/png",
      width = 800,
      height = 600
    ))
  }, deleteFile = FALSE)

  #### format data for elevation plots ####

  observe({
    if (input$type2 == "maps") {
      updateSelectInput(session, "variable2",
        choices = list(
          "Precipitation" = "ppt",
          "Temperature" = "temp"
        )
      )
    } else {
      updateSelectInput(session, "variable2",
        choices = list(
          "Maximum Temperature" = "tmax",
          "Minimum Temperature" = "tmin",
          "Precipitation" = "ppt"
        )
      )
    }
  })

  output$elevPlot <- renderImage({
    if (input$type2 == "maps") {
      new_path <- paste(base, "elev", "elev_map", input$variable2, sep = "/") %>%
        list.files(full.names = T, pattern = ".png") %>%
        grep(input$stat2, ., value = T) %>%
        get_cd_map(input$cd)


      wid <- 600
      hei <- 800
    } else {
      new_path <- paste(base, "elev", input$type2, input$stat2, sep = "/") %>%
        list.files(full.names = T, pattern = ".png") %>%
        grep(input$variable2, ., value = T) %>%
        get_cd_reg(input$cd)

      wid <- 800
      hei <- 600
    }

    return(list(
      src = new_path,
      contentType = "image/png",
      width = wid,
      height = hei
    ))
  }, deleteFile = FALSE)

  #### format data for map plots ####

  observe({
    if (input$time3 == "Annual") {
      updateSelectInput(session, "tscale",
        choices = list("Annual" = "01")
      )
    } else if (input$time3 == "Seasonal") {
      updateSelectInput(session, "tscale",
        choices = list(
          "Winter" = "01",
          "Spring" = "02",
          "Summer" = "03",
          "Autumn" = "04"
        )
      )
    } else if (input$time3 == "Monthly") {
      updateSelectInput(session, "tscale",
        choices = list(
          "January" = "01",
          "February" = "02",
          "March" = "03",
          "April" = "04",
          "May" = "05",
          "June" = "06",
          "July" = "07",
          "August" = "08",
          "September" = "09",
          "October" = "10",
          "November" = "11",
          "December" = "12"
        )
      )
    }
  })

  output$mapPlot <- renderImage({
    if (input$dev3) {
      dev_name <- "devT"
    } else {
      dev_name <- "devF"
    }

    return(list(
      src = paste(base, "maps", dev_name, input$stat3, input$time3, sep = "/") %>%
        list.files(full.names = T, pattern = ".png") %>%
        grep(input$variable3, ., value = T) %>%
        grep(input$tscale, ., value = T),
      contentType = "image/png",
      width = 800,
      height = 600
    ))
  }, deleteFile = FALSE)
}

# Run the application
shinyApp(ui, server)
