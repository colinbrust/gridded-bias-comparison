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
library(mcor)
source("Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/R/viz_map.R")
source("Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/R/titles_map.R")
source("Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/R/save_maps.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Compaison of Gridded and Mesonet Data from 2017-01-01 to 2018-01-01"),

  tabsetPanel(

    #### Mesonet Comparison ####
    tabPanel("Maps",

             # Sidebar with a slider input for number of bins
             sidebarLayout(

               sidebarPanel(

                 selectInput("time", "Temporal Scale:",
                             c("Annual" = "Annual",
                               "Seasonal" = "Seasonal",
                               "Monthly" = "Monthly")),

                 selectInput("variable", "Variable:",
                             c("Maximum Temperature" = "tmax",
                               "Minimum Temperature" = "tmin",
                               "Precipitation" = "ppt")),

                 selectInput("stat", "Statistic:",
                              c("Normal" = "Normal",
                                "Standard Deviation" = "SD")),

                 checkboxInput("dev", "Show Deviation from Ensemble Mean", FALSE)
               ), # end sidebarPanel

               mainPanel(
                 plotly::plotlyOutput("mapPlot")
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
dat <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/app_data" %>%
  list.files(full.names = T)

#### server function ####
server <- function(input, output) {

  output$mapPlot <- plotly::renderPlotly({

    if (input$dev) deviation = "dif"
    else deviation = "val"

    dat2 <- dat %>%
      grep(".shp", ., value = T) %>%
      grep(deviation, ., value = T) %>%
      grep(input$time, ., value = T) %>%
      grep(input$variable, ., value = T) %>%
      grep(input$stat, ., value = T) %>%
      sf::read_sf()

    ggplot() +
      geom_sf(data = dat2, aes(fill = EnsDiff), color = NA) +
      geom_sf(data = mt_counties_simple, fill = NA, color = "gray40", size = 0.5,
              alpha = 0.1) +
      geom_sf(data = mt_state_simple, fill = NA, color = "gray40", size = 1) +
      labs(title = titles_map(input$variable, input$time, input$stat, "01", input$dev)) +
      mdt_theme_map() +
      pal(input$dev, input$variable) +
      facet_wrap( ~ Dataset)

  })

}

# Run the application
shinyApp(ui, server)
