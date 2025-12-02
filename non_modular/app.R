library(tidyverse)
library(sf)
library(shiny)
library(shinydashboard)
library(waiter)
library(htmltools)
library(highcharter)
library(geojsonio)
library(bslib)
library(bsicons)

# ui <- 
page_fillable(
  h1("ECONOMIC VALUE CALCULATOR 2025"),
  
  page_navbar(
    value_box(
      title = "Total Social Return on Investment",
      value = "£3.8bn",
      showcase = bs_icon("bar-chart"),
      theme = "blue",
      max_height = "100px",
      fill = FALSE
    ),
    sidebar = sidebar(
                      
                    layout_column_wrap(
                        width = "50px",
                        fill = F,
                        value_box(
                          title = "Economic value",
                          value = "123bn",
                          showcase = bs_icon("bar-chart"),
                          theme = "purple"
                          )
                      ),
                    
                    layout_column_wrap(
                      width = "50px",
                      fill = F,
                      value_box(
                        title = "Reduced re-offending",
                        value = "123bn",
                        showcase = bs_icon("bar-chart"),
                        theme = "yellow"
                      )
                    ),
                    
                    
                    layout_column_wrap(
                      width = "50px",
                      fill = F,
                      value_box(
                        title = "DWP/Health admin",
                        value = "123bn",
                        showcase = bs_icon("bar-chart"),
                        theme = "red"
                      )
                    ),
                    layout_column_wrap(
                      width = "50px",
                      fill = F,
                      value_box(
                        title = "Volunteers",
                        value = "123bn",
                        showcase = bs_icon("bar-chart"),
                        theme = "orange"
                      )
                    ),
                    layout_column_wrap(
                      width = "50px",
                      fill = F,
                      value_box(
                        title = "Wellbeing",
                        value = "123bn",
                        showcase = bs_icon("bar-chart"),
                        theme = "grey"
                      )
                    ),
                    
                    
                    ),
    nav_panel("£ value", "page 1 content"),
    nav_panel("People", "Page 2 content"),
    nav_panel("Glossary", "Page 3 content"),
    nav_panel("Data and methods", "Page 4 content")
  )
)
  
  




# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
}


shinyApp(ui = ui, server = server)
