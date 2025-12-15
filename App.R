library(tidyverse)
library(sf)
library(shiny)
# library(shinydashboard)
library(waiter)
library(htmltools)
library(highcharter)
library(geojsonio)
library(bslib)
library(bsicons)
library(sass)


#read in data, colours and highchart chart

source("datasetup_charts.r")

my_theme <- bs_theme(version = 5) |>
  bs_add_rules(
    sass::sass(
      input = sass::sass_file("www/styles.scss")
    )
  )

########################################################################
########################################################################
#####################         UI         ###############################
########################################################################
########################################################################

ui <- 
  
  page_fillable(
    theme = my_theme,
    
    tags$h1("ECONOMIC VALUE CALCULATOR 2025", id = "main-title"),
    #intro box
    # Intro row
    fluidRow(
      column(
        width = 12,
        bslib::card(
          id = "intro_box",
          uiOutput("intro_box_copy")
        )
      )
    ),

    page_navbar(
      title = "",   # HEADER
      
      # ---- TAB summary ----
      nav_panel(
        "Summary",
        
        #removed as we don't need a sidebar for the summary
                # layout_sidebar(
        #   sidebar = sidebar(
        #     value_box("SMetric A", value = "321", height = "10em",),
        #     value_box("SMetric B", value = "654", height = "10em",),
        #     value_box("SMetric C", value = "987", height = "10em",),
        #     value_box("SMetric D", value = "202", height = "10em",),
        #     value_box("SMetric E", value = "303", height = "10em",)
        #   ),
        
          layout_column_wrap(
            width = 1/1,
            max_height = 350,
            min_height = 350,
            # --- FILTER ROW ---
            card(
              class = "filter-card",
              #style = "padding: 0;",
              
              card_body(
                
                layout_column_wrap(
                  width = 1/2,
                  selectizeInput("filter1", "Parent", choices = unique(df$group_type)),
                  selectizeInput("filter2", "Subgroup", choices = NULL) #this needs to reference the selector above!
                )
              )
            ),
            value_box(title = "Summary total",
                      value = "120,000", #pipe this in from the data
                      showcase = bs_icon("bar-chart"),
                      theme = "red", 
                      fill = TRUE),
            
            
            card(fill = FALSE,
                 #card_header("Title"),
                 highchartOutput("highchart_plot1")
            )
          )
        #)
      ),
      
      # ---- TAB £ VALUE ----
      nav_panel(
        "£ value",
        layout_sidebar(
          sidebar = sidebar(
            value_box(title = "Economic value", value = custom_number_format(df_ten_yr$`Economic value (GVA)`[df_ten_yr$group == "all"]), #pipe this in from the data
                      showcase = bs_icon("bar-chart"), height = "10em",
                      theme = "purple"),
            value_box(title = "Reduced re-offending", value = custom_number_format(df_ten_yr$`Reduced re-offending`[df_ten_yr$group == "all"]),#pipe this in from the data
                      showcase = bs_icon("bar-chart"), height = "10em",
                      theme = "yellow"),
            value_box(title = "DWP/Health admin", value = custom_number_format(df_ten_yr$`DWP/health admin`[df_ten_yr$group == "all"]),#pipe this in from the data
                      showcase = bs_icon("bar-chart"), height = "10em",
                      theme = "red"),
            value_box(title = "Volunteers", value = custom_number_format(df_ten_yr$`Volunteer value`[df_ten_yr$group == "all"]),#pipe this in from the data
                      showcase = bs_icon("bar-chart"), height = "10em",
                      theme = "orange"),
            value_box(title = "Wellbeing", value = custom_number_format(df_ten_yr$Wellbeing[df_ten_yr$group == "all"]),#pipe this in from the data
                      showcase = bs_icon("bar-chart"),, height = "10em",theme = "grey")
          ),
          # Main body
          layout_column_wrap(
            width = 1/1,
            max_height = 350,
            min_height = 350,
            # --- FILTER ROW ---
            card(
              class = "filter-card",
              card_body(
                style = "height: 90px;",
                layout_column_wrap(
                  width = 1/2,
                  selectizeInput("filter3", "Subgroup parent", choices = unique(df$group_type)),
                  selectizeInput("filter4", "Subgroup", choices = NULL) 
                )
              )
            ),
            
            value_box(title = "Total Social Return on Investment",value = "£3.8bn", showcase = bs_icon("bar-chart"),theme = "blue", fill = TRUE),
            
            card(fill = FALSE,
                 #card_header("Title"),
                 highchartOutput("highchart_plot2")
            )
          )
        )
      ),
      
      # # ---- TAB PEOPLE ----
      # nav_panel(
      #   "People",
      #   layout_sidebar(
      #     sidebar = sidebar(
      #       value_box("Metric A", value = "321"),
      #       value_box("Metric B", value = "654"),
      #       value_box("Metric C", value = "987"),
      #       value_box("Metric D", value = "202"),
      #       value_box("Metric E", value = "303")
      #     ),
      #     layout_column_wrap(
      #       width = 1/1,
      #       
      #       value_box(title = "People total",value = "120,000", showcase = bs_icon("bar-chart"),theme = "blue", fill = TRUE),
      #                 
      #     card(fill = FALSE,
      #       card_header("Chart"),
      #       plotOutput("plot2")
      #       )
      #     )
      #   )
      # ),
      
      # ---- TAB Glossary ----
      nav_panel(
        "Glossary",
        card(
          card_header("Glossary"),
          card_body("Glossary content goes here")
        )
      ),
      
      # ---- TAB Data Sources ----
      nav_panel(
        "Data Sources",
        card(
          card_header("Data Sources"),
          card_body("List of sources, links, etc.")
        )
      )
    ))

########################################################################
########################################################################
#####################         SERVER     ###############################
########################################################################
########################################################################

server <- function(input, output, session) {
  
  
  #################
  ################# filter stuff tab1
  #################
  
  # Shared filter state across tabs
  state <- reactiveValues(
    parent = NULL,
    subgroup = NULL
  )
  
  # Tab 1: when filter1 changes
  observeEvent(input$filter1, {
    state$parent <- input$filter1
    
    if (is.null(input$filter1) || input$filter1 == "") {
      updateSelectizeInput(session, "filter2", choices = NULL)
      state$subgroup <- NULL
      return()
    }
    
    subset_choices <- unique(df$group[df$group_type == input$filter1])
    
    updateSelectizeInput(
      session, "filter2",
      choices  = subset_choices,
      selected = subset_choices[1],
      server   = TRUE
    )
    
    state$subgroup <- subset_choices[1]
  })
  
  # Tab 1: when filter2 changes
  observeEvent(input$filter2, {
    state$subgroup <- input$filter2
  })
  
  
  #################
  ################# filter stuff tab2
  #################
  
  # Shared filter state across tabs
  state2 <- reactiveValues(
    parent = NULL,
    subgroup = NULL
  )
  
  # Tab 1: when filter1 changes
  observeEvent(input$filter3, {
    state2$parent <- input$filter3
    
    if (is.null(input$filter3) || input$filter3 == "") {
      updateSelectizeInput(session, "filter4", choices = NULL)
      state2$subgroup <- NULL
      return()
    }
    
    subset_choices2 <- unique(df$group[df$group_type == input$filter3])
    
    updateSelectizeInput(
      session, "filter4",
      choices  = subset_choices2,
      selected = subset_choices2[1],
      server   = TRUE
    )
    
    state2$subgroup <- subset_choices2[1]
  })
  
  # Tab 1: when filter2 changes
  observeEvent(input$filter4, {
    state2$subgroup <- input$filter4
  })
  
  #the copy for the umm intro box!
  output$intro_box_copy <- renderUI({
    HTML("<span class='intro_copy'>Intro text copy to go here, box to be styled etc.")
  })
  
  

  
  ###############
  ############### reactive data for tab 1 chart
  ###############
  
  data_highchart1 <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df %>% filter(`Cohort years` == "2024/25") %>% 
      select(-c(`Cohort count`,`Total savings`)) %>% 
      filter(group == input$filter2, # <<<< INTERACTIVE INPUT HERE
             
      ) %>% 
      pivot_longer(
        cols = 4:ncol(.),
        values_to = "values",
        names_to = "names"
      ) %>% 
      arrange(match(`names`, df_total$names)) 
  })
  
  
  ###############
  ############### Tab1 XS chart
  ###############
  
  output$highchart_plot1 <- 
    
    renderHighchart(expr = {#chart
      highchart1 <- highchart() %>% 
        hc_chart(type = "column", spacingRight = 80) %>%
        
        hc_xAxis(categories = df_total$names, #substitute this for the selected interactive filter input
                 title = list(text = "")
                 
        ) %>% 
        
        hc_plotOptions(
          column = list(
            grouping = FALSE   # don’t put series side by side
            #stacking = "normal" # use stack to align them
          )
        ) %>%  

        
        # BACK BAR (TOTAL)
        hc_add_series(
          name = "All",
          data = df_total$values ,
          type = "column",
          stack = "Main",
          
          # Shared width logic
          pointPadding = 0,
          groupPadding = 0.2,
          maxPointWidth = 120,
          pointPlacement = 0,
          
          borderWidth = 0,
          color = kt_colors[6],
          zIndex = 1
          #showInLegend = FALSE
        ) %>%
        
        # FRONT BAR (SUB-GROUP)
        hc_add_series(
          name = unique(data_highchart1()$group), #"All", ##<<<< interactive value 
          data = data_highchart1()$values, ##<<<< interactive value 
          type = "column",
          stack = "Main",
          
          # Must match the total series here
          pointPadding = 0,
          groupPadding = 0.2,
          maxPointWidth = 120,
          pointPlacement = 0,
          
          borderWidth = 0,
          color = kt_colors[1],
          zIndex = 2
        ) %>%
        
        hc_yAxis(title = list(text = "£")) %>%
        hc_exporting(enabled = FALSE)
      
      highchart1
      
    } 
    )
  
  ###############
  ############### reactive data for tab 2 chart
  ###############
  
  data_highchart2 <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df %>% 
      filter(group == input$filter4) # <<<< INTERACTIVE INPUT HERE
    
  })
  
  ###############
  ############### Tab2 TS chart
  ###############
  
  output$highchart_plot2 <- 
    
    renderHighchart(expr = {
      
      # this sets up a object for the data labels
      # NOT interactive, this is the constant background series
      cht_data <- df_all$`Economic value (GVA)`
      
      # Create a list of points with dataLabels only on the last one, showing series.name
      cht_series <- lapply(seq_along(cht_data), function(i) {
        if (i == length(cht_data)) {
          list(
            y = cht_data[i],
            dataLabels = list(
              enabled = TRUE,
              align = "left",
              y = 15,
              crop = F,
              overflow = "allow",
              format = "{series.name}"
            )
          )
        } else {
          list(y = cht_data[i])
        }
      })
      
      
      
      # this sets up a object for the data labels for the second series - USER INTERACTIVE
      cht_data_sub <- data_highchart2() %>% pull(`Economic value (GVA)`)
      
      # Create a list of points with dataLabels only on the last one, showing series.name
      cht_series_sub <- lapply(seq_along(cht_data_sub), function(i) {
        if (i == length(cht_data)) {
          list(
            y = cht_data_sub[i],
            dataLabels = list(
              enabled = TRUE,
              align = "left",
              y = 15,
              crop = F,
              overflow = "allow",
              format = "{series.name}"
            )
          )
        } else {
          list(y = cht_data_sub[i])
        }
      })
      
      
      highchart2 <- highchart() %>% 
        hc_chart(type = "column", spacingRight = 80) %>%
        
        hc_xAxis(categories = df_all$`Cohort years`, #substitute this for the selected interactive filter input
                 title = list(text = "")
                 
        ) %>% 
        
        #bar total (CONSTANT)
        hc_add_series(name= "Total SROI",
                      data = (df_all$`Total savings`),
                      stack = "Main",
                      pointPadding = 0,
                      
                      pointWidth = 25,
                      pointPlacement = 0.4,
                      groupPadding = 0,
                      color = kt_colors[6], #light grey
                      zIndex = 1) %>%
        
        #bar sub-group
        hc_add_series(name= unique(data_highchart2()$group),
                      data = data_highchart2()$`Total savings`, #make this interactive from the side boxes
                      color = kt_colors[8], #purple
                      borderWidth = 0,
                      pointWidth = 23,
                      position = list(offsetY = -25),
                      stack = "Main",
                      zIndex = 2,
                      x = -25) %>%
        
        #line component value
        hc_add_series(data = cht_series, #make this interactive from the side boxes
                      type = "line",
                      name = "Economic value (GVA)", #make this interactive from the side boxes
                      marker = list(symbol = 'circle'),
                      pointPlacement = "on",
                      color = kt_colors[5],
                      zIndex = 50,
                      dataLabels = list(enabled = F)) %>%
        
        #line component value sub-group
        hc_add_series(data = cht_series_sub, #make this interactive from the side boxes
                      type = "line",
                      name = unique(data_highchart2()$group), #make this interactive from the side boxes
                      color = kt_colors[4],

                      zIndex = 51,
                      marker = list(symbol = 'circle'),
                      dataLabels = list(enabled = F)) %>%
        
        hc_xAxis(title = list(text = ""))%>%
        hc_yAxis(title = list(text = "£")
        ) %>%
        #hc_size(width = 500) %>% 
        hc_title(text = "", align = "left", 
                 
                 style = list(fontSize ="24px",#color = green.pair[1], 
                              fontFamily = "Arial", fontWeight = "400" )) %>% 
        hc_exporting(enabled = F) 
      
      highchart2
      
    } 
    )
  
  
}

shinyApp(ui, server)
