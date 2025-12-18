library(tidyverse)
library(sf)
library(shiny)
library(waiter)
library(htmltools)
library(highcharter)
library(geojsonio)
library(bslib)
library(bsicons)
library(sass)


#read in data, colours and highchart chart

source("datasetup_charts.r")

my_theme <- bs_theme(version = 5) %>% 
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
   
    
    div(class = "container-md",
    tags$h1("ECONOMIC VALUE CALCULATOR 2025", id = "main-title"),
    
    tags$script(HTML(" Shiny.addCustomMessageHandler('addSelectedClass', function(id) {
                     $('#' + id).addClass('selected'); });
                     Shiny.addCustomMessageHandler('removeSelectedClass', function(id) { $('#' + id).removeClass('selected'); });
                     ")),
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
                  selectizeInput("filter1", "Parent", choices = unique(df$group_type), options = list( persist = FALSE, create = FALSE )),
                  selectizeInput("filter2", "Subgroup", choices = NULL,options = list( persist = FALSE, create = FALSE )) #this needs to reference the selector above!
                )
              )
            ),
            
            
            value_box(title = "10 year total SROI",
                      value = custom_number_format(df_ten_yr$`Total savings`[df_ten_yr$group == "all"]), #pipe this in from the data
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
            
            tags$div(
              id = "select_econ_value_box",
              class = "value-box-button",
              onclick = "Shiny.setInputValue('select_econ_value', Math.random())",
              value_box(
                title = "Economic value",
                value = custom_number_format(df_ten_yr$`Economic value (GVA)`[df_ten_yr$group == "all"]),
                height = "6em",
                theme = "purple"
              )
            ),
            
            
            tags$div(
              id = "select_off_value_box",
              class = "value-box-button",
              onclick = "Shiny.setInputValue('select_off_value', Math.random())",
              value_box(
                title = "Re-offender value",
                value = custom_number_format(df_ten_yr$`Reduced re-offending`[df_ten_yr$group == "all"]),
                height = "6em",
                theme = "yellow"
              )
            ),
            
            tags$div(
              id = "select_dwp_value_box",
              class = "value-box-button",
              onclick = "Shiny.setInputValue('select_dwp_value', Math.random())",
              value_box(
                title = "DWP/health value",
                value = custom_number_format(df_ten_yr$`DWP/health admin`[df_ten_yr$group == "all"]),
                height = "6em",
                theme = "red"
              )
            ) ,
                
                
            tags$div(
              id = "select_vol_value_box",
              class = "value-box-button",
              onclick = "Shiny.setInputValue('select_vol_value', Math.random())",
              value_box(
                title = "Volunteer value",
                value = custom_number_format(df_ten_yr$`Volunteer value`[df_ten_yr$group == "all"]),
                height = "6em",
                theme = "orange"
              )
            ) ,
            
            
            
            tags$div(
              id = "select_well_value_box",
              class = "value-box-button",
              onclick = "Shiny.setInputValue('select_well_value', Math.random())",
              value_box(
                title = "Volunteer value",
                value = custom_number_format(df_ten_yr$Wellbeing[df_ten_yr$group == "all"]),
                height = "6em",
                theme = "white"
              )
            ) 
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
                  selectizeInput("filter3", "Subgroup parent", choices = unique(df$group_type),options = list( persist = FALSE, create = FALSE )),
                  selectizeInput("filter4", "Subgroup", choices = NULL,options = list( persist = FALSE, create = FALSE )) 
                )
              )
            ),
            
            value_box(title = "Total Social Return on Investment",
                      value = custom_number_format(df_ten_yr$`Total savings`[df_ten_yr$group == "all"]),
                      showcase = bs_icon("bar-chart"),theme = "blue", fill = TRUE),
            
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
)
    

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
  ############### clickable cards as selectors
  ###############
  
  #  1 Define the mapping (static, non-reactive)
  metric_map <- c(
    select_econ_value = colnames(df_ten_yr)[4], #the indexed colnames in case they change
    select_off_value  = colnames(df_ten_yr)[5],
    select_dwp_value  = colnames(df_ten_yr)[6],
    select_well_value = colnames(df_ten_yr)[7],
    select_vol_value  = colnames(df_ten_yr)[9]
  )
  
  # 2 Define the reactive state
  selected_metric <- reactiveVal("Dummy")  # default
  selected_metric_id <- reactiveVal(NULL)
  # 3 Observe all value-box buttons
  # observeEvent(
  #   {
  #     input$select_econ_value
  #     input$select_off_value
  #     input$select_dwp_value
  #     input$select_well_value
  #     input$select_vol_value
  #   },
  #   {
  #     clicked_id <- getDefaultReactiveDomain()$input$.lastInput
  #     selected_metric(metric_map[[clicked_id]])
  #   }
  # )
  
  observeEvent(input$select_econ_value, {
    selected_metric(metric_map[["select_econ_value"]])
    selected_metric_id("select_econ_value")
  })
  
  observeEvent(input$select_off_value, {
    selected_metric(metric_map[["select_off_value"]])
    selected_metric_id("select_off_value")
  })
  
  observeEvent(input$select_dwp_value, {
    selected_metric(metric_map[["select_dwp_value"]])
    selected_metric_id("select_dwp_value")
  })
  
  observeEvent(input$select_well_value, {
    selected_metric(metric_map[["select_well_value"]])
    selected_metric_id("select_well_value")
  })
  
  observeEvent(input$select_vol_value, {
    selected_metric(metric_map[["select_vol_value"]])
    selected_metric_id("select_vol_value")
  })
  
  
  # different css for clicked and not clicked sidebar
  observe({
    # remove selected class from all
    lapply(c(
      "select_econ_value_box",
      "select_off_value_box",
      "select_dwp_value_box",
      "select_well_value_box",
      "select_vol_value_box"
    ), function(id) {
      session$sendCustomMessage("removeSelectedClass", id)
    })
    
    # add selected class to the active one 
    if (!is.null(selected_metric_id())) {
      active_id <- paste0(selected_metric_id(), "_box") 
      session$sendCustomMessage("addSelectedClass", active_id) }
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
  
  data_highchart_aspect_sub <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df %>% 
      filter(group == input$filter4) %>%  # <<<< INTERACTIVE INPUT HERE
    select(c(`Cohort years`, group, group_type, `Cohort count`, selected_column = all_of(selected_metric())))
  })
  
  data_highchart_total_sub <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df %>% 
      filter(group == input$filter4) %>%  # <<<< INTERACTIVE INPUT HERE
      select(c(`Cohort years`, group, group_type, `Cohort count`, `Total savings`))
  })
  
  
  data_highchart_aspect_all <- reactive({
    #set up the df to feed the chart. This will change depending on user inputs
    df_all %>% 
      select(c(`Cohort years`, group, group_type, `Cohort count`, 
               selected_column = all_of(selected_metric())))
  })
  
  ###############
  ############### Tab2 TS chart
  ###############
  
  output$highchart_plot2 <- 
    
    renderHighchart(expr = {
      
      # this sets up a object for the data labels
      # Interactive with the aspect/element of SROI only - always shows the all/total no subgroups
      #cht_data <- df_all$`Economic value (GVA)`
      cht_data <- data_highchart_aspect_all() %>% pull(selected_column)
      
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
      cht_data_sub <- data_highchart_aspect_sub() %>% pull(selected_column)
      
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
                      # Shared width logic
                      pointPadding = 0,
                      groupPadding = 0.2,
                      maxPointWidth = 120,
                      pointPlacement = 0,
                      
                      color = kt_colors[6], #light grey
                      zIndex = 1) %>%
        

        hc_xAxis(title = list(text = ""))%>%
        hc_yAxis(title = list(text = "£")
        ) %>%
        #hc_size(width = 500) %>% 
        hc_title(text = "", align = "left", 
                 
                 style = list(fontSize ="24px",#color = green.pair[1], 
                              fontFamily = "Arial", fontWeight = "400" )) %>% 
        hc_exporting(enabled = F) %>% 
          
        #line component value
          hc_add_series(data = cht_series, #make this interactive from the side boxes
                        type = "line",
                        name = ifelse(selected_metric() == "Dummy","",  paste0(selected_metric(), ":<br>All ")), #how to get this out of metric_map??
                        marker = list(symbol = 'circle'),
                        pointPlacement = "on",
                        color = kt_colors[5],
                        zIndex = 50,
                        dataLabels = list(enabled = F))
      
      #condition so that sub groups don't render until a sub group selected
      if (input$filter4 != "all") {
      highchart2 <- highchart2 %>% 
        
        #bar sub-group
        hc_add_series(name= paste0("Total SROI: ", unique(data_highchart_total_sub()$group)),
                      data = data_highchart_total_sub()$`Total savings`, #make this interactive from the side boxes
                      color = kt_colors[8], #purple
                      # Shared width logic
                      pointPadding = 0,
                      groupPadding = 0.2,
                      maxPointWidth = 120,
                      pointPlacement = 0,
                      #position = list(offsetY = -25),
                      stack = "Main",
                      zIndex = 2
                      ) %>%

        
        #line component value sub-group
        hc_add_series(data = cht_series_sub, #make this interactive from the side boxes
                      type = "line",
                      name = ifelse(str_detect(selected_metric(),"Dummy"), 
                                    "",  
                                    paste0(selected_metric(),":<br>", unique(data_highchart_aspect_sub()$group))), #make this interactive from the side boxes
                      color = kt_colors[4],
                      
                      zIndex = 51,
                      marker = list(symbol = 'circle'),
                      dataLabels = list(enabled = F))}
      

      
      highchart2
    } 
    )
  
  
}

shinyApp(ui, server)
