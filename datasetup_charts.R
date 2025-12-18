library(highcharter)
library(tidyverse)
library(readxl)


#number format function
# Create your function
custom_number_format <- function(x){ifelse(x > 999999999,
                                           paste0(format(round((x/1000000000), 2), 
                                                        nsmall=1, big.mark=","), "bn"),
                                           ifelse(x > 999999, 
                                                  paste0(format(round((x/1000000), 1), 
                                                               nsmall=1, big.mark=","),"m"), 
                                                  format(round(x), nsmall=0, big.mark=",")))}

# Now try it out
custom_number_format(999)
custom_number_format(999999)
custom_number_format(1000000)
custom_number_format(999900000)
custom_number_format(1000000000)

#####################
##################### 1.0 define colours
#####################

#colours
kt_colors <- c("#CC0033", #red
               "#1E1964", #blue
               "#28D796", #limegreen
               "#5009B0", #purple
               "#323232", #charcoal
               "#F2F2F2", #grey
               "#7a6da0", #mid-purple
               "#a69bbf", #light purple
               "#95e8bf", #mid green
               "#def6e9" #light green
               ) 

#####################
##################### 1.1 data read in and clean up
#####################

#read in the data and clean up a bit
df <- read_csv("db_data.csv") %>% 
  select(-`Salaries (-dw, attribution, and displacement) (£)`) %>% 
  rename("Economic value (GVA)" = `GVA (-dw, attribution, and displacement) (£)`,
         "Reduced re-offending" = `Savings from reduced reoffending (£)`,
         "DWP/health admin" = `Savings to DWP/Health (£)`,
         "Wellbeing" = `WELLBY (-dw and attribution) (£)`,
         "Total savings" = `Total savings (£)`
         ) %>% 
  mutate(
    "Volunteer value" = `Volunteers (-attribution) (£)` + `Volunteer value (mentor and non-mentor) (£)`,
    "Dummy" = NA
  ) %>% 
  select(-c(`Volunteers (-attribution) (£)`, `Volunteer value (mentor and non-mentor) (£)`)) %>% 
  relocate(`group_type`, .after = `group`) %>% 
  filter(`Cohort years` %in% c(#"2014/15", 
                               "2015/16", 
                               "2016/17", 
                               "2017/18", 
                               "2018/19", 
                               "2019/20", 
                               "2020/21", 
                               "2021/22", 
                               "2022/23", 
                               "2023/24", 
                               "2024/25")) %>% filter(!is.na(group))

#subset the total/all group for a constant series in the chart
df_all <- df %>% filter(group == "all") #%>% na.omit()

df_ten_yr <- df %>% group_by(group, group_type) %>%
  summarise_if(is.numeric, sum)
  

#set up a color lookup for the chart
kt_sroi_colors_df <- tibble(
  subgroup = colnames(df)[4:ncol(df)],
  subgroup.colour = kt_colors[1:(ncol(df)-3)]
)

  
#####################
##################### 2.0 time series chart
#####################

# # this sets up a object for the data labels
# # NO interactive, this is the constant background series
# cht_data <- df_all$`Economic value (GVA)`
# 
# # Create a list of points with dataLabels only on the last one, showing series.name
# cht_series <- lapply(seq_along(cht_data), function(i) {
#   if (i == length(cht_data)) {
#     list(
#       y = cht_data[i],
#       dataLabels = list(
#         enabled = TRUE,
#         align = "left",
#         y = 15,
#         crop = F,
#         overflow = "allow",
#         format = "{series.name}"
#       )
#     )
#   } else {
#     list(y = cht_data[i])
#   }
# })
# 
# 
# 
# # this sets up a object for the data labels for the second series - USER INTERACTIVE
# cht_data_sub <- df %>% filter(group == #input$group
#                                         "Male"
#                               ) %>%  ### substitute this for the selected interactive filter input
#   pull(`Economic value (GVA)`) 
# 
# # Create a list of points with dataLabels only on the last one, showing series.name
# cht_series_sub <- lapply(seq_along(cht_data_sub), function(i) {
#   if (i == length(cht_data)) {
#     list(
#       y = cht_data_sub[i],
#       dataLabels = list(
#         enabled = TRUE,
#         align = "left",
#         y = 15,
#         crop = F,
#         overflow = "allow",
#         format = "{series.name}"
#       )
#     )
#   } else {
#     list(y = cht_data_sub[i])
#   }
# })
# 
# 
# highchart1 <- highchart() %>% 
#   hc_chart(type = "column", spacingRight = 80) %>%
#   
#   hc_xAxis(categories = df_all$`Cohort years`, #substitute this for the selected interactive filter input
#            title = list(text = "")
#            
#   ) %>% 
#   
#   #bar total (CONSTANT)
#   hc_add_series(name="Total SROI",
#                 data = (df_all$`Total savings`),
#                 stack = "Main",
#                 pointPadding = 0,
#                 
#                 pointWidth = 25,
#                 pointPlacement = 0.4,
#                 groupPadding = 0,
#                 color = kt_colors[6], #light grey
#                 zIndex = 1) %>%
#   
#   #bar sub-group
#   hc_add_series(name="Total SROI - Male",
#                 data = df %>% filter(group == "Male"
#                                      #input$group
#                                      ) %>% 
#                   pull(`Economic value (GVA)`), #substitute this for the selected interactive filter input
#                 color = kt_colors[8], #purple
#                 borderWidth = 0,
#                 pointWidth = 23,
#                 position = list(offsetY = -25),
#                 stack = "Main",
#                 zIndex = 2,
#                 x = -25) %>%
#   
#   #line component value
#   hc_add_series(data = cht_series, 
#                 type = "line",
#                 name = "Economic value",
#                 marker = list(symbol = 'circle'),
#                 pointPlacement = "on",
#                 color = kt_colors[5],
#                 zIndex = 50,
#                 dataLabels = list(enabled = F)) %>%
#   
#   #line component value sub-group
#   hc_add_series(data = cht_series_sub, 
#                 type = "line",
#                 name = "Economic value <br> male",
#                 color = kt_colors[4],
#                 
#                 zIndex = 51,
#                 marker = list(symbol = 'circle'),
#                 dataLabels = list(enabled = F)) %>%
#   
#   hc_xAxis(title = list(text = ""))%>%
#   hc_yAxis(title = list(text = "£")
#            )%>%
#   #hc_size(width = 500) %>% 
#   hc_title(text = "", align = "left", 
#            
#            style = list(fontSize ="24px",#color = green.pair[1], 
#                         fontFamily = "Arial", fontWeight = "400" ))%>% 
#   hc_exporting(enabled = F) 
# 
# highchart1

#####################
##################### 3.0 cross sectional chart
#####################

#background series always present
df_total <- df %>% filter(`Cohort years` == "2024/25") %>% 
  select(-c(`Cohort count`,`Total savings`)) %>% 
  filter(group == "all"
  ) %>% 
  pivot_longer(
    cols = 4:ncol(.),
    values_to = "values",
    names_to = "names"
  ) %>% 
  arrange(desc(`values`))


# #set up the df to feed the chart. This will change depending on user inputs
# df_ts <- df %>% filter(`Cohort years` == "2024/25") %>% 
#   select(-c(`Cohort count`,`Total savings`)) %>% 
#   filter(group == #input$group # <<<< INTERACTIVE INPUT HERE
#            "Male" #remove this
#            ) %>% 
#   pivot_longer(
#     cols = 4:ncol(.),
#     values_to = "values",
#     names_to = "names"
#   ) %>% 
#   arrange(match(`names`, df_total$names)) #make sure sort order always the same as the main series

# #chart
# highchart2 <- highchart() %>% 
#   hc_chart(type = "column", spacingRight = 80) %>%
#   
#   hc_xAxis(categories = df_ts$names, #substitute this for the selected interactive filter input
#            title = list(text = "")
#            
#   ) %>% 
# 
#   hc_plotOptions(
#     column = list(
#       grouping = FALSE   # don’t put series side by side
#       #stacking = "normal" # use stack to align them
#     )
#   ) %>%  
#   
#   hc_xAxis(
#     categories = df_ts$names,
#     title = list(text = "")
#   ) %>% 
#   
#   # BACK BAR (TOTAL)
#   hc_add_series(
#     name = "All",
#     data = df_total$values ,
#     type = "column",
#     stack = "Main",
#     
#     # Shared width logic
#     pointPadding = 0,
#     groupPadding = 0.2,
#     maxPointWidth = 120,
#     pointPlacement = 0,
#     
#     borderWidth = 0,
#     color = kt_colors[6],
#     zIndex = 1
#   ) %>%
#   
#   # FRONT BAR (SUB-GROUP)
#   hc_add_series(
#     name = df_total$names[df_total$names == input$filter2], #"All", ##<<<< interactive value 
#     data = df_total$values[df_total$names == input$filter2], ##<<<< interactive value 
#     type = "column",
#     stack = "Main",
#     
#     # Must match the total series here
#     pointPadding = 0,
#     groupPadding = 0.2,
#     maxPointWidth = 120,
#     pointPlacement = 0,
#     
#     borderWidth = 0,
#     color = kt_colors[1],
#     zIndex = 2
#   ) %>%
#   
#   hc_yAxis(title = list(text = "£")) %>%
#   hc_exporting(enabled = FALSE)
# 
#   
# 
# highchart2
