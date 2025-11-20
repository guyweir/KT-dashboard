library(highcharter)
library(tidyverse)
library(readxl)

#colours
kt_colors <- c("#CC0033", #red
               "#1E1964", #blue
               "#28D796", #limegreen
               "#5009B0", #purple
               "#323232", #charcoal
               "#F2F2F2",
               "#7a6da0",
               "#a69bbf") 


#read in test data (total)
df <- read_excel("Total savings year.xlsx") 
df[2:9] <- lapply(df[2:9], function(x){str_replace_all(x, "£", "")})
df[2:9] <- lapply(df[2:9], function(x){str_replace_all(x, ",", "")})
df[2:9] <- lapply(df[2:9], as.numeric) 
df <- df %>% na.omit() %>% 
mutate("Subgroup" = "Total")

#read in test data (by sex)
df2 <- read_excel("Savings by year and sex.xlsx") 
df2[3:11] <- lapply(df2[3:11], function(x){str_replace_all(x, "£", "")})
df2[3:11] <- lapply(df2[3:11], function(x){str_replace_all(x, ",", "")})
df2[3:11] <- lapply(df2[3:11], as.numeric) 
df2 <- df2 %>% na.omit() %>% 
  rename("Subgroup" = "gender")


  

#chart
# this sets up a object for the data labels
cht_data <- df$`GVA (-dw, attribution, and displacement) (£)`

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


#chart
# this sets up a object for the data labels for the second series
cht_data_sub <- df2$`GVA (-dw, attribution, and displacement) (£)`[df2$Subgroup == "Male"]

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


plot <- highchart() %>% 
  hc_chart(type = "column", spacingRight = 80) %>%
  
  hc_xAxis(categories = df$`Cohort years`,
           title = list(text = "")
           
  ) %>% 
  
  #bar total
  hc_add_series(name="Total SROI",
                data = (df$`Total savings (£)`),
                stack = "Main",
                pointPadding = 0,
                pointWidth = 25,
                pointPlacement = 0.4,
                groupPadding = 0,
                color = kt_colors[6], #light grey
                zIndex = 1) %>%
  
  #bar sub-group
  hc_add_series(name="Total SROI - Male",
                data = (df2$`Total savings (£)`[df2$Subgroup == "Male"]),
                color = kt_colors[8], #purple
                borderWidth = 0,
                pointWidth = 23,
                
                pointPlacement = "on",
                position = list(offsetY = -25),
                
                stack = "Main",
                zIndex = 2,
                x = -25) %>%
  
  #line component value
  hc_add_series(data = cht_series, 
                type = "line",
                name = "Economic value",
                marker = list(symbol = 'circle'),
                color = kt_colors[5],
                zIndex = 50,
                dataLabels = list(enabled = F)) %>%
  
  #line component value sub-group
  hc_add_series(data = cht_series_sub, 
                type = "line",
                name = "Economic value <br> male",
                color = kt_colors[4],
                
                zIndex = 51,
                marker = list(symbol = 'circle'),
                dataLabels = list(enabled = F)) %>%
  
  hc_xAxis(title = list(text = ""))%>%
  hc_yAxis(title = list(text = "£")
           )%>%
  #hc_size(width = 500) %>% 
  hc_title(text = "", align = "left", 
           
           style = list(fontSize ="24px",#color = green.pair[1], 
                        fontFamily = "Arial", fontWeight = "400" ))%>% 
  hc_exporting(enabled = F) 

plot

###################
#second version for `counts` TOTALLY FICTIONAL DATA!
###################


df_count <- df
df2_count <- df2

#chart
# this sets up a object for the data labels
cht_data_ct <- df_count$`GVA (-dw, attribution, and displacement) (£)`/3000

# Create a list of points with dataLabels only on the last one, showing series.name
cht_series_ct <- lapply(seq_along(cht_data_ct), function(i) {
  if (i == length(cht_data_ct)) {
    list(
      y = cht_data_ct[i],
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
    list(y = cht_data_ct[i])
  }
})


#chart
# this sets up a object for the data labels for the second series
cht_data_sub_ct <- df2$`Cohort count`[df2$Subgroup == "Male"]

# Create a list of points with dataLabels only on the last one, showing series.name
cht_series_sub_ct <- lapply(seq_along(cht_data_sub_ct), function(i) {
  if (i == length(cht_data)) {
    list(
      y = cht_data_sub_ct[i],
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
    list(y = cht_data_sub_ct[i])
  }
})

plot2 <- highchart() %>% 
  hc_chart(type = "column", spacingRight = 80) %>%
  
  hc_xAxis(categories = df_count$`Cohort years`,
           title = list(text = "")
           
  ) %>% 
  
  #bar total
  hc_add_series(name="Total SROI",
                data = (df_count$`Total savings (£)`/3000),
                stack = "Main",
                pointPadding = 0,
                pointWidth = 25,
                pointPlacement = 0.4,
                groupPadding = 0,
                color = kt_colors[6],
                zIndex = 1) %>%
  
  #bar sub-group
  hc_add_series(name="Total SROI - Male",
                data = (df2_count$`Cohort count`[df2_count$Subgroup == "Male"]*1.5),
                color = kt_colors[3],
                borderWidth = 0,
                pointWidth = 23,
                
                pointPlacement = "on",
                position = list(offsetY = -25),
                
                stack = "Main",
                zIndex = 2,
                x = -25) %>%
  
  #line component value
  hc_add_series(data = cht_series_ct, 
                type = "line",
                name = "Economic value",
                marker = list(symbol = 'circle'),
                color = kt_colors[2],
                zIndex = 50,
                dataLabels = list(enabled = F)) %>%
  
  #line component value sub-group
  hc_add_series(data = cht_series_sub_ct, 
                type = "line",
                name = "Economic value <br> male",
                color = kt_colors[2],
                opacity = 0.6,
                zIndex = 51,
                marker = list(symbol = 'circle'),
                dataLabels = list(enabled = F)) %>%
  
  hc_xAxis(title = list(text = ""))%>%
  hc_yAxis(title = list(text = "Count")
  )%>%
  #hc_size(width = 500) %>% 
  hc_title(text = "", align = "left", 
           
           style = list(fontSize ="24px",#color = green.pair[1], 
                        fontFamily = "Arial", fontWeight = "400" ))%>% 
  hc_exporting(enabled = F) 

plot2
