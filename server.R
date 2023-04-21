source("weather climate R code.R")
library(shiny)
library(shinythemes)
library(rgl)
library(DT)
library(dplyr)
library(leaflet)
library(gt)
library(scales)
library(hash)
library(plotly)

options(warn=-1)

weather_files_path = "data/weather data"
air_files_path = "data/air quality data"

weather_plots_path = "plots/weather plots"
daytime_plots_path = "plots/daytime plots"
weather_condition_plots_path = "plots/weather condition plots"
air_quality_plots_path = "plots/air quality plots"

weather_files = list.files(weather_files_path,pattern = "\\.csv$")
air_files = list.files(air_files_path,pattern = "\\.csv$")

function_1 = function(x) {
  return (str_to_title(gsub(" *[0-9].*$", "", x)))
}
function_2 = function(x) {
  return (str_to_title(gsub("-.*$", "", x)))
}
weather_cities = unlist(lapply(weather_files, function_1))
air_cities = unlist(lapply(air_files, function_2))

weather_cities_dict = setNames(weather_files, weather_cities)
air_cities_dict = setNames(air_files, air_cities)

df = read.csv("data/City Climate Data and Koppen Climate Classification.csv")
df <- df[order(df$Koppen.climate), ]
climate_list = df$Climate.Names
climate_name_dict <- setNames(as.list(df$Climate.Names), paste(df$Climate.Names, ", ", df$Koppen.climate))
df$lat <- ifelse(df$latitude..degree. > 0, 
                 paste(as.character(df$latitude..degree.), "\u00B0","N"), 
                 paste(as.character(-df$latitude..degree.), "\u00B0","S"))

df$long <- ifelse(df$longitude..degree. > 0, 
                  paste(as.character(df$longitude..degree.), "\u00B0","E"), 
                  paste(as.character(-df$longitude..degree.), "\u00B0","W"))
color_mapping = hash(as.list(df$Climate.Names),as.list(df$Color.Code))
viz_cols = c(colnames(df)[6:13],colnames(df)[17:25])
viz_cols_names = c("Latitude (\u00B0)" ,             "Longitude (\u00B0)"    ,        
                   "Elevation (m)"      ,            "Distance to the Sea (km)"      ,        
                   "Annual Mean Temperature (\u00B0C)", "Annual Mean Solar Radiation (W/m2)" ,   
                   "Mean Humidity (\u0025)"     ,         "Annual Mean Windspeed (kph)"       ,   
                   "Annual Temperature Standard Deviation (\u00B0C)", "Annual Mean Daily Temperature Range (\u00B0C)"  ,   
                   "Annual Mean Temperature Difference (\u00B0C)" ,"Annual Precipitation Standard Deviation (mm)"            ,   
                   "Annual Precipitation Sum (mm)", "Temperature Precipitation Correlation Coefficient"  ,    
                   "Humidity Solar Radiation Correlation Coefficient" ,  "Humidity Precipitation Correlation Coefficient"         ,    
                   "Temperature Solar Radiation Correlation Coefficient" )
viz_cols_mapping = hash(as.list(viz_cols_names),as.list(viz_cols))


server <- function(input, output) {
  output$svg_1_1 <- renderUI({
    if (input$city_1 != ""){
      tryCatch({
        # plot_weather_precipitation_new(input$weather_city,input$unit)
        svg_contents <- readLines(paste(weather_plots_path,paste(input$city_1," Weather Precipitation ", input$unit_compare," 2022.svg",sep=""),sep="/"), encoding = "UTF-8")
        svg_html <- HTML(paste(svg_contents, collapse = "\n"))
        div(svg_html, class = "svg-container")}, error = function(e){
          plot_weather_precipitation_new(input$city_1,input$unit_compare)
          svg_contents <- readLines(paste(weather_plots_path,paste(input$city_1," Weather Precipitation ", input$unit_compare," 2022.svg",sep=""),sep="/"), encoding = "UTF-8")
          svg_html <- HTML(paste(svg_contents, collapse = "\n"))
          div(svg_html, class = "svg-container")
        }
      )
    }else{
      output$svg_1_1=HTML("", collapse = "\n")
      div(svg_html, class = "svg-container")
    }
  })
  
  output$svg_2_1 <- renderUI({
    if (input$city_1 != ""){
      tryCatch({
        # plot_daytime_new(input$daytime_city)
        svg_contents <- readLines(paste(daytime_plots_path,paste(input$city_1,"Daytime 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
        svg_html <- HTML(paste(svg_contents, collapse = "\n"))
        div(svg_html, class = "svg-container")}, error = function(e){
          plot_daytime_new(input$city_1)
          svg_contents <- readLines(paste(daytime_plots_path,paste(input$city_1,"Daytime 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
          svg_html <- HTML(paste(svg_contents, collapse = "\n"))
          div(svg_html, class = "svg-container")
        }
      )
    }else{
      output$svg_2_1=HTML("", collapse = "\n")
      div(svg_html, class = "svg-container")
    }
  })
  
  output$svg_3_1 <- renderUI({
    if (input$city_1 != ""){
      tryCatch({
        # plot_weather_condition_new(input$weather_condition_city)
        svg_contents <- readLines(paste(weather_condition_plots_path,paste(input$city_1,"Weather Condition 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
        svg_html <- HTML(paste(svg_contents, collapse = "\n"))
        div(svg_html, class = "svg-container")}, error = function(e){
          plot_weather_condition_new(input$city_1)
          svg_contents <- readLines(paste(weather_condition_plots_path,paste(input$city_1,"Weather Condition 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
          svg_html <- HTML(paste(svg_contents, collapse = "\n"))
          div(svg_html, class = "svg-container")
        }
      )
    }else{
      output$svg_3_1=HTML("", collapse = "\n")
      div(svg_html, class = "svg-container")
    }
  })
  
  output$svg_4_1 <- renderUI({
    if (input$city_1 != ""){
      tryCatch({
        # plot_air_quality_new(input$air_quality_city)
        svg_contents <- readLines(paste(air_quality_plots_path,paste(input$city_1,"Air Pollution 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
        svg_html <- HTML(paste(svg_contents, collapse = "\n"))
        div(svg_html, class = "svg-container")}, error = function(e){
          tryCatch({
            plot_air_quality_new(input$city_1)
            svg_contents <- readLines(paste(air_quality_plots_path,paste(input$city_1,"Air Pollution 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
            svg_html <- HTML(paste(svg_contents, collapse = "\n"))
            div(svg_html, class = "svg-container")}, error = function(e){
              output$svg_4_1 = renderUI({
                HTML(paste("No Data", " ", sep="<br/><br/><br/><br/><br/><br/><br/><br/>"))
              })
            })
        }
      )
    }else{
      output$svg_4_1=HTML("", collapse = "\n")
      div(svg_html, class = "svg-container")
    }
  })
  
  output$svg_1_2 <- renderUI({
    if (input$city_2 != ""){
      tryCatch({
        # plot_weather_precipitation_new(input$weather_city,input$unit)
        svg_contents <- readLines(paste(weather_plots_path,paste(input$city_2," Weather Precipitation ", input$unit_compare," 2022.svg",sep=""),sep="/"), encoding = "UTF-8")
        svg_html <- HTML(paste(svg_contents, collapse = "\n"))
        div(svg_html, class = "svg-container")}, error = function(e){
          plot_weather_precipitation_new(input$city_2,input$unit_compare)
          svg_contents <- readLines(paste(weather_plots_path,paste(input$city_2," Weather Precipitation ", input$unit_compare," 2022.svg",sep=""),sep="/"), encoding = "UTF-8")
          svg_html <- HTML(paste(svg_contents, collapse = "\n"))
          div(svg_html, class = "svg-container")
        }
      )
    }else{
      output$svg_1_2=HTML("", collapse = "\n")
      div(svg_html, class = "svg-container")
    }
  })
  
  output$svg_2_2 <- renderUI({
    if (input$city_2 != ""){
      tryCatch({
        # plot_daytime_new(input$daytime_city)
        svg_contents <- readLines(paste(daytime_plots_path,paste(input$city_2,"Daytime 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
        svg_html <- HTML(paste(svg_contents, collapse = "\n"))
        div(svg_html, class = "svg-container")}, error = function(e){
          plot_daytime_new(input$city_2)
          svg_contents <- readLines(paste(daytime_plots_path,paste(input$city_2,"Daytime 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
          svg_html <- HTML(paste(svg_contents, collapse = "\n"))
          div(svg_html, class = "svg-container")
        }
      )
    }else{
      output$svg_2_2=HTML("", collapse = "\n")
      div(svg_html, class = "svg-container")
    }
  })
  
  output$svg_3_2 <- renderUI({
    if (input$city_2 != ""){
      tryCatch({
        # plot_weather_condition_new(input$weather_condition_city)
        svg_contents <- readLines(paste(weather_condition_plots_path,paste(input$city_2,"Weather Condition 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
        svg_html <- HTML(paste(svg_contents, collapse = "\n"))
        div(svg_html, class = "svg-container")}, error = function(e){
          plot_weather_condition_new(input$city_2)
          svg_contents <- readLines(paste(weather_condition_plots_path,paste(input$city_2,"Weather Condition 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
          svg_html <- HTML(paste(svg_contents, collapse = "\n"))
          div(svg_html, class = "svg-container")
        }
      )
    }else{
      output$svg_3_2=HTML("", collapse = "\n")
      div(svg_html, class = "svg-container")
    }
  })
  
  output$svg_4_2 <- renderUI({
    if (input$city_2 != ""){
      tryCatch({
        # plot_air_quality_new(input$air_quality_city)
        svg_contents <- readLines(paste(air_quality_plots_path,paste(input$city_2,"Air Pollution 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
        svg_html <- HTML(paste(svg_contents, collapse = "\n"))
        div(svg_html, class = "svg-container")}, error = function(e){
          tryCatch({
            plot_air_quality_new(input$city_2)
            svg_contents <- readLines(paste(air_quality_plots_path,paste(input$city_2,"Air Pollution 2022.svg",sep=" "),sep="/"), encoding = "UTF-8")
            svg_html <- HTML(paste(svg_contents, collapse = "\n"))
            div(svg_html, class = "svg-container")}, error = function(e){
              output$svg_4_2 = renderUI({
                HTML(paste("No Data", " ", sep="<br/><br/><br/><br/><br/><br/><br/><br/>"))
              })
            })
        }
      )
    }else{
      output$svg_4_2=HTML("", collapse = "\n")
      div(svg_html, class = "svg-container")
    }
  })
  # options(encoding = "UTF-8")
  df_1 <- reactive({
    df = df[df$Climate.Names==climate_name_dict[input$climate_1],c("city_name","Country")]
    names(df) <- c("City", "Country or Region")
    row.names(df) <- seq_along(df[,1])
    df
  })
  
  df_2 <- reactive({
    df = df[df$city_name==input$city_1|df$city_name==input$city_2,
            c("city_name","Country","First.Administration","latitude..degree.",
              "longitude..degree.","elevation..m.","dist_to_sea..km.","Climate.Names",
              "Koppen.climate","Group","Precipitation.Type","Level.of.Heat" )]
    df$latitude..degree. <- ifelse(df$latitude..degree. > 0, 
                                   paste(as.character(df$latitude..degree.), "\u00B0","N"), 
                                   paste(as.character(-df$latitude..degree.), "\u00B0","S"))
    df$longitude..degree. <- ifelse(df$longitude..degree. > 0, 
                                    paste(as.character(df$longitude..degree.), "\u00B0","E"), 
                                    paste(as.character(-df$longitude..degree.), "\u00B0","W"))
    names(df) <- c("City","Country/Region","First Administration","Latitude (\u00B0)",
                   "Longitude (\u00B0)","Elevation (m)","Distance to the Sea (km)","Koppen Climate",
                   "Climate Code","Group","Precipitation Type","Level of Heat" )
    row.names(df) <- seq_along(df[,1])
    df
  })
  
  df_3 <- reactive({
    df = df[df$city_name==input$city_1|df$city_name==input$city_2,
            c("city_name","Country","mean_temp..Celsius.","mean_solarradiation..W.m2.",
              "mean_humidity....","mean_windspeed..kph.","temp_std..Celsius.","mean_temp_range..Celsius.",
              "mean_abs_temp_diff_1..Celsius.","precip_std..mm.","precip_sum..mm." ,"temp.precip.corr",
              "humid.radiat.corr", "humid.precip.corr", "temp.radiat.corr")]
    
    names(df) <- c("City","Country/Region","Annual Mean Temp (\u00B0C)","Annual Mean Solarradiation (W/m2)",
                   "Annual Mean Humidity (\u0025)","Mean Windspeed (kph)","Annual Temp Std (\u00B0C)","Annual Mean Temp Range (\u00B0C)",
                   "Annual Mean Temp Diff (\u00B0C)","Precip Std (mm)","Precip Sum (mm)" ,"Temp Precip Corr",
                   "Humid Radiat Corr", "Humid Precip Corr", "Temp Radiat Corr")
    row.names(df) <- seq_along(df[,1])
    df
  })
  
  df_4 <- reactive({
    df = df[df$Country==input$country_1,c("city_name","Climate.Names","Koppen.climate")]
    names(df) <- c("City", "Koppen Climate","Climate Code")
    row.names(df) <- seq_along(df[,1])
    df
  })
  
  output$df_1 = DT::renderDataTable({
    DT::datatable(df_1(), escape = FALSE, 
                  options = list(dom = 't', paging = FALSE))
  })
  
  output$df_2 = DT::renderDataTable({
    DT::datatable(df_2(), escape = FALSE, 
                  options = list(dom = 't', paging = FALSE))
  })
  
  output$df_3 = DT::renderDataTable({
    DT::datatable(df_3(), escape = FALSE, 
                  options = list(dom = 't', paging = FALSE))
  })
  
  output$df_4 = DT::renderDataTable({
    DT::datatable(df_4(), escape = FALSE, 
                  options = list(dom = 't', paging = FALSE))
  })
  
  output$text_1 = renderText({
    input$climate_1
  })
  
  output$text_2 = renderText({
    "General Information:"
  })
  
  output$text_3 = renderText({
    "Weather Statistics:"
  })
  
  df_7 <- reactive({
    if (input$country_2 == "(All)"){
    } else {
      df = df[df$Country == input$country_2, ]
    }
    if (input$climate_3 == "(All)"){
    } else {
      df = df[df$Climate.Names==climate_name_dict[input$climate_3], ]
    }
    df
  })
  
  output$map_1 <- renderLeaflet({
    df = df_7()
    popup = paste0("<strong>City:</strong> ", df$city_name, "<br>",
                   "<strong>Country/Region:</strong> ", df$Country, "<br>",
                   "<strong>Latitude:</strong> ", df$lat, "<br>",
                   "<strong>Longitude:</strong> ", df$long, "<br>",
                   "<strong>Elevation (m):</strong> ", df$elevation..m., "<br>",
                   "<strong>Distance to the Sea (km):</strong> ", df$dist_to_sea..km., "<br>",
                   "<strong>Koppen Climate:</strong> ", df$Climate.Names, "<br>",
                   "<strong>Climate Code:</strong> ", df$Koppen.climate, "<br>",
                   "<strong>Annual Mean Temperature (\u00B0C):</strong> ", df$mean_temp..Celsius., "<br>",
                   "<strong>Annual Precipitation Sum (mm):</strong> ", df$precip_sum..mm., "<br>"
    )
    leaflet(data = df) %>%
      addTiles() %>%
      addCircleMarkers(lat = as.numeric(df$latitude..degree.), 
                       lng = as.numeric(df$longitude..degree.),
                       label = lapply(as.list(popup), HTML),
                       radius = 4,color = ~Color.Code,
                       stroke = FALSE, fillOpacity = 0.8
      )
  })
  
  df_5 <- reactive({
    if (input$country_2 == "(All)"){
    } else {
      df = df[df$Country == input$country_2, ]
    }
    if (input$climate_3 == "(All)"){
    } else {
      df = df[df$Climate.Names==climate_name_dict[input$climate_3], ]
    }
    df <- df %>% 
      group_by(`Climate.Names`, `Koppen.climate`) %>% 
      summarise(Cities = paste(city_name, collapse = ", ")) %>% 
      arrange(`Koppen.climate`) %>% 
      rename( `Koppen Climate Classification`= `Climate.Names`,
              `Climate Code`= `Koppen.climate`)
    df
  })
  
  output$df_5 = DT::renderDataTable({
    DT::datatable(df_5(), escape = FALSE, 
                  options = list(dom = 't', paging = FALSE))
  })
  
  output$text_5 = renderText({
    "Climates and Cities:"
  })
  
  df_6 <- reactive({
    df= df[df$city_name==input$city_1|df$city_name==input$city_2,]
    row.names(df) <- seq_along(df[,1])
    df
  })
  
  output$map_2 <- renderLeaflet({
    df_6 = df_6()
    popup = paste0("<strong>City:</strong> ", df_6$city_name, "<br>",
                   "<strong>Country/Region:</strong> ", df_6$Country, "<br>",
                   "<strong>Latitude:</strong> ", df_6$lat, "<br>",
                   "<strong>Longitude:</strong> ", df_6$long, "<br>",
                   "<strong>Elevation (m):</strong> ", df_6$elevation..m., "<br>",
                   "<strong>Distance to the Sea (km):</strong> ", df_6$dist_to_sea..km., "<br>",
                   "<strong>Koppen Climate:</strong> ", df_6$Climate.Names, "<br>",
                   "<strong>Climate Code:</strong> ", df_6$Koppen.climate, "<br>",
                   "<strong>Annual Mean Temperature (\u00B0C):</strong> ", df_6$mean_temp..Celsius., "<br>",
                   "<strong>Annual Precipitation Sum (mm):</strong> ", df_6$precip_sum..mm., "<br>"
    )
    leaflet(data = df_6) %>%
      addTiles() %>%
      addCircleMarkers(lat = as.numeric(df_6$latitude..degree.), 
                       lng = as.numeric(df_6$longitude..degree.),
                       label = lapply(as.list(popup), HTML),
                       radius = 6,color = df_6$Color.Code,
                       stroke = FALSE, fillOpacity = 0.8
      )
  })
  
  plot_df_1 = reactive({
    if (input$plot_1_x_transform == "Linear"){
      df$x = as.numeric(df[[viz_cols_mapping[[input$plot_1_x_axis]]]])
    } else if (input$plot_1_x_transform == "Absolute") {
      # df[[viz_cols_mapping[[input$plot_1_x_axis]]]] 
      df$x = abs(as.numeric(df[[viz_cols_mapping[[input$plot_1_x_axis]]]]))
    } else if (input$plot_1_x_transform == "Cosine") {
      # df[[viz_cols_mapping[[input$plot_1_x_axis]]]] 
      df$x = cos(as.numeric(df[[viz_cols_mapping[[input$plot_1_x_axis]]]])/360*2*3.1415926)
    } else {
      # df[[viz_cols_mapping[[input$plot_1_x_axis]]]] 
      df$x = ifelse(as.numeric(df[[viz_cols_mapping[[input$plot_1_x_axis]]]])>0,log(as.numeric(df[[viz_cols_mapping[[input$plot_1_x_axis]]]])),-log(-as.numeric(df[[viz_cols_mapping[[input$plot_1_x_axis]]]]))) 
    }
    if (input$plot_1_y_transform == "Linear"){
      df$y = as.numeric(df[[viz_cols_mapping[[input$plot_1_y_axis]]]])
    } else if (input$plot_1_y_transform == "Absolute") {
      # df[[viz_cols_mapping[[input$plot_1_y_axis]]]] 
      df$y = abs(as.numeric(df[[viz_cols_mapping[[input$plot_1_y_axis]]]]))
    } else if (input$plot_1_y_transform == "Cosine") {
      # df[[viz_cols_mapping[[input$plot_1_y_axis]]]] 
      df$y = cos(as.numeric(df[[viz_cols_mapping[[input$plot_1_y_axis]]]])/360*2*3.1415926)
    } else {
      # df[[viz_cols_mapping[[input$plot_1_y_axis]]]] 
      df$y = ifelse(as.numeric(df[[viz_cols_mapping[[input$plot_1_y_axis]]]])>0,log(as.numeric(df[[viz_cols_mapping[[input$plot_1_y_axis]]]])),-log(-as.numeric(df[[viz_cols_mapping[[input$plot_1_y_axis]]]]))) 
    }
    df
  })
  
  plot_df_2 = reactive({
    if (input$plot_2_x_transform == "Linear"){
      df$x = as.numeric(df[[viz_cols_mapping[[input$plot_2_x_axis]]]])
    } else if (input$plot_2_x_transform == "Absolute") {
      df$x = abs(as.numeric(df[[viz_cols_mapping[[input$plot_2_x_axis]]]]))
    } else if (input$plot_2_x_transform == "Cosine") {
      df$x = cos(as.numeric(df[[viz_cols_mapping[[input$plot_2_x_axis]]]])/360*2*3.1415926)
    } else {
      df$x = ifelse(as.numeric(df[[viz_cols_mapping[[input$plot_2_x_axis]]]])>0,log(as.numeric(df[[viz_cols_mapping[[input$plot_2_x_axis]]]])),-log(-as.numeric(df[[viz_cols_mapping[[input$plot_2_x_axis]]]]))) 
    }
    if (input$plot_2_y_transform == "Linear"){
      df$y = as.numeric(df[[viz_cols_mapping[[input$plot_2_y_axis]]]])
    } else if (input$plot_2_y_transform == "Absolute") {
      df$y = abs(as.numeric(df[[viz_cols_mapping[[input$plot_2_y_axis]]]]))
    } else if (input$plot_2_y_transform == "Cosine") {
      df$y = cos(as.numeric(df[[viz_cols_mapping[[input$plot_2_y_axis]]]])/360*2*3.1415926)
    } else {
      df$y = ifelse(as.numeric(df[[viz_cols_mapping[[input$plot_2_y_axis]]]])>0,log(as.numeric(df[[viz_cols_mapping[[input$plot_2_y_axis]]]])),-log(-as.numeric(df[[viz_cols_mapping[[input$plot_2_y_axis]]]]))) 
    }
    if (input$plot_2_z_transform == "Linear"){
      df$z = as.numeric(df[[viz_cols_mapping[[input$plot_2_z_axis]]]])
    } else if (input$plot_2_z_transform == "Absolute") {
      df$z = abs(as.numeric(df[[viz_cols_mapping[[input$plot_2_z_axis]]]]))
    } else if (input$plot_2_z_transform == "Cosine") {
      df$z = cos(as.numeric(df[[viz_cols_mapping[[input$plot_2_z_axis]]]])/360*2*3.1415926)
    } else {
      df$z = ifelse(as.numeric(df[[viz_cols_mapping[[input$plot_2_z_axis]]]])>0,log(as.numeric(df[[viz_cols_mapping[[input$plot_2_z_axis]]]])),-log(-as.numeric(df[[viz_cols_mapping[[input$plot_2_z_axis]]]]))) 
    }
    df
  })
  
  output$plot_1 <- renderPlotly({
    p = ggplot(data = plot_df_1())+
      geom_point(aes(x,
                     y,
                     text = paste(
                       "City: ", df$city_name,
                       "\nCountry/Region: ", df$Country,
                       "\nKoppen Climate: ", df$Climate.Names,
                       "\nClimate Code: ", df$Koppen.climate,
                       "\n",input$plot_1_x_axis,": ", df[[viz_cols_mapping[[input$plot_1_x_axis]]]],
                       "\n",input$plot_1_y_axis,": ", df[[viz_cols_mapping[[input$plot_1_y_axis]]]])),
                 color=df$Color.Code)+                                     
      stat_smooth(aes(x = x, y = y), method = "lm") + theme_bw() + 
      xlab(input$plot_1_x_axis) +
      ylab(input$plot_1_y_axis) 
    ggplotly(p, tooltip = "text")
  })
  
  output$regression_text_1 <- renderText({
    model = lm(y ~ x, data = plot_df_1())
    adjr2 = summary(model)$adj.r.squared
    fstat = summary(model)$fstatistic[1]
    paste("adjusted R2: ",round(adjr2,4),", ","f-statistics: ",round(fstat,4))
  })
  
  output$plot_2 = renderPlotly({
    plot_ly(plot_df_2(), x = ~x, y = ~y, z = ~z, 
            text = paste(
              "City: ", df$city_name,
              "\nCountry/Region: ", df$Country,
              "\nKoppen Climate: ", df$Climate.Names,
              "\nClimate Code: ", df$Koppen.climate,
              "\n",input$plot_1_x_axis,": ", df[[viz_cols_mapping[[input$plot_1_x_axis]]]],
              "\n",input$plot_1_y_axis,": ", df[[viz_cols_mapping[[input$plot_1_y_axis]]]]),
            color=df$Koppen.climate,
            colors = df$Color.Code)%>%
      layout(scene = list(xaxis = list(title = input$plot_2_x_axis), 
                          yaxis = list(title = input$plot_2_y_axis), 
                          zaxis = list(title = input$plot_2_z_axis))
      )
  }) 
  output$contribution_text = renderUI({
    HTML("<p>This project is created by Yifei Sun.</p>
<p>Contribute to this project by uploading more data to <a href='https://github.com/YifeiSun01/Weather-Visualization/tree/main/data'>this Github</a>.</p>
<p>Data Source:</p> 
<p><a href='https://www.visualcrossing.com/weather/'>VISUAL CROSSING WEATHER</a></p>
<p><a href='https://aqicn.org/data-platform/register/#form'>Air Quality Historical Data Platform</a></p>")
  })
}