source("functions.R")

server <- function(input, output) {
  
  output$svg_1_1 <- renderPlot({
    if (input$city_1 != ""){
      plot_weather_precipitation_return_new(input$city_1, input$unit_compare)
    }else{
      output$svg_1_1=HTML("", collapse = "\n")
    }
  })
  
  output$svg_2_1 <- renderPlot({
    if (input$city_1 != ""){
      plot_daytime_return_new(input$city_1)
    }else{
      output$svg_2_1=HTML("", collapse = "\n")
    }
  })
  
  output$svg_3_1 <- renderPlot({
    if (input$city_1 != ""){
      plot_weather_condition_return_new(input$city_1)
    }else{
      output$svg_3_1=HTML("", collapse = "\n")
    }
  })
  
  output$svg_4_1 <- renderPlot({
    if (input$city_1 != ""){
      plot_air_quality_return_new(input$city_1)
    }else{
      output$svg_4_1=HTML("", collapse = "\n")
    }
  })
  
  output$svg_5_1 <- renderPlot({
    if (input$city_1 != ""){
      plot_weather_micelaneous_return_new(input$city_1, input$unit_compare)
    }else{
      output$svg_5_1=HTML("", collapse = "\n")
    }
  })
  
  output$svg_6_1 <- renderPlot({
    if (input$city_1 != ""){
      plot_precipitation_snow_return_new(input$city_1, input$unit_compare)
    }else{
      output$svg_6_1=HTML("", collapse = "\n")
    }
  })
  
  output$svg_7_1 <- renderPlot({
    if (input$city_1 != ""){
      plot_wind_return_new(input$city_1, input$unit_compare)
    }else{
      output$svg_7_1=HTML("", collapse = "\n")
    }
  })
  
  output$svg_8_1 <- renderPlot({
    if (input$city_1 != ""){
      plot_humidity_return_new(input$city_1, input$unit_compare)
    }else{
      output$svg_8_1=HTML("", collapse = "\n")
    }
  })
  
  output$svg_1_2 <- renderPlot({
    if (input$city_2 != ""){
      plot_weather_precipitation_return_new(input$city_2, input$unit_compare)
    }else{
      output$svg_1_2=HTML("", collapse = "\n")
    }
  })
  
  output$svg_2_2 <- renderPlot({
    if (input$city_2 != ""){
      plot_daytime_return_new(input$city_2)
    }else{
      output$svg_2_2=HTML("", collapse = "\n")
    }
  })
  
  output$svg_3_2 <- renderPlot({
    if (input$city_2 != ""){
      plot_weather_condition_return_new(input$city_2)
    }else{
      output$svg_3_2=HTML("", collapse = "\n")
    }
  })
  
  output$svg_4_2 <- renderPlot({
    if (input$city_2 != ""){
      plot_air_quality_return_new(input$city_2)
    }else{
      output$svg_4_2=HTML("", collapse = "\n")
    }
  })
  
  output$svg_5_2 <- renderPlot({
    if (input$city_2 != ""){
      plot_weather_micelaneous_return_new(input$city_2, input$unit_compare)
    }else{
      output$svg_5_2=HTML("", collapse = "\n")
    }
  })
  
  output$svg_6_2 <- renderPlot({
    if (input$city_2 != ""){
      plot_precipitation_snow_return_new(input$city_2, input$unit_compare)
    }else{
      output$svg_6_2=HTML("", collapse = "\n")
    }
  })
  
  output$svg_7_2 <- renderPlot({
    if (input$city_2 != ""){
      plot_wind_return_new(input$city_2, input$unit_compare)
    }else{
      output$svg_7_2=HTML("", collapse = "\n")
    }
  })
  
  output$svg_8_2 <- renderPlot({
    if (input$city_1 != ""){
      plot_humidity_return_new(input$city_2, input$unit_compare)
    }else{
      output$svg_8_2=HTML("", collapse = "\n")
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
              "Koppen.climate","Group","Precipitation.Type","Level.of.Heat", "Color.Code")]
    df$latitude..degree. <- ifelse(df$latitude..degree. > 0, 
                                   paste(as.character(df$latitude..degree.), "\u00B0","N"), 
                                   paste(as.character(-df$latitude..degree.), "\u00B0","S"))
    df$longitude..degree. <- ifelse(df$longitude..degree. > 0, 
                                    paste(as.character(df$longitude..degree.), "\u00B0","E"), 
                                    paste(as.character(-df$longitude..degree.), "\u00B0","W"))
    names(df) <- c("City","Country/Region","First-Level Administrative Division","Latitude (\u00B0)",
                   "Longitude (\u00B0)","Elevation (m)","Distance to the Sea (km)","Koppen Climate",
                   "Climate Code","Group","Precipitation Type","Level of Heat", "Color Code")
    row.names(df) <- seq_along(df[,1])
    df
  })
  
  df_3 <- reactive({
    df = df[df$city_name==input$city_1|df$city_name==input$city_2,
            c("city_name","Country","mean_temp..Celsius.","mean_solarradiation..W.m2.",
              "mean_humidity....","mean_windspeed..kph.","temp_std..Celsius.","mean_temp_range..Celsius.",
              "mean_abs_temp_diff_1..Celsius.","precip_std..mm.","precip_sum..mm." ,"temp.precip.corr",
              "humid.radiat.corr", "humid.precip.corr", "temp.radiat.corr", "Color.Code")]
    
    names(df) <- c("City","Country/Region","Annual Mean Temp (\u00B0C)","Annual Mean Solarradiation (W/m2)",
                   "Annual Mean Humidity (\u0025)","Mean Windspeed (kph)","Annual Temp Std (\u00B0C)","Annual Mean Temp Range (\u00B0C)",
                   "Annual Mean Temp Diff (\u00B0C)","Precip Std (mm)","Precip Sum (mm)" ,"Temp Precip Corr",
                   "Humid Radiat Corr", "Humid Precip Corr", "Temp Radiat Corr", "Color Code")
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
  
  output$df_2 = renderReactable({
    color_vec = df_2()[["Color Code"]]
    reactable(df_2()[,-ncol(df_2())], searchable = TRUE, defaultPageSize = 10, 
              rowStyle = function(index) {
                list(background = color_vec[index],
                     color = contrast_color(color_vec[index])
                )
              }
    )
  })
  
  output$df_3 = renderReactable({
    color_vec = df_3()[["Color Code"]]
    reactable(df_3()[,-ncol(df_3())], searchable = TRUE, defaultPageSize = 10, 
              rowStyle = function(index) {
                list(background = color_vec[index],
                     color = contrast_color(color_vec[index])
                )
              }
    )
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
  
  df_10 <- reactive({
    if (input$country_2 == "(All)"){
    } else {
      df = df[df$Country == input$country_2, ]
    }
    if (input$climate_3 == "(All)"){
    } else {
      df = df[df$Climate.Names==climate_name_dict[input$climate_3], ]
    }
    df <- df %>% 
      group_by(`Koppen.climate`, `Color.Code`) %>% 
      summarise(Cities = paste(city_name, collapse = ", ")) %>% 
      arrange(`Koppen.climate`)
    df
  })
  
  
  output$df_5 = renderReactable({
    color_vec = df_10()$Color.Code
    reactable(df_5(), searchable = TRUE, defaultPageSize = 10, 
              columns = list(
      "Koppen Climate Classification" = colDef(width = 200),
      "Climate Code" = colDef(width = 80),
      "Cities" = colDef(width = 650)
    ),
    rowStyle = function(index) {
      list(background = color_vec[index],
           color = contrast_color(color_vec[index])
           )
    }
  )
  })
  
  df_8 <- reactive({
    if (input$climate_3 == "(All)"){
    } else {
      df_index = df_index[df_index$Climate==climate_code_dict[input$climate_3], ]
    }
    df = df_index %>% arrange(`Climate`)
    df})
  
  output$df_8 = renderReactable({
    color_vec = df_10()$Color.Code
    reactable(df_8(), searchable = TRUE,defaultPageSize = 10, 
              columns = list(
                "Climate" = colDef(width = 70),
                "1st" = colDef(width = 100),
                "2nd" = colDef(width = 100),
                "3rd" = colDef(width = 100),
                "Climate Name" = colDef(width = 100),
                "Description" = colDef(width = 1000)
             ),
              rowStyle = function(index) {
                list(background = if (is.null(climate_color_dict[[df_8()$Climate[index]]])){
                  "#FFFFFF"
                } else {
                  climate_color_dict[[df_8()$Climate[index]]]
                },
                     color = contrast_color(if (is.null(climate_color_dict[[df_8()$Climate[index]]])){
                       "#FFFFFF"
                       } else {
                         climate_color_dict[[df_8()$Climate[index]]]
                       })
                ) 
                }
          )
        })
  
  # output$df_8 = DT::renderDataTable({
  #   DT::datatable(df_8(), escape = FALSE, 
  #                 options = list(dom = 't', paging = FALSE))
  # })
  
  df_9 <- reactive({
    df_descriptions
     if (input$climate_3 == "(All)"){
     } else {
       df_descriptions = df_descriptions[df_descriptions$Climate.Code==climate_code_dict[input$climate_3], ]
     }
     df_descriptions
   })
  
   output$df_9 = DT::renderDataTable({
     DT::datatable(df_9(), escape = FALSE, 
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
  
  output$df_10 = renderReactable({
    color_vec = df_wind$Color
    df = subset(df_wind, select = -Color)
    reactable(df, searchable = TRUE,defaultPageSize = 10, 
              columns = list(
                "Beaufort number" = colDef(width = 70),
                "knot（kt)" = colDef(width = 60),
                "km/h" = colDef(width = 50),
                "m/s" = colDef(width = 50),
                "Wave height (m)" = colDef(width = 100),
                "Description" = colDef(width = 70),
                "Sea conditions" = colDef(width = 400),
                "Land conditions" = colDef(width = 400)
              ),
              rowStyle = function(index) {
                list(background = color_vec[index],
                color = contrast_color(color_vec[index])
                )
              }
    )
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
    df = plot_df_1()
    normal_size=1
    stress_size=3
    normal_opacity=0.3
    stress_opacity=1
    original_size=2
    original_opacity=0.8
    if (input$country_3 == "(All)" & input$climate_4 == "(All)") {
      size_vec = rep(original_size,length(df$city_name))
      opacity_vec = rep(original_opacity,length(df$city_name))
    } else {
      if (input$country_3 != "(All)" & input$climate_4 == "(All)") {
        size_vec = ifelse(df$Country==input$country_3,stress_size,normal_size) 
        opacity_vec = ifelse(df$Country==input$country_3,stress_opacity,normal_opacity) 
      } else if (input$country_3 == "(All)" & input$climate_4 != "(All)"){
        size_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_4],stress_size,normal_size)
        opacity_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_4],stress_opacity,normal_opacity) 
      } else if (input$country_3 != "(All)" & input$climate_4 != "(All)"){
        size_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_4] & df$Country==input$country_3,stress_size,normal_size)
        opacity_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_4] & df$Country==input$country_3,stress_opacity,normal_opacity)
      }
    }
    
    if (input$favorite_1 == "No"){
      
    } else {
      lower_quantile = input$ranking_range[1]
      upper_quantile = input$ranking_range[2]
      lower_ranking = round(lower_quantile*nrow(df_city_ranking())/100,0)
      upper_ranking = round(upper_quantile*nrow(df_city_ranking())/100,0)
      chosen_city_list = as.vector(df_city_ranking()[lower_ranking+1:upper_ranking,c("City")])
      size_vec = ifelse(df$city_name %in% chosen_city_list,size_vec,normal_size)
      opacity_vec = ifelse(df$city_name %in% chosen_city_list,opacity_vec,normal_opacity)
    }
      
    p = ggplot(data = df)+
      geom_point(aes(x,
                     y,
                     text = paste(
                       "City: ", df$city_name,
                       "\nCountry/Region: ", df$Country,
                       "\nKoppen Climate: ", df$Climate.Names,
                       "\nClimate Code: ", df$Koppen.climate,
                       "\n",input$plot_1_x_axis,": ", df[[viz_cols_mapping[[input$plot_1_x_axis]]]],
                       "\n",input$plot_1_y_axis,": ", df[[viz_cols_mapping[[input$plot_1_y_axis]]]])),
                 color=df$Color.Code,
                 size=size_vec,
                 alpha=opacity_vec)+                                     
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
    df = plot_df_2()
    normal_size=15
    stress_size=25
    normal_opacity=0.8
    stress_opacity=1
    original_size=20
    original_opacity=0.9
    if (input$country_4 == "(All)" & input$climate_5 == "(All)") {
      size_vec = rep(original_size,length(df$city_name))
      opacity_vec = rep(original_opacity,length(df$city_name))
    } else {
      if (input$country_4 != "(All)" & input$climate_5 == "(All)") {
        size_vec = ifelse(df$Country==input$country_4,stress_size,normal_size) 
        opacity_vec = ifelse(df$Country==input$country_4,stress_opacity,normal_opacity) 
      } else if (input$country_4 == "(All)" & input$climate_5 != "(All)"){
        size_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_5],stress_size,normal_size)
        opacity_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_5],stress_opacity,normal_opacity) 
      } else if (input$country_4 != "(All)" & input$climate_5 != "(All)"){
        size_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_5] & df$Country==input$country_4,stress_size,normal_size)
        opacity_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_5] & df$Country==input$country_4,stress_opacity,normal_opacity)
      }
    }
    
    if (input$favorite_2 == "No"){
      
    } else {
      lower_quantile = input$ranking_range[1]
      upper_quantile = input$ranking_range[2]
      lower_ranking = round(lower_quantile*nrow(df_city_ranking())/100,0)
      upper_ranking = round(upper_quantile*nrow(df_city_ranking())/100,0)
      chosen_city_list = as.vector(df_city_ranking()[lower_ranking+1:upper_ranking,c("City")])
      size_vec = ifelse(df$city_name %in% chosen_city_list,size_vec,normal_size)
      opacity_vec = ifelse(df$city_name %in% chosen_city_list,opacity_vec,normal_opacity)
    }
    
    plot_ly(df, x = ~x, y = ~y, z = ~z, 
            text = paste(
              "City: ", df$city_name,
              "\nCountry/Region: ", df$Country,
              "\nKoppen Climate: ", df$Climate.Names,
              "\nClimate Code: ", df$Koppen.climate,
              "\n",input$plot_1_x_axis,": ", df[[viz_cols_mapping[[input$plot_1_x_axis]]]],
              "\n",input$plot_1_y_axis,": ", df[[viz_cols_mapping[[input$plot_1_y_axis]]]]),
            color=df$Koppen.climate,
            colors = df$Color.Code,
            opacity = opacity_vec,
            marker = list(size = size_vec)
    )%>%
      layout(scene = list(xaxis = list(title = input$plot_2_x_axis), 
                          yaxis = list(title = input$plot_2_y_axis), 
                          zaxis = list(title = input$plot_2_z_axis))
      )
  }) 
  
  output$map_3 = renderLeaflet({
    df = plot_df_2()
    normal_size=15
    stress_size=25
    normal_opacity=0.3
    stress_opacity=1
    original_size=10
    original_opacity=0.8
    if (input$country_5 == "(All)" & input$climate_6 == "(All)") {
      size_vec = rep(original_size,length(df$city_name))
      opacity_vec = rep(original_opacity,length(df$city_name))
    } else {
      if (input$country_5 != "(All)" & input$climate_6 == "(All)") {
        size_vec = ifelse(df$Country==input$country_5,stress_size,normal_size) 
        opacity_vec = ifelse(df$Country==input$country_5,stress_opacity,normal_opacity) 
      } else if (input$country_5 == "(All)" & input$climate_6 != "(All)"){
        size_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_6],stress_size,normal_size)
        opacity_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_6],stress_opacity,normal_opacity) 
      } else if (input$country_5 != "(All)" & input$climate_6 != "(All)"){
        size_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_6] & df$Country==input$country_4,stress_size,normal_size)
        opacity_vec = ifelse(df$Climate.Names==climate_name_dict[input$climate_6] & df$Country==input$country_4,stress_opacity,normal_opacity)
      }
    }
    
    
    if (input$favorite_3 == "No"){
      
    } else {
      lower_quantile = input$ranking_range[1]
      upper_quantile = input$ranking_range[2]
      lower_ranking = round(lower_quantile*nrow(df_city_ranking())/100,0)
      upper_ranking = round(upper_quantile*nrow(df_city_ranking())/100,0)
      chosen_city_list = as.vector(df_city_ranking()[lower_ranking+1:upper_ranking,c("City")])
      size_vec = ifelse(df$city_name %in% chosen_city_list,size_vec,normal_size)
      opacity_vec = ifelse(df$city_name %in% chosen_city_list,opacity_vec,normal_opacity)
    }
    
    popup = paste0("<strong>City:</strong> ", df$city_name, "<br>",
                   "<strong>Country/Region:</strong> ", df$Country, "<br>",
                   "<strong>Latitude:</strong> ", df$lat, "<br>",
                   "<strong>Longitude:</strong> ", df$long, "<br>",
                   "<strong>Elevation (m):</strong> ", df$elevation..m., "<br>",
                   "<strong>Distance to the Sea (km):</strong> ", df$dist_to_sea..km., "<br>",
                   "<strong>Koppen Climate:</strong> ", df$Climate.Names, "<br>",
                   "<strong>Climate Code:</strong> ", df$Koppen.climate, "<br>",
                   "<strong>",input$map_3_feature,":</strong> ", df[[viz_cols_mapping[[input$map_3_feature]]]], "<br>")
    rc2 <- colorRampPalette(colors = c("green", "red"), space = "Lab")(180)
    pal <- colorQuantile(
      palette = rc2,
      domain = as.numeric(df[[viz_cols_mapping[[input$map_3_feature]]]]),
      n = 10,
      na.color = "#808080"
    )
    qpal_colors <- unique(pal(sort(df[[viz_cols_mapping[[input$map_3_feature]]]]))) # hex codes
    qpal_labs <- round(quantile(df[[viz_cols_mapping[[input$map_3_feature]]]], seq(0, 1, 1/(length(qpal_colors))), na.rm=TRUE),2)
    qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1]
    
    leaflet(data = df) %>%
      addTiles() %>%
      addCircleMarkers(lat = as.numeric(df$latitude..degree.), 
                       lng = as.numeric(df$longitude..degree.),
                       label = lapply(as.list(popup), HTML),
                       radius = 6,color = ~pal(df[[viz_cols_mapping[[input$map_3_feature]]]]),
                       stroke = FALSE, fillOpacity = opacity_vec)%>%
      addLegend("bottomright",
                title = input$map_3_feature,
                labFormat = labelFormat(prefix = ""),
                colors = qpal_colors, labels = qpal_labs, 
                opacity = 1)
  })
  
  output$mapImage <- renderImage({
    if (input$climate_group_1 == "(All)"){list(src = paste("maps/",climate_zone_dict[[input$climate_group_1]],sep=""), 
                                               contentType = "Images/png", 
                                               width = 900, height = 500,
                                               alt = paste("The Distribution of World's Climates"))} else {
          list(src = paste("maps/",climate_zone_dict[[input$climate_group_1]],sep=""), 
         contentType = "Images/png", 
         width = 900, height = 600,
        alt = paste("The Distribution of ",input$climate_group_1," Climates"))
        }
  }, deleteFile = F)
  
  output$contribution_text = renderUI({
    HTML("<p>This project is created by Yifei Sun, as the final project of SI 649 Information Visualization at the University of Michigan</p>
    <p>Inspired by <a href='http://www.weather-radials.com/'>WEATHER RADIALS</a></p>
<p>Contribute to this project by uploading more data to <a href='https://github.com/YifeiSun01/Weather-Visualization/tree/main/data'>this Github page</a>.</p>
<p>Data Source:</p> 
<p><a href='https://www.visualcrossing.com'>VISUAL CROSSING WEATHER</a></p>
<p><a href='https://aqicn.org/data-platform/register/#form'>Air Quality Historical Data Platform</a></p>
<p><a href='https://www.tamdistrict.org/cms/lib/CA01000875/Centricity/Domain/986/ClimateControls14.pdf'>What	Causes	Different	Climates?
</a></p>
<p><a href='https://en.wikipedia.org/wiki/K%C3%B6ppen_climate_classification'>Köppen climate classification</a></p>
         ")
  })
  
  output$explain_text_1 = renderUI({
    HTML("<p><b>Relative humidity:</b></p>
    <p>Relative humidity, often expressed as a percentage, indicates a present state of absolute humidity relative to a maximum humidity given the same temperature.</p>
    <p>The ideal relative humidity for health and comfort is somewhere between 30-50% humidity, according to the Mayo Clinic. This means that the air holds between 30-50% of the maximum amount of moisture it can contain.</p>
    <p><b>Feelslike Temperature:</b></p>
    <p>The 'feels like' temperature is a measurement of how hot or cold it really feels like outside. The 'Feels Like' temperature relies on environmental data including the ambient air temperature, relative humidity, and wind speed to determine how weather conditions feel to bare skin.</p>
    <p>Different combinations of temperature, humidity, and wind speed can increase the sensation of being hot or cold. For example, skin that is exposed to wind and cold temperatures will make a person feel that it is colder outside than it really is because heat is drawn away from the body at a faster rate. As another example, a day that is very humid may feel hotter than it really is outside because your body sweat does not evaporate (and thereby cool the body) the way it is intended.</p>
         <p>The World Health Organization in 1987 found that comfortable indoor temperatures of between 18 and 24 °C (64 and 75 °F) were not associated with health risks for healthy adults with appropriate clothing, humidity, and other factors.</p>"
          )
  })  
  
  output$explain_text_2 = renderUI({
    HTML("<p><b>Dew Points/Dew Point Temperature:</b></p>
<p>The dew point is the temperature the air needs to be cooled to (at constant pressure) in order to achieve a relative humidity (RH) of 100%.At this point the air cannot hold more water in the gas form. If the air were to be cooled even more, water vapor would have to come out of the atmosphere in the liquid form, usually as fog or precipitation.</p>
<p>The higher the dew point rises, the greater the amount of moisture in the air. This directly affects how 'comfortable' it will feel outside. Many times, relative humidity can be misleading. For example, a temperature of 30 and a dew point of 30 will give you a relative humidity of 100%, but a temperature of 80 and a dew point of 60 produces a relative humidity of 50%. It would feel much more 'humid' on the 80 degree day with 50% relative humidity than on the 30 degree day with a 100% relative humidity. This is because of the higher dew point.</p>
</br>
<p>General comfort levels USING <b> DEW POINT </b> that can be expected during the summer months:</p>
<ol>
<li>less than or equal to 55: dry and comfortable</li>
<li>between 55 and 65: becoming 'sticky' with muggy evenings</li>
<li>greater than or equal to 65: lots of moisture in the air, becoming oppressive</li>
         </ol>"
         )
  })
  
  output$explain_text_3 = renderUI({
    HTML("<p><b>Ultra Violet Index:</b></p>
    <p>The UVI is a measure of the level of UV radiation.</p>
<p>The values of the index range from zero upward - the higher the UVI, the greater the potential for damage to the skin and eye, and the less time it takes for harm to occur.</p>
<p>The UVI is an important vehicle to alert people about the need to use sun protection.</p>
  <p><table>
  <tr>
  <th><b>UV index</b></th>
  <th><b>Action</b></th>
  </tr>
  <tr>
  <td>0 to 2</td>
  <td>You can safely enjoy being outside!</td>
  </tr>
  <tr>
  <td>3 to 7</td>
  <td>Seek shade during midday hours! Slip on a shirt, slop on sunscreen and slap on hat!</td>
  </tr>
  <tr>
  <td>8 and above</td>
  <td>Avoid being outside during midday hours! Make sure you seek shade! Shirt, sunscreen and hat are a must!</td>
  </tr>
  </table></p>     
         "
    )
  })  
  
  output$explain_text_4 = renderUI({
    HTML(
  "<p><b>Air Quality Index /AQI:</b></p>
  <p>An air quality index (AQI) is used by government agencies to communicate to the public how polluted the air currently is or how polluted it is forecast to become. AQI information is obtained by averaging readings from an air quality sensor, which can increase due to vehicle traffic, forest fires, or anything that can increase air pollution. Pollutants tested include particulates, ozone, nitrogen dioxide, carbon monoxide, sulphur dioxide, among others.</p>
  <p><table>
  <tr>
  <th><b>AQI</b></th>
  <th><b>Air Pollution Level</b></th>
  <th><b>Health Implications</b></th>
  <th><b>Cautionary Statement (for PM2.5)</b></th>
  </tr>
  <tr>
  <td>0 - 50</td>
  <td>Good</td>
  <td>Air quality is considered satisfactory, and air pollution poses little or no risk</td>
  <td>None</td>
  </tr>
  <tr>
  <td>51 -100</td>
  <td>Moderate</td>
  <td>Air quality is acceptable; however, for some pollutants there may be a moderate health concern for a very small number of people who are unusually sensitive to air pollution.</td>
  <td>Active children and adults, and people with respiratory disease, such as asthma, should limit prolonged outdoor exertion.</td>
  </tr>
  <tr>
  <td>101-150</td>
  <td>Unhealthy for Sensitive Groups</td>
  <td>Members of sensitive groups may experience health effects. The general public is not likely to be affected.</td>
  <td>Active children and adults, and people with respiratory disease, such as asthma, should limit prolonged outdoor exertion.</td>
  </tr>
  <tr>
  <td>151-200</td>
  <td>Unhealthy</td>
  <td>Everyone may begin to experience health effects; members of sensitive groups may experience more serious health effects</td>
  <td>Active children and adults, and people with respiratory disease, such as asthma, should avoid prolonged outdoor exertion; everyone else, especially children, should limit prolonged outdoor exertion</td>
  </tr>
  <tr>
  <td>201-300</td>
  <td>Very Unhealthy</td>
  <td>Health warnings of emergency conditions. The entire population is more likely to be affected.</td>
  <td>Active children and adults, and people with respiratory disease, such as asthma, should avoid all outdoor exertion; everyone else, especially children, should limit outdoor exertion.</td>
  </tr>
  <tr>
  <td>300+</td>
  <td>Hazardous</td>
  <td>Health alert: everyone may experience more serious health effects</td>
  <td>Everyone should avoid all outdoor exertion</td>
  </tr>
  </table></p>
  
  "
    )
  }) 
  
  output$explain_text_5 = renderUI({
    HTML(
    "<p><b>Variables That Affect Sunrise And Sunset Times Where You Live:</b></p>
     <p><b>Longitude</b></p>
     <p>Longitude lines are the imaginary lines that run vertically around the globe, intersecting at the North and South Poles. Longitude is a way of talking about how far east or west a location is in relationship to one specific imaginary line, called the Prime Meridian, that runs through the town of Greenwich, England. Because the Earth spins clockwise, the Sun appears to the east of any given location each day. It takes about one hour for the Sun’s light to move 15° longitude. This is why the Sun’s rays reach the East Coast each day before the West Coast.</p>
     <p><b>Time Zone</b></p>
     <p>Time zones were created to acknowledge the fact that the Sun is not “up” at the same time everywhere around the globe. Because the Sun’s light moves across the Earth at a rate of 15° longitude per hour, people divided the Earth into 24 roughly equal sections of about 15° each, and assigned a one-hour time difference to each successive zone. This means that, all other things being equal, the Sun rises at roughly the same time no matter which time zone you’re in. Unfortunately, it’s not quite that simple. First of all, the many time zones don’t line up perfectly with the longitude lines, due to political and other complications. In addition, not all of us live right at the easternmost edge of our respective time zones. The further west you live within your time zone, the later your sunrise will be. There is a simple, but somewhat inexact, way to figure this out. For every 70 miles you travel west within your time zone, sunrise will be about four minutes later. This time decreases somewhat the further you move from the Equator, though. That’s because of latitude …</p>
     <p><b>Latitude</b></p>
     <p>Latitude lines are imaginary lines that encircle the globe horizontally. Latitude is a way of talking about how far north or south a location is in relationship to the Equator, the imaginary line that runs around the center of the globe. Unlike longitude lines, latitude lines never intersect. Because the Earth’s circumference is smaller near its poles than it is around its center, extreme northern or southern locations experience greater variations in the length of their days than locations closer to the Equator. While equatorial locations get approximately 12 hours of daylight and darkness each day, year-round, areas closer to the North and South Poles can experience several months of constant sunlight or darkness at a stretch. Most North Americans live somewhere between these two regions.</p>
     <p><b>Altitude</b></p>
     <p>As if all of these factors weren’t enough, altitude also plays a role. Generally speaking, the higher your location is, the earlier the Sun will rise, and the later it will set, compared to when it would for the same location if it were at sea level. An easy way to determine what effect altitude has is to remember than sunrise will be one minute earlier for every mile of altitude, and that sunset will be later by the same amount.</p>
      "
    )
  }) 
  
  output$explain_text_6 = renderUI({
    HTML(
      "<p><b>Köppen–Geiger climate classification</b></p>
      <p>The <b>Köppen climate classification</b> is one of the most widely used climate classification systems. It was first published by German-Russian climatologist Wladimir Köppen (1846–1940) in 1884, with several later modifications by Köppen, notably in 1918 and 1936. Later, German climatologist Rudolf Geiger (1894–1981) introduced some changes to the classification system, which is thus sometimes called the <b>Köppen–Geiger climate classification</b>.</p>
      <p>The Köppen climate classification divides climates into five main climate groups, with each group being divided based on patterns of seasonal precipitation and temperature. The five main groups are A (tropical), B (arid), C (temperate), D (continental), and E (polar). Each group and subgroup is represented by a letter. All climates are assigned a main group (the first letter). All climates except for those in the E group are assigned a seasonal precipitation subgroup (the second letter). For example, Af indicates a tropical rainforest climate. The system assigns a temperature subgroup for all groups other than those in the A group, indicated by the third letter for climates in B, C, D, and the second letter for climates in E. For example, Cfb indicates an oceanic climate with warm summers as indicated by the ending b. Climates are classified based on specific criteria unique to each climate type.</p>
      <p>As Köppen designed the system based on his experience as a botanist, his main climate groups are based on the types of vegetation occurring in a given climate classification region. In addition to identifying climates, the system can be used to analyze ecosystem conditions and identify the main types of vegetation within climates. Due to its association with the plant life of a given region, the system is useful in predicting future changes of plant life within that region.</p>
      <p>The Köppen climate classification system was modified further within the Trewartha climate classification system in 1966 (revised in 1980). The Trewartha system sought to create a more refined middle latitude climate zone, which was one of the criticisms of the Köppen system (the climate group C was too general).</p>
      
      "
    )
  })
  
  output$explain_text_7 = renderUI({
    HTML(
      "<p><b>Rain Level</b></p>
      <ul>
      <li>Light rain — when the precipitation rate is < 2.5 mm (0.098 in) per hour / < 60.0 mm (2.352 in) per day</li>
      <li>Moderate rain — when the precipitation rate is between 2.5 mm (0.098 in) – 7.6 mm (0.30 in) or 10 mm (0.39 in) per hour / between 60 mm (2.352 in) – 182.4 mm (7.20 in) or 240 mm (9.36 in) per day </li>
      <li>Heavy rain — when the precipitation rate is > 7.6 mm (0.30 in) per hour, or between 10 mm (0.39 in) and 50 mm (2.0 in) per hour / > 182.4 mm (7.20 in) per day, or between 240 mm (9.36 in) and 1200 mm (48.0 in) per day </li>
      <li>Violent rain — when the precipitation rate is > 50 mm (2.0 in) per hour / > 1200 mm (48.0 in) per day </li>
      </ul>
       "
    )
  })
  
  output$reason_text_1 = renderUI({
    HTML(
      "
      <ol>
      <li>Because the Earth tilts on its axis, and because the 
earth is spherical the Sun’s rays strike the Earth at 
different angles.<ol><li>as a result, different parts of Earth receive 
different amounts of solar radiation</li></ol></li>
      <li>The <b style='color:red !important;'>tropics</b> receive the 
most solar radiation 
because the sun’
s rays 
strike almost directly. 
Temperatures in the 
tropics are warm year round.</li>
      <li>The <b style='color:red !important;'>temperate</b> zones have 
moderate conditions.</li>
      <li>The <b style='color:red !important;'>polar</b> zones receive the 
least radiation because the 
suns rays strike at a very 
low angle. Temperatures 
in polar regions are usually 
cold.
</li>
      </ol>
      "
    )
  })
  
  output$reason_text_2 = renderUI({
    HTML(
      "
      <ol>
      <li>Elevation is the height above sea level.</li>
      <li>On average, air temperature drops about 
6.5\u00b0C for every 1000 m of altitude.</li>
      <li>The higher the elevation, the colder the climate.</li>
      </ol>
      "
    )
  })
  
  output$reason_text_3 = renderUI({
    HTML(
      "
      <ol>
      <li>Climates often differ on either side of a mountain.</li>
      <li>As air rises over a mountain, it cools. As it cools, 
it condenses, and releases moisture (rain). This is 
called the <b style='color:red !important;'>windward</b> side.</li>
      <li>As the dry air flows over the mountain, it descends 
and warms, usually producing deserts. 
This is called the <b style='color:red !important;'>leeward</b> side.</li>
</ol>
<p>Deserts	
such	as	the	
Atacama	in	
Chile	are	
common	
on	leeward	
sides	of	
mountains.</p>
<p>The	dry	area	is	
Called	a	rain	shadow	
and	can	extend	for	
hundreds	of	km	
downwind	of	a	
mountain	range.</p>
      "
    )
  })
  
  output$reason_text_4 = renderUI({
    HTML(
      "
      <ol>
      <li>Land gains and loses heat much faster than water.</li>
      <li>The temperature of a large body of water can influence 
the temperature of the air above it.</li>
      <li>Based on other factors certain areas closer to large 
bodies of water may have a relatively small yearly 
temperature range.<ol><li>example: the California coast vs. the interior of 
California</li></ol></li>
<li>Continental interiors have large yearly temperature 
ranges.</li>
<li>Dry air gains and loses heat much faster than humid 
air, so deserts have large daily temperature ranges.</li>
<li>Ocean currents can warm or cool the air above.</li>
<li>Ocean currents may be considerably warmer or colder 
than the normal air temperature for that latitude.</li>
</ol>
      "
    )
  })
  
  output$reason_text_5 = renderUI({
    HTML(
      "
      <ol>
      <li>Solar energy and earth’s rotation create motion in the 
atmosphere called planetary winds.</li>
      <li>There are three basic wind systems in each hemisphere: 
Polar Easterlies, Northeast or Southeast Tradewinds and 
Prevailing Westerlies.</li>
      <li>These winds blow air masses with distinct regions of 
origin (ie. formed over land or water, formed at certain 
latitudes).</li>
<li>Winds move warm air toward the poles and cool air 
toward the equator.</li>
</ol>
<p>These	belts	shift	
seasonally	as	the	earth	
spins	on	it’s	axis	and	
different	laAtudes	
receive	direct	sunlight.</p>
<p>In	the	Northern	
Hemisphere,	belts	
shift	northwards	in	the	
summer,	and	
southwards	in	the	
winter.		</p>
      "
    )
  })
  
  output$reason_text_6 = renderUI({
    HTML(
      "
      <ol>
      <li>Vegetation influences how much of the sun’
s energy 
is absorbed and how quickly this energy is released, 
which affects the climate.</li>
      <li>During <b style='color:red !important;'>transpiration</b>, plants release water vapor 
from their leaves into the air.</li>
      <li>Some plants release particles that promote the 
formation of clouds.</li>
<li>Large areas of vegetation mimic large bodies of 
water.</li>
</ol>
      "
    )
  })
  
  output$input_factors_ui <- renderUI({
    vec_list = list(
      c("Temperature","Metric"," (unit: \u00b0C)",-20,50,15,25,1),
      c("Temperature","Imperial"," (unit: \u00b0F)",-4,122,59,77,1),
      c("Humidity","Metric"," (unit: \u0025)",0,100,30,50,1),
      c("Humidity","Imperial"," (unit: \u0025)",0,100,30,50,1),
      c("Wind Speed","Metric"," (unit: kph)",0,80,1,10,1),
      c("Wind Speed","Imperial"," (unit: mph)",0,50,1,6,1),
      c("Cloud Cover","Metric"," (unit: \u0025)",0,100,10,20,1),
      c("Cloud Cover","Imperial"," (unit: \u0025)",0,100,10,20,1),
      c("Solar Radiation","Metric"," (unit: W/m2)",0,500,200,250,10),
      c("Solar Radiation","Imperial"," (unit: W/m2)",0,500,200,250,10),
      c("Snow","Metric"," (unit: cm)",0,150,0,5,1),
      c("Snow","Imperial"," (unit: inch)",0,60,0,2,1),
      c("Precipitation","Metric"," (unit: mm)",0,200,0,5,1),
      c("Precipitation","Imperial"," (unit: inch)",0,10,0,0.5,0.1)
    )
    df = data.frame(do.call(rbind, vec_list))
    colnames(df) = c("factor","unit_sys","unit","min","max","var1","var2","step")
    
    lapply(seq(length(input$factors)), function(i){
    unit = toString(df[df$factor==input$factors[[i]] & df$unit_sys==input$unit_2, ]["unit"])
    min = as.numeric(df[df$factor==input$factors[[i]] & df$unit_sys==input$unit_2, ]["min"])
    max = as.numeric(df[df$factor==input$factors[[i]] & df$unit_sys==input$unit_2, ]["max"])
    var1 = as.numeric(df[df$factor==input$factors[[i]] & df$unit_sys==input$unit_2, ]["var1"])
    var2 = as.numeric(df[df$factor==input$factors[[i]] & df$unit_sys==input$unit_2, ]["var2"])
    step = as.numeric(df[df$factor==input$factors[[i]] & df$unit_sys==input$unit_2, ]["step"])
    column(width=12,
          sliderInput(inputId = paste0("input_factor_val_", i),label = paste("Choose your prefered ",input$factors[[i]]," range in a day", unit, ":"), min=min, max=max, value=c(var1,var2), step = step)  
      )
    })
  })
  
  output$input_self_weights_ui <- renderUI({
    lapply(seq(length(input$factors)), function(i){
    column(width=2,
             numericInput(inputId = paste0("input_self_", i),label = paste("Weight of ", input$factors[[i]], "(\u0025) :"),value = 0)  
           )
     })
  })
  
  output$input_AHP_weights_ui <- renderUI({
  lapply(seq(length(input$factors)), function(j){
    column(width=2,
           lapply(seq(length(input$factors)),function(i){
             if (i < j){selectInput(inputId = paste0("input_AHP_", i, "_", j),
                          label = paste(input$factors[[i]],"Compared to", input$factors[[j]], ":"),
                          choices = c("(None)",
                                      paste(rep(input$factors[[i]],10), importance_vec, rep(input$factors[[j]],10)),
                                      paste(rep(input$factors[[j]],9), importance_vec[2:10], rep(input$factors[[i]],9))))
               } else {
                            verbatimTextOutput("")    
                          }
             })
          )
      })
  })
  
  df_city_ranking = eventReactive(input$button1, {
    if (input$weight_method == "Self-Defined Weights Vector"){
    val_1s = c()
    val_2s = c()
    weights = c()
    for (i in 1:length(input$factors)){
      val_1s = c(val_1s,input[[paste0("input_factor_val_", i)]][1])
      val_2s = c(val_2s,input[[paste0("input_factor_val_", i)]][2])
      weights = c(weights,input[[paste0("input_self_", i)]])
      }
    } else {
      val_1s = c()
      val_2s = c()
      AHP_calc_matrix = diag(length(input$factors))
      for (i in 1:length(input$factors)){
        val_1s = c(val_1s,input[[paste0("input_factor_val_", i)]][1])
        val_2s = c(val_2s,input[[paste0("input_factor_val_", i)]][2])
      }
      for (i in 1:length(input$factors)){
        for (j in 1:length(input$factors)){
          if (i<j){
            if (input[[paste0("input_AHP_", i, "_", j)]]!="(None)") {
              match_found <- FALSE
              for (word in as.vector(names(importance_dict))) {
                if (grepl(word, input[[paste0("input_AHP_", i, "_", j)]])) {
                  match_found <- TRUE
                  break
                }
              }
              factor1 = input$factors[i]
              factor2 = input$factors[j]
              factor1_pos = regexpr(factor1, input[[paste0("input_AHP_", i, "_", j)]])[1]
              factor2_pos = regexpr(factor2, input[[paste0("input_AHP_", i, "_", j)]])[1]
              val = importance_dict[[word]]
              if  (factor1_pos < factor2_pos){
                AHP_calc_matrix[i,j] = val
              } else {
                AHP_calc_matrix[i,j] = 1/val
              }
            }
          } else if (i>j) {
            if (input[[paste0("input_AHP_", j, "_", i)]]!="(None)") {
              match_found <- FALSE
              for (word in as.vector(names(importance_dict))) {
                if (grepl(word, input[[paste0("input_AHP_", j, "_", i)]])) {
                  match_found <- TRUE
                  break
                }
              }
              factor1 = input$factors[j]
              factor2 = input$factors[i]
              factor1_pos = regexpr(factor1, input[[paste0("input_AHP_", j, "_", i)]])[1]
              factor2_pos = regexpr(factor2, input[[paste0("input_AHP_", j, "_", i)]])[1]
              val = importance_dict[[word]]
              if  (factor1_pos < factor2_pos){
                AHP_calc_matrix[i,j] = 1/val
              } else {
                AHP_calc_matrix[i,j] = val
              }
            }
          }
        }
      }
      
      max_eign_vec = tryCatch({
        eigen(AHP_calc_matrix)$vectors[, which.max(eigen(AHP_calc_matrix)$values)]
      }, error = function(e) {
        rep(1,length(input$factors))
      })
      max_eign_vec = abs(max_eign_vec)
      weights = round(max_eign_vec/sum(max_eign_vec)*100,3)
    }
    df = add_final_score(get_indices_df(input$factors,val_1s,val_2s), weights)
    df = df[rev(order(df[["Final Score"]])), ]
    df = cbind(ranking = 1:nrow(df), df)
    df
  })
  
  observeEvent(input$button1, {
  output$city_ranking = renderReactable(
    reactable(df_city_ranking(), searchable = TRUE,defaultPageSize = 10)
  )
  })
  
  output$AHP_matrix = renderReactable({
    AHP_show_matrix = diag(length(input$factors))
    for (i in 1:length(input$factors)){
      for (j in 1:length(input$factors)){
        if (i<j){
          if (input[[paste0("input_AHP_", i, "_", j)]]!="(None)") {
            match_found <- FALSE
            for (word in as.vector(names(importance_dict))) {
              if (grepl(word, input[[paste0("input_AHP_", i, "_", j)]])) {
                match_found <- TRUE
                break
              }
            }
            factor1 = input$factors[i]
            factor2 = input$factors[j]
            factor1_pos = regexpr(factor1, input[[paste0("input_AHP_", i, "_", j)]])[1]
            factor2_pos = regexpr(factor2, input[[paste0("input_AHP_", i, "_", j)]])[1]
            val = importance_dict[[word]]
            if  (factor1_pos < factor2_pos){
              AHP_show_matrix[i,j] = as.character(val)
            } else {
              AHP_show_matrix[i,j] = paste("1/",as.character(val),sep="")
            }
          }
        } else if (i>j) {
          if (input[[paste0("input_AHP_", j, "_", i)]]!="(None)") {
            match_found <- FALSE
            for (word in as.vector(names(importance_dict))) {
              if (grepl(word, input[[paste0("input_AHP_", j, "_", i)]])) {
                match_found <- TRUE
                break
              }
            }
            factor1 = input$factors[j]
            factor2 = input$factors[i]
            factor1_pos = regexpr(factor1, input[[paste0("input_AHP_", j, "_", i)]])[1]
            factor2_pos = regexpr(factor2, input[[paste0("input_AHP_", j, "_", i)]])[1]
            val = importance_dict[[word]]
            if  (factor1_pos < factor2_pos){
              AHP_show_matrix[i,j] = paste("1/",as.character(val),sep="")
            } else {
              AHP_show_matrix[i,j] = as.character(val)
            }
          }
        }
      }
    }
    AHP_show_matrix_df = data.frame(AHP_show_matrix)
    colnames(AHP_show_matrix_df) = input$factors
    rownames(AHP_show_matrix_df) = input$factors
    reactable(AHP_show_matrix_df)
  })
  
  output$test_df = renderReactable({
    AHP_calc_matrix = diag(length(input$factors))
    for (i in 1:length(input$factors)){
      for (j in 1:length(input$factors)){
        if (i<j){
          if (input[[paste0("input_AHP_", i, "_", j)]]!="(None)") {
            match_found <- FALSE
            for (word in as.vector(names(importance_dict))) {
              if (grepl(word, input[[paste0("input_AHP_", i, "_", j)]])) {
                match_found <- TRUE
                break
              }
            }
            factor1 = input$factors[i]
            factor2 = input$factors[j]
            factor1_pos = regexpr(factor1, input[[paste0("input_AHP_", i, "_", j)]])[1]
            factor2_pos = regexpr(factor2, input[[paste0("input_AHP_", i, "_", j)]])[1]
            val = importance_dict[[word]]
            if  (factor1_pos < factor2_pos){
              AHP_calc_matrix[i,j] = val
            } else {
              AHP_calc_matrix[i,j] = 1/val
            }
          }
        } else if (i>j) {
          if (input[[paste0("input_AHP_", j, "_", i)]]!="(None)") {
            match_found <- FALSE
            for (word in as.vector(names(importance_dict))) {
              if (grepl(word, input[[paste0("input_AHP_", j, "_", i)]])) {
                match_found <- TRUE
                break
              }
            }
            factor1 = input$factors[j]
            factor2 = input$factors[i]
            factor1_pos = regexpr(factor1, input[[paste0("input_AHP_", j, "_", i)]])[1]
            factor2_pos = regexpr(factor2, input[[paste0("input_AHP_", j, "_", i)]])[1]
            val = importance_dict[[word]]
            if  (factor1_pos < factor2_pos){
              AHP_calc_matrix[i,j] = 1/val
            } else {
              AHP_calc_matrix[i,j] = val
            }
          }
        }
      }
    }
    
    max_eign_vec = tryCatch({
      max_eigenval = max(abs(eigen(AHP_calc_matrix)$values))
    }, error = function(e) {
      rep(1,length(input$factors))
    })
    RI_vector = c(0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45,1.49,1.51)
    if (length(input$factors)<3){
      CI = " "
      RI = " "
      CR = " "
      pass = "Consistency test does not apply."
    } else {
    CI = (max_eigenval-length(input$factors))/(length(input$factors)-1)
    RI = RI_vector[length(input$factors)]
    CR = CI/RI
    if (CR < 0.1) {
      pass = "Pass the consistency test. Proceed to the next step."
    } else {
      pass = "Do not pass the consistency test. Please revise the AHP matrix."
    }
    }
    test_df = data.frame(
      "CI"=c(CI),
      "RI"=c(RI),
      "CR"=c(CR),
      "pass"=c(pass)
    )
    colnames(test_df)=c("Consistency Index","Random Index","Consistency Radio"," ")
    reactable(test_df)
  })
  
  output$AHP_weights = renderReactable({
    AHP_calc_matrix = diag(length(input$factors))
    for (i in 1:length(input$factors)){
      for (j in 1:length(input$factors)){
        if (i<j){
          if (input[[paste0("input_AHP_", i, "_", j)]]!="(None)") {
            match_found <- FALSE
            for (word in as.vector(names(importance_dict))) {
              if (grepl(word, input[[paste0("input_AHP_", i, "_", j)]])) {
                match_found <- TRUE
                break
              }
            }
            factor1 = input$factors[i]
            factor2 = input$factors[j]
            factor1_pos = regexpr(factor1, input[[paste0("input_AHP_", i, "_", j)]])[1]
            factor2_pos = regexpr(factor2, input[[paste0("input_AHP_", i, "_", j)]])[1]
            val = importance_dict[[word]]
            if  (factor1_pos < factor2_pos){
              AHP_calc_matrix[i,j] = val
            } else {
              AHP_calc_matrix[i,j] = 1/val
            }
          }
        } else if (i>j) {
          if (input[[paste0("input_AHP_", j, "_", i)]]!="(None)") {
            match_found <- FALSE
            for (word in as.vector(names(importance_dict))) {
              if (grepl(word, input[[paste0("input_AHP_", j, "_", i)]])) {
                match_found <- TRUE
                break
              }
            }
            factor1 = input$factors[j]
            factor2 = input$factors[i]
            factor1_pos = regexpr(factor1, input[[paste0("input_AHP_", j, "_", i)]])[1]
            factor2_pos = regexpr(factor2, input[[paste0("input_AHP_", j, "_", i)]])[1]
            val = importance_dict[[word]]
            if  (factor1_pos < factor2_pos){
              AHP_calc_matrix[i,j] = 1/val
            } else {
              AHP_calc_matrix[i,j] = val
            }
          }
        }
      }
    }
    
    max_eign_vec = tryCatch({
      eigen(AHP_calc_matrix)$vectors[, which.max(eigen(AHP_calc_matrix)$values)]
      }, error = function(e) {
        rep(1,length(input$factors))
      })
    max_eign_vec = abs(max_eign_vec)
    normalized_max_eign_vec = round(max_eign_vec/sum(max_eign_vec)*100,3)
    weight_df = as.data.frame(t(normalized_max_eign_vec))
    colnames(weight_df) = paste(input$factors,rep(" (\u0025)",length(input$factors)))
    rownames(weight_df) = c("Weight")
    reactable(weight_df)
  })
  
  output$self_weight_text = renderUI({
    HTML(
      "
      <p>
Weights should sum to 100. 
      </p>
      "
    )
  })
  
  output$AHP_weight_text = renderUI({
    HTML(
      "
      <p>
The AHP matrix should be close to a rank-one matrix, meaning the all rows and all columns should be proportional.
      </p>
      "
    )
  })
  
  df_21 <- reactive({
    lower_quantile = input$ranking_range[1]
    upper_quantile = input$ranking_range[2]
    lower_ranking = round(lower_quantile*nrow(df_city_ranking())/100,0)
    upper_ranking = round(upper_quantile*nrow(df_city_ranking())/100,0)
    df_chosen = df_city_ranking()[lower_ranking+1:upper_ranking,c("ranking","City")]
    merged_df <- merge(df_chosen, df, by.x = "City", by.y = "city_name")
    merged_df = merged_df[order(merged_df[["ranking"]]), ]
    merged_df
  })
  
  df_22 <- reactive({
    lower_quantile = input$ranking_range[1]
    upper_quantile = input$ranking_range[2]
    lower_ranking = round(lower_quantile*nrow(df_city_ranking())/100,0)
    upper_ranking = round(upper_quantile*nrow(df_city_ranking())/100,0)
    df_chosen = df_city_ranking()[lower_ranking+1:upper_ranking,c("ranking","City")]
    merged_df <- merge(df_chosen, df, by.x = "City", by.y = "city_name")
    merged_df = merged_df[order(merged_df[["ranking"]]), c("ranking","City","Country","mean_temp..Celsius.","mean_solarradiation..W.m2.",
                                                           "mean_humidity....","mean_windspeed..kph.","temp_std..Celsius.","mean_temp_range..Celsius.",
                                                           "mean_abs_temp_diff_1..Celsius.","precip_std..mm.","precip_sum..mm." ,"temp.precip.corr",
                                                           "humid.radiat.corr", "humid.precip.corr", "temp.radiat.corr","Color.Code")]
    colnames(merged_df) <- c("Ranking","City","Country/Region","Annual Mean Temp (\u00B0C)","Annual Mean Solarradiation (W/m2)",
                   "Annual Mean Humidity (\u0025)","Mean Windspeed (kph)","Annual Temp Std (\u00B0C)","Annual Mean Temp Range (\u00B0C)",
                   "Annual Mean Temp Diff (\u00B0C)","Precip Std (mm)","Precip Sum (mm)" ,"Temp Precip Corr",
                   "Humid Radiat Corr", "Humid Precip Corr", "Temp Radiat Corr","Color Code")
    merged_df    
  })
  
  df_23 <- reactive({
    lower_quantile = input$ranking_range[1]
    upper_quantile = input$ranking_range[2]
    lower_ranking = round(lower_quantile*nrow(df_city_ranking())/100,0)
    upper_ranking = round(upper_quantile*nrow(df_city_ranking())/100,0)
    df_chosen = df_city_ranking()[lower_ranking+1:upper_ranking,c("ranking","City")]
    merged_df <- merge(df_chosen, df, by.x = "City", by.y = "city_name")
    merged_df = merged_df[order(merged_df[["ranking"]]),
                          c("ranking","City","Country","First.Administration","lat",
                            "long","elevation..m.","dist_to_sea..km.","Climate.Names",
                            "Koppen.climate","Group","Precipitation.Type","Level.of.Heat","Color.Code")]
    colnames(merged_df) <- c("Ranking","City","Country/Region","First-Level Administrative Division","Latitude (\u00B0)",
                             "Longitude (\u00B0)","Elevation (m)","Distance to the Sea (km)","Koppen Climate",
                             "Climate Code","Group","Precipitation Type","Level of Heat","Color Code")
    merged_df    
  })
  
  output$map_4 <- renderLeaflet({
    df = df_21()
    popup = paste0("<strong>City:</strong> ", df$City, "<br>",
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
                       radius = 6,color = df$Color.Code,
                       stroke = FALSE, fillOpacity = 0.8
      )
  })
  
  output$df_22 = renderReactable({
    color_vec = df_22()[["Color Code"]]
    reactable(df_22()[,-ncol(df_22())], searchable = TRUE, defaultPageSize = 10, 
              rowStyle = function(index) {
                list(background = color_vec[index],
                     color = contrast_color(color_vec[index])
                )
              }
    )
  })
  
  output$df_23 = renderReactable({
    color_vec = df_23()[["Color Code"]]
    reactable(df_23()[,-ncol(df_23())], searchable = TRUE, defaultPageSize = 10, 
              rowStyle = function(index) {
                list(background = color_vec[index],
                     color = contrast_color(color_vec[index])
                )
              }
    )
  })
  
}





