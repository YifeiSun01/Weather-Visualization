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
library(chron)
library(ggplot2)
library(viridis)
library(scales)
library(tidyverse)
library(cowplot)
library(ggforce)
library(comprehenr)
library(dplyr)
library(stringr)
library(LaplacesDemon)
library(data.table)
library(lubridate)
library(geomtextpath)
library(shinyBS)
library(reactable)

options(warn=-1)

weather_files_path = "data/weather data"
air_files_path = "data/air quality data"

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
climate_code_dict <- setNames(as.list(df$Koppen.climate), paste(df$Climate.Names, ", ", df$Koppen.climate))
climate_zone_dict <- setNames(as.list(c("Köppen-Geiger_Climate_Classification_Map.png",
                                        "Koppen-Geiger_Map_A_present.png",
                                        "Koppen-Geiger_Map_B_present.png",
                                        "Koppen-Geiger_Map_C_present.png",
                                        "Koppen-Geiger_Map_D_present.png",
                                        "Koppen-Geiger_Map_E_present.png")), 
                              as.list(c("(All)","A, Tropical","B, Arid","C, Temperate","D, Cold","E, Polar")))
climate_color_dict <- setNames(as.list(df$Color.Code), 
                              as.list(df$Koppen.climate))

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

df_index = read.csv("data/Climate 789.csv", row.names = 1)
# df_descriptions = read.csv("data/Climate Descriptions.csv", row.names = 1, fileEncoding="RFC-4180")
colnames(df_index) = c("Climate","1st","2nd","3rd","Climate Name","Description")
df_wind = read.csv("data/wind english.csv", header = TRUE, check.names = FALSE, row.names = 1)

importance_vec = c(" is equally important as ", 
                   " is slightly more important than ",
                   " is somewhat more important than ",
                   " is moderately more important than ",
                   " is fairly more important than ",
                   " is strongly more important than ",
                   " is considerably more important than ",
                   " is significantly more important than ",
                   " is substantially more important than ",
                   " is extremely more important than ")

plot_weather_precipitation_return = function(filname, unit){
  df = read.csv(filname)
  df$datetime=as.Date(df$datetime, origin = "2022-01-01")
  weather_conditions = c("Clear","Partially cloudy","Overcast","Rain","Snow","Ice",
                         "Freezing Drizzle/Freezing Rain")
  df[weather_conditions] = 0
  for (i in 1:nrow(df)){
    for (j in weather_conditions){
      if (j %in% unlist(strsplit(df$conditions[i],", "))){
        df[[j]][i] = 1
      } 
    }
  }
  
  city_name = str_to_title(sub(".*/", "", gsub(" *[0-9].*$", "", filname)))
  
  options(warn=-1)
  
  lower_y = -50
  upper_y = 50
  my_colors = rev(c("red","red","#ff2800","#ff4500", "orange",
                    "#ffc100", "yellow","#9ACD32", 
                    "#6aa121","#52b2bf", "#4683b7","#0047ab", 
                    "#00004d","#192841","#192841","#000137","#000137","#152238","#00008B"))
  interval_1 = 19
  my_values = (logit(seq(0.1,0.9,0.8/interval_1))-min(logit(seq(0.1,0.9,0.8/interval_1))))/(max(logit(seq(0.1,0.9,0.8/interval_1)))-min(logit(seq(0.1,0.9,0.8/interval_1))))
  my_breaks <- seq(1, 10, length.out = length(my_colors) + 1)
  
  myAngle <-seq(0,-360,length.out = 13)
  options(repr.plot.width = 20, repr.plot.height = 20)
  opacity_1 = 0.35
  opacity_2 = 0.3
  offset_2 = -30
  ratio_3 = 0.85
  temp_size = 1.5
  
  p = ggplot(df,aes(datetime, origin="2022-01-01"))+
    geom_ribbon(aes(ymin=0, ymax=4*(df$snowdepth)^(0.5)), fill="#601EF9", col="#601EF9", alpha=0.15)+
    geom_point(y=0.7*(df$temp-lower_y)+lower_y-10,shape=19,size=3*(df$precip)^(1/2),colour="#618db8",alpha=0.25,fill="#ADD8E6")+
    geom_point(y=0.7*(df$temp-lower_y)+lower_y-10,shape=19,size=5*(df$snow)^(1/2),colour="#7953a9",alpha=0.25,fill="#acace6")+
    geom_point(y=45,shape=19,size=0.035*df$solarradiation,colour="#FFAE42",alpha=0.27,fill="#FFCC33")+
    geom_point(y=55,shape=19,size=0.05*250*(df$humidity/100)^4,colour="#ADD8E6",alpha=0.27,fill="#ADD8E6")+
    geom_linerange(aes(datetime, ymin=offset_2, ymax=df$precip*ratio_3+offset_2), fill="#0047ab", col="#00008B", size = 1.5,alpha = opacity_1*df[["Rain"]])+
    geom_linerange(aes(datetime, ymin=offset_2, ymax=df$precip*ratio_3+offset_2), fill="#7034fa", col="#301934", size = 1.5,alpha = opacity_1*df[["Snow"]])+
    geom_linerange(aes(datetime, ymin=offset_2, ymax=df$precip*ratio_3+offset_2), fill="#330044", col="#5A5A5A", size = 1.5,alpha = opacity_2*df[["Freezing Drizzle/Freezing Rain"]])+
    geom_linerange(aes(datetime, ymin = feelslikemin, ymax = feelslikemax, color = ((feelslike-lower_y-10)/(upper_y-3-lower_y-10))), size = temp_size, alpha = 0.5) +
    geom_linerange(aes(datetime, ymin = tempmin, ymax = tempmax, color = ((temp-lower_y-10)/(upper_y-3-lower_y-10))), size = temp_size, alpha = 1) +
    scale_color_gradientn(colors=my_colors, values=my_values, limits=c(0,1)) + 
    scale_x_date(labels = date_format("%B"), breaks = date_breaks("month")) + 
    ylim(lower_y, upper_y+10) + 
    coord_curvedpolar() +
    theme_light() +
    labs(
      title = paste(city_name,"Weather Radial 2022"),
      subtitle = " ",
      caption = " ",
      x = NULL,
      y = NULL, 
      size = 30
    )+
    theme(plot.title = element_text(hjust = 0.5, size = 25)) +
    theme(axis.text.x=element_text(margin = margin(t = 0.1), size = 17,colour="#18191a"))+
    theme( # remove the vertical grid lines
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size=.5, color="lightgray"),
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line(size=.6, color="gray"),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )+
    scale_y_continuous(limits = c(lower_y, upper_y),
                       minor_breaks = seq(lower_y+20, upper_y-10, by = 10),
                       breaks = seq(lower_y+10, upper_y, by = 10))+
    theme(legend.position = "none")+
    theme(legend.key.height= unit(0.3, 'cm'),
          legend.key.width= unit(3, 'cm'))
  
  ratio_1 = 0.3665
  ratio_2 = 0.001
  
  time=9
  step=0.03648
  
  x_cen = 0.502
  y_cen = 0.4874
  
  block_step = 0.04
  block_step_1 = 0.04
  interval=0.005
  start_y = 0.724
  start_x=0.06
  start_x_1=0.06
  start_y_1=0.07
  r = 0.015
  if (unit=="Metric"){
    temp_list = c("-40","-30","-20","-10","0","10","20","30","40","50")
    temp_label = c("\u00B0C")
    unit_name = "Celsius"} else{
      temp_list = c("-40","-22","-4","14","32","50","68","86","104","122")
      temp_label = c("\u00B0F") 
      unit_name = "Fahrenheit"
    }
  
  image = ggdraw(p) +
    geom_text(
      data = data.frame(x = x_cen,
                        y = y_cen-0.008, label = city_name),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 30/.pt,
      color = "#333333",
      inherit.aes = FALSE
    )+
    geom_circle(aes(x0 = x_cen, y0 = y_cen, r = 0.183),alpha=0.13, fill="#B7C9E2", color = "#6699CC",size=0.5,inherit.aes = FALSE)+
    geom_circle(aes(x0 = x_cen, y0 = y_cen, r = 0.183*(-lower_y+offset_2)/(-lower_y)),alpha=0.0, fill="#B7C9E2", color = "#0047ab",size=0.5,inherit.aes = FALSE)+ 
    geom_segment(aes(
      x = x_cen+ratio_1*sin(2*pi*as.numeric(df$datetime-df$datetime[1])/360), 
      y = y_cen+ratio_1*cos(2*pi*as.numeric(df$datetime-df$datetime[1])/360), 
      xend = x_cen+ratio_1*sin(2*pi*as.numeric(df$datetime-df$datetime[1])/360)+ratio_2*cos(pi*(df$winddir+180)/180)*df$windspeed, 
      yend = y_cen+ratio_1*cos(2*pi*as.numeric(df$datetime-df$datetime[1])/360)+ratio_2*sin(pi*(df$winddir+180)/180)*df$windspeed),
      arrow = arrow(length = unit(0.07, "cm")),alpha=1.3*(df$windspeed/50)^(1.9),color="#30106b",size=1.3*(df$windspeed/50)^(1/4))+ 
    geom_segment(aes(
      x = x_cen, 
      y = y_cen+0.019, 
      xend = x_cen, 
      yend = y_cen+0.385),arrow = arrow(length = unit(0.07, "cm")),alpha=0.7,color="#5A5A5A",size = 1)+
    geom_text(
      data = data.frame(x = x_cen-0.013+rep(0,10),
                        y = y_cen+0.0315+to_vec(for(i in 0:time) i*step), label = temp_list),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen+0.014,
                        y = y_cen+0.0315+time*step, label = temp_label),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen+0.014,
                        y = y_cen+0.0315+(-lower_y+offset_2/10)*step, label = c("0mm")),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 10/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = start_x+0.03+rep(0,6),
                        y = start_y+to_vec(for(i in 1:6) i*block_step),
                        label = c("Precipitation (mm)","Snow","Solar Radiation (W/m2)","Humidity (\u0025)","Snow Depth (cm)", "Wind (kph)")),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 12/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=1
    )+
    geom_circle(aes(x0 = start_x, y0 = start_y+block_step, r = 0.015), shape=19, size=0.1, colour="#618db8", alpha=0.45, fill="#ADD8E6")+
    geom_circle(aes(x0 = start_x, y0 = start_y+2*block_step, r = 0.015), shape=19, size=0.1, colour="#7953a9", alpha=0.45, fill="#acace6")+
    geom_circle(aes(x0 = start_x, y0 = start_y+3*block_step, r = 0.015), shape=19, size=0.1, colour="#FFAE42", alpha=0.5, fill="#FFCC33")+
    geom_circle(aes(x0 = start_x, y0 = start_y+4*block_step, r = 0.015), shape=19, size=0.1, colour="#ADD8E6", alpha=0.5, fill="#ADD8E6")+
    geom_ribbon(aes(x=c(start_x-5*interval,start_x+5*interval),ymin=start_y+4.6*block_step, ymax=start_y+5.4*block_step), fill="#601EF9", col="#601EF9", alpha=0.15)+ 
    geom_segment(aes(
      x = start_x-0.02, 
      y = start_y+6*block_step-0.02, 
      xend = start_x+ratio_1*sin(pi/4)*0.1, 
      yend = start_y+6*block_step+ratio_1*cos(pi/4)*0.1),
      arrow = arrow(length = unit(0.07, "cm")),
      alpha=0.4,color="#30106b",size=1)+
    geom_text(
      data = data.frame(x = 0.04,
                        y = 0.05,
                        label = c(paste("Daily temperatures are shown as the color of the bars, \n the length of the bar represents the range of the temperature on that day, \n the opaque part of the bar shows the feels-like temperature on that day. \n Temperature Unit: ",unit_name))),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 10/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=1
    )+
    geom_text(
      data = data.frame(x = start_x_1+0.03+rep(0,6),
                        y = start_y_1+to_vec(for(i in 1:6) i*block_step_1),
                        label = c("Rain","Snow","Freezing Rain"," "," ", " ")),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 12/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=1
    )+
    geom_linerange(aes(x = start_x_1, ymin = start_y_1+block_step_1-r, ymax = start_y_1+block_step_1+r), colour="#0047ab", alpha=0.45, size=15)+
    geom_linerange(aes(x = start_x_1, ymin = start_y_1+2*block_step_1-r, ymax = start_y_1+2*block_step_1+r), colour="#7034fa", alpha=0.45, size=15)+
    geom_linerange(aes(x = start_x_1, ymin = start_y_1+3*block_step_1-r, ymax = start_y_1+3*block_step_1+r), colour="#330044", alpha=0.45, size=15)
  return (image)
}

## -------------------------------------------------------------------------------------------------------------


plot_precipitation_snow_return = function(filname, unit){
  df = read.csv(filname)
  df$datetime=as.Date(df$datetime, origin = "2022-01-01")
  weather_conditions = c("Clear","Partially cloudy","Overcast","Rain","Snow","Ice",
                         "Freezing Drizzle/Freezing Rain")
  df[weather_conditions] = 0
  for (i in 1:nrow(df)){
    for (j in weather_conditions){
      if (j %in% unlist(strsplit(df$conditions[i],", "))){
        df[[j]][i] = 1
      } 
    }
  }
  
  city_name = str_to_title(sub(".*/", "", gsub(" *[0-9].*$", "", filname)))
  
  options(warn=-1)
  
  lower_y = -50
  upper_y = 50
  my_colors = rev(c("red","red","#ff2800","#ff4500", "orange",
                    "#ffc100", "yellow","#9ACD32", 
                    "#6aa121","#52b2bf", "#4683b7","#0047ab", 
                    "#00004d","#192841","#192841","#000137","#000137","#152238","#00008B"))
  interval_1 = 19
  my_values = (logit(seq(0.1,0.9,0.8/interval_1))-min(logit(seq(0.1,0.9,0.8/interval_1))))/(max(logit(seq(0.1,0.9,0.8/interval_1)))-min(logit(seq(0.1,0.9,0.8/interval_1))))
  my_breaks <- seq(1, 10, length.out = length(my_colors) + 1)
  
  myAngle <-seq(0,-360,length.out = 13)
  options(repr.plot.width = 20, repr.plot.height = 20)
  opacity_1 = 0.9
  opacity_2 = 0.3
  offset_2 = -30
  ratio_3 = 1
  temp_size = 1.5
  snow_offset = -30
  
  p = ggplot(df,aes(datetime, origin="2022-01-01"))+
    geom_linerange(aes(datetime, ymin = feelslikemin, ymax = feelslikemax, color = ((feelslike-lower_y-10)/(upper_y-3-lower_y-10))), size = temp_size, alpha = 0.3) +
    geom_linerange(aes(datetime, ymin = tempmin, ymax = tempmax, color = ((temp-lower_y-10)/(upper_y-3-lower_y-10))), size = temp_size, alpha = 0.4) +
    scale_color_gradientn(colors=my_colors, values=my_values, limits=c(0,1)) + 
    geom_linerange(aes(datetime, ymin=offset_2, ymax=df$precip*ratio_3+offset_2), col="#00008B", size = 1.5,alpha = opacity_1*df[["Rain"]])+
    geom_linerange(aes(datetime, ymin=offset_2, ymax=df$precip*ratio_3+offset_2), col="#301934", size = 1.5,alpha = opacity_1*df[["Snow"]])+
    geom_linerange(aes(datetime, ymin=offset_2, ymax=df$precip*ratio_3+offset_2), col="#5A5A5A", size = 1.5,alpha = opacity_2*df[["Freezing Drizzle/Freezing Rain"]])+
    geom_ribbon(aes(ymin=snow_offset, ymax=df$snowdepth/2+snow_offset), fill="#601EF9", col="#601EF9", alpha=0.15,size=1)+
    scale_x_date(labels = date_format("%B"), breaks = date_breaks("month")) + 
    ylim(lower_y, upper_y+10) + 
    coord_curvedpolar() +
    theme_light() +
    labs(
      title = paste(city_name,"Weather Radial 2022"),
      subtitle = " ",
      caption = " ",
      x = NULL,
      y = NULL, 
      size = 30
    )+
    theme(plot.title = element_text(hjust = 0.5, size = 25)) +
    theme(axis.text.x=element_text(margin = margin(t = 0.1), size = 17,colour="#18191a"))+
    theme( # remove the vertical grid lines
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size=.5, color="lightgray"),
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line(size=.6, color="gray"),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )+
    scale_y_continuous(limits = c(lower_y, upper_y),
                       minor_breaks = seq(lower_y+20, upper_y-10, by = 10),
                       breaks = seq(lower_y+10, upper_y, by = 10))+
    theme(legend.position = "none")+
    theme(legend.key.height= unit(0.3, 'cm'),
          legend.key.width= unit(3, 'cm'))
  
  ratio_1 = 0.3665
  ratio_2 = 0.001
  
  time=9
  step=0.03648
  
  x_cen = 0.502
  y_cen = 0.4874
  
  block_step = 0.04
  block_step_1 = 0.04
  interval=0.005
  start_y = 0.724
  start_x=0.06
  start_x_1=0.06
  start_y_1=0.07
  r = 0.015
  if (unit=="Metric"){
    precip_list = c(0,10,20,30,40,50,60,70,80,90)+20+offset_2
    precip_list_str = ifelse(precip_list>=0,as.character(precip_list),"")
    precip_label = c("mm")
    precip_unit_name = "Millimeter"
    snow_list = c(0,20,40,60,80,100,120,140,160,180)+10+snow_offset
    snow_list_str = ifelse(snow_list>=0,as.character(snow_list),"")
    snow_label = c("cm")
    snow_unit_name = "Centimeter"} else{
      precip_list = round((c(0,10,20,30,40,50,60,70,80,90)+20+offset_2)*0.0393701,1)
      precip_list_str = ifelse(precip_list>=0,as.character(precip_list),"")
      precip_label = c("inch \n(precip)") 
      precip_unit_name = "Inch"
      snow_list = round((c(0,20,40,60,80,100,120,140,160,180)+10+snow_offset)*0.393701,0)
      snow_list_str = ifelse(snow_list>=0,as.character(snow_list),"")
      snow_label = c("inch \n(snow)")
      snow_unit_name = "Inch"
    }
  
  image = ggdraw(p) +
    geom_text(
      data = data.frame(x = x_cen,
                        y = y_cen-0.008, label = city_name),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 30/.pt,
      color = "#333333",
      inherit.aes = FALSE
    )+
    geom_circle(aes(x0 = x_cen, y0 = y_cen, r = 0.183),alpha=0.05, fill="#B7C9E2", color = "#6699CC",size=0.5,inherit.aes = FALSE)+
    geom_circle(aes(x0 = x_cen, y0 = y_cen, r = 0.183*(-lower_y+offset_2)/(-lower_y)),alpha=0.0, fill="#B7C9E2", color = "#0047ab",size=0.5,inherit.aes = FALSE)+ 
    geom_segment(aes(
      x = x_cen, 
      y = y_cen+0.019, 
      xend = x_cen, 
      yend = y_cen+0.385),arrow = arrow(length = unit(0.07, "cm")),alpha=0.7,color="#5A5A5A",size = 1)+
    geom_text(
      data = data.frame(x = x_cen-0.013+rep(0,10),
                        y = y_cen-0.039-to_vec(for(i in 0:time) i*step), label = precip_list_str),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen-0.013+rep(0,10),
                        y = y_cen+0.034+to_vec(for(i in 0:time) i*step), label = snow_list_str),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen+0.019,
                        y = y_cen-0.042-time*step, label = precip_label),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen+0.019,
                        y = y_cen+0.0315+time*step, label = snow_label),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen+0.014,
                        y = y_cen+0.0315+(-lower_y+offset_2/10)*step, label = c("0mm")),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 10/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = start_x+0.03+rep(0,6),
                        y = start_y+to_vec(for(i in 1:6) i*block_step),
                        label = c(" "," "," "," ",paste("Snow Depth","(",strsplit(snow_label[1]," \n")[[1]][1],")"), " ")),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 12/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=1
    )+
    geom_ribbon(aes(x=c(start_x-5*interval,start_x+5*interval),ymin=start_y+4.6*block_step, ymax=start_y+5.4*block_step), fill="#601EF9", col="#601EF9", alpha=0.15)+ 
    geom_text(
      data = data.frame(x = start_x_1+0.03+rep(0,6),
                        y = start_y_1+to_vec(for(i in 1:6) i*block_step_1),
                        label = c(paste("Rain","(",strsplit(precip_label[1]," \n")[[1]][1],")"),
                                  paste("Snow","(",strsplit(precip_label[1]," \n")[[1]][1],")"),
                                  paste("Freezing Rain","(",strsplit(precip_label[1]," \n")[[1]][1],")")
                                  ," "," ", " ")),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 12/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=1
    )+
    geom_linerange(aes(x = start_x_1, ymin = start_y_1+block_step_1-r, ymax = start_y_1+block_step_1+r), colour="#0047ab", alpha=0.45, size=15)+
    geom_linerange(aes(x = start_x_1, ymin = start_y_1+2*block_step_1-r, ymax = start_y_1+2*block_step_1+r), colour="#7034fa", alpha=0.45, size=15)+
    geom_linerange(aes(x = start_x_1, ymin = start_y_1+3*block_step_1-r, ymax = start_y_1+3*block_step_1+r), colour="#330044", alpha=0.45, size=15)
  return (image)
}

## -------------------------------------------------------------------------------------------------------------


plot_wind_return = function(filname, unit){
  df = read.csv(filname)
  df$datetime=as.Date(df$datetime, origin = "2022-01-01")
  
  city_name = str_to_title(sub(".*/", "", gsub(" *[0-9].*$", "", filname)))
  
  options(warn=-1)
  
  lower_y = -30
  upper_y = 110
  df_wind = read.csv("data/wind english.csv")

  myAngle <-seq(0,-360,length.out = 13)
  options(repr.plot.width = 20, repr.plot.height = 20)
  opacity_1 = 0.9
  opacity_2 = 0.5
  offset_2 = -30
  ratio_3 = 0.85
  temp_size = 1.5
  
  p = ggplot(df,aes(datetime, origin="2022-01-01"))+
    geom_ribbon(aes(datetime, ymin=0, ymax=df$windgust), fill = "#ADD8E6", color = "#00008B", size = 1, alpha = 0.2)+
    geom_ribbon(aes(datetime, ymin=0, ymax=df$windspeed), fill = "#ADD8E6", color = "#8A2BE2", size = 1, alpha = 0.3)+
    scale_x_date(labels = date_format("%B"), breaks = date_breaks("month")) + 
    ylim(lower_y, upper_y) + 
    coord_curvedpolar() +
    theme_light() +
    labs(
      title = paste(city_name,"Weather Radial 2022"),
      subtitle = " ",
      caption = " ",
      x = NULL,
      y = NULL, 
      size = 30
    )+
    theme(plot.title = element_text(hjust = 0.5, size = 25)) +
    theme(axis.text.x=element_text(margin = margin(t = 0.1), size = 17,colour="#18191a"))+
    theme( # remove the vertical grid lines
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size=.5, color="lightgray"),
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line(size=.6, color="gray"),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )+
    scale_y_continuous(limits = c(lower_y, upper_y),
                       minor_breaks = seq(lower_y+20, upper_y-10, by = 10),
                       breaks = seq(lower_y+10, upper_y, by = 10))+
    theme(legend.position = "none")+
    theme(legend.key.height= unit(0.3, 'cm'),
          legend.key.width= unit(3, 'cm'))
  
  ratio_1 = 0.3665
  ratio_2 = 0.03
  
  time=5
  step=0.053
  
  x_cen = 0.502
  y_cen = 0.4874
  
  block_step = 0.04
  block_step_1 = 0.04
  interval=0.005
  start_y = 0.724
  start_x=0.06
  start_x_1=0.06
  start_y_1=0.07
  r = 0.015
  if (unit=="Metric"){
    wind_list = c(0,20,40,60,80,100)
    wind_label = c("kph")
    wind_name = "(kph)"} else{
      wind_list = round(c(0,20,40,60,80,100)*0.621371,0)
      wind_label = c("mph")
      wind_name = "(mph)"
    }
  
  image = ggdraw(p) +
    geom_text(
      data = data.frame(x = x_cen,
                        y = y_cen-0.008, label = city_name),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 30/.pt,
      color = "#333333",
      inherit.aes = FALSE
    )+
    geom_segment(aes(
      x = x_cen+ratio_1*sin(2*pi*as.numeric(df$datetime-df$datetime[1])/360), 
      y = y_cen+ratio_1*cos(2*pi*as.numeric(df$datetime-df$datetime[1])/360), 
      xend = x_cen+ratio_1*sin(2*pi*as.numeric(df$datetime-df$datetime[1])/360)+ratio_2*cos(pi*(df$winddir+180)/180), 
      yend = y_cen+ratio_1*cos(2*pi*as.numeric(df$datetime-df$datetime[1])/360)+ratio_2*sin(pi*(df$winddir+180)/180)),
      arrow = arrow(length = unit(0.07, "cm")),alpha=1.3*(df$windspeed/50)^(1.9),color="#30106b",size=1.3*(df$windspeed/50)^(1/4))+ 
    geom_segment(aes(
      x = x_cen, 
      y = y_cen+0.019, 
      xend = x_cen, 
      yend = y_cen+0.385),arrow = arrow(length = unit(0.07, "cm")),alpha=0.7,color="#5A5A5A",size = 1)+
    geom_text(
      data = data.frame(x = x_cen-0.013+rep(0,6),
                        y = y_cen+0.078+to_vec(for(i in 0:time) i*step), label = wind_list),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen+0.014,
                        y = y_cen+0.078+time*step, label = wind_label),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = start_x+0.03+rep(0,6),
                        y = start_y+to_vec(for(i in 1:6) i*block_step),
                        label = c("","","",paste("Wind Speed",wind_name,sep=" "),paste("Wind Gust Speed",wind_name,sep=" "), "Wind Direction")),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 12/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=1
    )+
    geom_segment(aes(
      x = start_x-0.02, 
      y = start_y+6*block_step-0.02, 
      xend = start_x+ratio_1*sin(pi/4)*0.1, 
      yend = start_y+6*block_step+ratio_1*cos(pi/4)*0.1),
      arrow = arrow(length = unit(0.07, "cm")),
      alpha=0.4,color="#30106b",size=1)+
    geom_ribbon(aes(x=c(start_x-5*interval,start_x+5*interval),ymin=start_y+4.6*block_step, ymax=start_y+5.4*block_step), fill = "#ADD8E6", color = "#00008B", alpha=0.3, size=1)+ 
    geom_ribbon(aes(x=c(start_x-5*interval,start_x+5*interval),ymin=start_y+3.6*block_step, ymax=start_y+4.4*block_step), fill = "#ADD8E6", color = "#8A2BE2", alpha=0.3, size=1)
return (image)
}


## -------------------------------------------------------------------------------------------------------------


plot_humidity_return = function(filname, unit){
  df = read.csv(filname)
  df$datetime=as.Date(df$datetime, origin = "2022-01-01")
  
  city_name = str_to_title(sub(".*/", "", gsub(" *[0-9].*$", "", filname)))
  
  options(warn=-1)
  
  lower_y = -30
  upper_y = 110

  myAngle <-seq(0,-360,length.out = 13)
  options(repr.plot.width = 20, repr.plot.height = 20)
  opacity_1 = 0.9
  opacity_2 = 0.5
  offset_2 = -30
  ratio_3 = 0.85
  temp_size = 1.5
  
  p = ggplot(df,aes(datetime, origin="2022-01-01"))+
    geom_ribbon(aes(datetime, ymin=0, ymax=df$humidity), fill = "#ADD8E6", color = "#00008B", size = 1, alpha = 0.2)+
    scale_x_date(labels = date_format("%B"), breaks = date_breaks("month")) + 
    ylim(lower_y, upper_y) + 
    coord_curvedpolar() +
    theme_light() +
    labs(
      title = paste(city_name,"Weather Radial 2022"),
      subtitle = " ",
      caption = " ",
      x = NULL,
      y = NULL, 
      size = 30
    )+
    theme(plot.title = element_text(hjust = 0.5, size = 25)) +
    theme(axis.text.x=element_text(margin = margin(t = 0.1), size = 17,colour="#18191a"))+
    theme( # remove the vertical grid lines
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size=.5, color="lightgray"),
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line(size=.6, color="gray"),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )+
    scale_y_continuous(limits = c(lower_y, upper_y),
                       minor_breaks = seq(lower_y+20, upper_y-10, by = 10),
                       breaks = seq(lower_y+10, upper_y, by = 10))+
    theme(legend.position = "none")+
    theme(legend.key.height= unit(0.3, 'cm'),
          legend.key.width= unit(3, 'cm'))
  
  ratio_1 = 0.3665
  ratio_2 = 0.03
  
  time=5
  step=0.053
  
  x_cen = 0.502
  y_cen = 0.4874
  
  block_step = 0.04
  block_step_1 = 0.04
  interval=0.005
  start_y = 0.724
  start_x=0.06
  start_x_1=0.06
  start_y_1=0.07
  r = 0.015
  humidity_list = c(0,20,40,60,80,100)
  humidity_label = c("\u0025")
  
  image = ggdraw(p) +
    geom_text(
      data = data.frame(x = x_cen,
                        y = y_cen-0.008, label = city_name),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 30/.pt,
      color = "#333333",
      inherit.aes = FALSE
    )+
    geom_text(
      data = data.frame(x = x_cen-0.013+rep(0,6),
                        y = y_cen+0.078+to_vec(for(i in 0:time) i*step), label = humidity_list),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen+0.014,
                        y = y_cen+0.078+time*step, label = humidity_label),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = start_x+0.03+rep(0,6),
                        y = start_y+to_vec(for(i in 1:6) i*block_step),
                        label = c("","","","Relative Humidity (\u0025)","", "")),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 12/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=1
    )+
    geom_ribbon(aes(x=c(start_x-5*interval,start_x+5*interval),ymin=start_y+4.6*block_step, ymax=start_y+5.4*block_step), fill = "#ADD8E6", color = "#00008B", alpha=0.3, size=1)
  return (image)
}


plot_weather_micelaneous_return = function(filname, unit){
  df = read.csv(filname)
  df$datetime=as.Date(df$datetime, origin = "2022-01-01")
  weather_conditions = c("Clear","Partially cloudy","Overcast","Rain","Snow","Ice",
                         "Freezing Drizzle/Freezing Rain")
  df[weather_conditions] = 0
  for (i in 1:nrow(df)){
    for (j in weather_conditions){
      if (j %in% unlist(strsplit(df$conditions[i],", "))){
        df[[j]][i] = 1
      } 
    }
  }
  
  city_name = str_to_title(sub(".*/", "", gsub(" *[0-9].*$", "", filname)))
  
  options(warn=-1)
  
  lower_y = -50
  upper_y = 50
  my_colors = rev(c("red","red","#ff2800","#ff4500", "orange",
                    "#ffc100", "yellow","#9ACD32", 
                    "#6aa121","#52b2bf", "#4683b7","#0047ab", 
                    "#00004d","#192841","#192841","#000137","#000137","#152238","#00008B"))
  my_colors_2 = c("#fff997","#fcdd6b","#feb83f","#fd8b20","#fc6a0c","#ee2201")
  interval_1 = 19
  my_values = (logit(seq(0.1,0.9,0.8/interval_1))-min(logit(seq(0.1,0.9,0.8/interval_1))))/(max(logit(seq(0.1,0.9,0.8/interval_1)))-min(logit(seq(0.1,0.9,0.8/interval_1))))
  my_values_2 = seq(0,1,1/6)
  my_breaks <- seq(1, 10, length.out = length(my_colors) + 1)
  
  myAngle <-seq(0,-360,length.out = 13)
  options(repr.plot.width = 20, repr.plot.height = 20)
  opacity_1 = 0.35
  opacity_2 = 0.3
  offset_2 = -30
  ratio_3 = 0.85
  temp_size = 1.5
  
  p = ggplot(df,aes(datetime, origin="2022-01-01"))+
    geom_linerange(aes(datetime, ymin = feelslikemin, ymax = feelslikemax, color = ((feelslike-lower_y-10)/(upper_y-3-lower_y-10))), size = temp_size, alpha = 0.1) +
    geom_linerange(aes(datetime, ymin = tempmin, ymax = tempmax, color = ((temp-lower_y-10)/(upper_y-3-lower_y-10))), size = temp_size, alpha = 0.45) + scale_color_gradientn(colors=my_colors, values=my_values, limits=c(0,1)) +
    geom_ribbon(aes(datetime, ymin = dew-0.1, ymax = dew+0.1), color = "#48bf91", size = 1, alpha = 1) +
    geom_ribbon(aes(ymin=-50, ymax=(sealevelpressure-940)/2-50), fill="#601EF9", col="black", alpha=0.15,size=1)+
    geom_ribbon(aes(ymin=45-cloudcover/20, ymax=45+cloudcover/20), fill="#ADD8E6", col="#040273", alpha=0.25,size=1)+
    geom_ribbon(aes(ymin=35-solarenergy/5, ymax=35+solarenergy/5), fill="#ffdf00", col="#ed7014", alpha=0.25,size=1)+
    geom_ribbon(aes(ymin=25-visibility/5, ymax=25+visibility/5), fill="#be93d4", col="#300038", alpha=0.25,size=1)+
    geom_linerange(aes(ymin=-30, ymax=precip/5-30), fill="#acc8d7", col="#253f4b", alpha=0.65,size=2.5)+
    scale_x_date(labels = date_format("%B"), breaks = date_breaks("month")) + 
    ylim(lower_y, upper_y+10) + 
    coord_curvedpolar() +
    theme_light() +
    labs(
      title = paste(city_name,"Weather Radial 2022"),
      subtitle = " ",
      caption = " ",
      x = NULL,
      y = NULL, 
      size = 30
    )+
    theme(plot.title = element_text(hjust = 0.5, size = 25)) +
    theme(axis.text.x=element_text(margin = margin(t = 0.1), size = 17,colour="#18191a"))+
    theme( # remove the vertical grid lines
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size=.5, color="lightgray"),
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line(size=.6, color="gray"),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )+
    scale_y_continuous(limits = c(lower_y, upper_y),
                       minor_breaks = seq(lower_y+20, upper_y-10, by = 10),
                       breaks = seq(lower_y+10, upper_y, by = 10))+
    theme(legend.position = "none")+
    theme(legend.key.height= unit(0.3, 'cm'),
          legend.key.width= unit(3, 'cm'))
  
  pressure_list = c("960","980","1000","1020","1040","1060","1080","1100","1120","1140")
  pressure_label = c("Millibar")
  if (unit=="Metric"){
    temp_list = c("-40","-30","-20","-10","0","10","20","30","40","50")
    temp_label = c("\u00B0C")
    unit_name = "Celsius"
    degree="C"} else{
      temp_list = c("-40","-22","-4","14","32","50","68","86","104","122")
      temp_label = c("\u00B0F") 
      unit_name = "Fahrenheit"
      degree="F"
    }
  
  ratio_1 = 0.3665
  ratio_2 = 0.001
  
  time=9
  step=0.03648
  
  x_cen = 0.502
  y_cen = 0.4874
  
  block_step = 0.04
  block_step_1 = 0.03
  interval=0.005
  start_y = 0.8
  start_x=0.06
  start_x_1=0.06
  start_y_1=0.07
  r = 0.015
  offset_2 = 0.005
  start_x_3 = 0.77
  len_1 = 0.03
  
  image = ggdraw(p)+
    geom_text(
      data = data.frame(x = x_cen,
                        y = y_cen-0.008, label = city_name),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 30/.pt,
      color = "#333333",
      inherit.aes = FALSE
    )+
    geom_text(
      data = data.frame(x = x_cen-0.013+rep(0,10),
                        y = y_cen+0.0315+to_vec(for(i in 0:time) i*step), label = pressure_list),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen+0.03,
                        y = y_cen+0.0315+time*step, label = pressure_label),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen-0.013+rep(0,10),
                        y = y_cen-0.04+to_vec(for(i in 0:time) i*(-step)), label = temp_list),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = x_cen+0.014,
                        y = y_cen-0.04+time*(-step), label = temp_label),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_ribbon(aes(x=c(start_x_3,start_x_3+len_1), ymin = start_y-0.04+0.495*block_step_1, ymax = start_y-0.04+0.505*block_step_1-offset_2), color = "#48bf91",fill="#48bf91", size = 1, alpha = 1) +
    geom_ribbon(aes(x=c(start_x_3,start_x_3+len_1), ymin=start_y-0.04+1*block_step_1, ymax=start_y-0.04+2*block_step_1-offset_2), fill="#601EF9", col="black", alpha=0.15,size=1)+
    geom_ribbon(aes(x=c(start_x_3,start_x_3+len_1), ymin=start_y-0.04+2*block_step_1, ymax=start_y-0.04+3*block_step_1-offset_2), fill="#ADD8E6", col="#040273", alpha=0.25,size=1)+
    geom_ribbon(aes(x=c(start_x_3,start_x_3+len_1), ymin=start_y-0.04+3*block_step_1, ymax=start_y-0.04+4*block_step_1-offset_2), fill="#ffdf00", col="#ed7014", alpha=0.25,size=1)+
    geom_ribbon(aes(x=c(start_x_3,start_x_3+len_1), ymin=start_y-0.04+4*block_step_1, ymax=start_y-0.04+5*block_step_1-offset_2), fill="#be93d4", col="#300038", alpha=0.25,size=1)+
    geom_linerange(aes(x=c(start_x_3+len_1/2), ymin=start_y-0.04+5*block_step_1, ymax=start_y-0.04+6*block_step_1-offset_2), fill="#acc8d7", col="#253f4b", alpha=0.65,size=12)+
    geom_text(
      data = data.frame(x = 0.89,
                        y = start_y-0.032+c(0:5)*block_step_1, label = c(
                          paste("Dew Point (\u00b0",degree,")"), "Sea Level Pressure (Millibar)",
                          "Cloud Cover (\u0025)", "Solar Energy (MJ/m2)", "Visibility (km)","Precipitation (mm)"
                        )),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 13/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )
  return(image)
}


## -------------------------------------------------------------------------------------------------------------
plot_daytime_return=function(filname){
  df = read.csv(filname)
  
  options(warn=-1)
  city_name = str_to_title(sub(".*/", "", gsub(" *[0-9].*$", "", filname)))
  
  df$datetime=as.Date(df$datetime)
  df$sunrise=gsub('T','',df$sunrise)
  df$sunset=gsub('T','',df$sunset)
  df$sunrise=times(format(as.POSIXct(df$sunrise,format="%Y-%m-%d%H:%M:%S"), format = "%H:%M:%S"))
  df$sunset=times(format(as.POSIXct(df$sunset,format="%Y-%m-%d%H:%M:%S"), format = "%H:%M:%S"))
  weather_conditions = c("Clear","Partially cloudy","Overcast","Rain","Snow","Ice",
                         "Freezing Drizzle/Freezing Rain")
  df[weather_conditions] = 0
  for (i in 1:nrow(df)){
    for (j in weather_conditions){
      if (j %in% unlist(strsplit(df$conditions[i],", "))){
        df[[j]][i] = 1
      } 
    }
  }
  opacity_1=0.92
  opacity=0.95
  line_width_1=0.4
  line_width_2=0.4
  line_opicaty_1=1
  line_opicaty_2=0.01
  num_opacity = 1
  color_1 = "#ffffff"
  color_2 = "#545454"
  color_3 = "#949494"
  myAngle <-seq(0,-360,length.out = 13)
  num_offset = 1/50
  thick_line_width = 0.8
  thick_line_color = "#ADD8E6"
  
  convert_time=function(x) {
    list_1 = rep(0,length(x))
    for (i in 1:length(x)){
      a = as.numeric(unlist(strsplit(format(x[i]),":")))
      y = a[1]+a[2]/60+a[3]/3600
      list_1[i] = y
    }
    return (list_1)
  } 
  
  full_time = convert_time(times("23:59:59"))
  offset = 0.6
  num_size = 5
  
  list_time_str = to_vec(for (i in 0:24) as.character(i))
  
  list_time_ratio = convert_time(c(times("00:00:00"),times("01:00:00"),times("02:00:00"),times("03:00:00"),times("04:00:00"),times("05:00:00"),times("06:00:00"),times("07:00:00"),times("08:00:00"),times("09:00:00"),times("10:00:00"),times("11:00:00"),times("12:00:00"),times("13:00:00"),times("14:00:00"),times("15:00:00"),times("16:00:00"),times("17:00:00"),times("18:00:00"),times("19:00:00"),times("20:00:00"),times("21:00:00"),times("22:00:00"),times("23:00:00"),times("23:59:59")))/full_time+offset
  
  image = ggplot(df,aes(datetime, origin = "2022-01-01"))+
    geom_ribbon(aes(ymin=list_time_ratio[1], ymax=convert_time(df$sunrise)/full_time+offset), fill="#000000", col="#000000", alpha=0.95,size=1)+
    geom_ribbon(aes(ymin=convert_time(df$sunset+0.01)/full_time+offset, ymax=list_time_ratio[25]), fill="#000000", col="#000000", alpha=0.95,size=1)+
    geom_ribbon(aes(ymin=ifelse(is.na(df$sunrise),list_time_ratio[1],list_time_ratio[1]), ymax=ifelse(is.na(df$sunset),list_time_ratio[25],list_time_ratio[1])), fill="#000000", col="#000000", alpha=opacity_1,size=1)+
    
    geom_ribbon(aes(ymin=list_time_ratio[2], ymax=list_time_ratio[2]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[3], ymax=list_time_ratio[3]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[5], ymax=list_time_ratio[5]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[6], ymax=list_time_ratio[6]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[8], ymax=list_time_ratio[8]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[9], ymax=list_time_ratio[9]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[11], ymax=list_time_ratio[11]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[12], ymax=list_time_ratio[12]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[14], ymax=list_time_ratio[14]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[15], ymax=list_time_ratio[15]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[17], ymax=list_time_ratio[17]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[18], ymax=list_time_ratio[18]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[20], ymax=list_time_ratio[20]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[21], ymax=list_time_ratio[21]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[23], ymax=list_time_ratio[23]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    geom_ribbon(aes(ymin=list_time_ratio[24], ymax=list_time_ratio[24]), color = color_2, alpha=line_opicaty_2,size=line_width_2)+
    
    geom_ribbon(aes(ymin=convert_time(df$sunrise+0.01)/full_time+offset, ymax=ifelse(convert_time(df$sunset)<convert_time(df$sunrise),rep(1+offset,nrow(df)),convert_time(df$sunset)/full_time+offset)), fill="#FFCC33", col="#FFCC33", alpha=opacity,size=1)+
    geom_ribbon(aes(ymin=list_time_ratio[1], ymax=ifelse(convert_time(df$sunset)<convert_time(df$sunrise),convert_time(df$sunset)/full_time+offset,list_time_ratio[1])), fill="#FFCC33", col="#FFCC33", alpha=opacity,size=0.01)+
    geom_ribbon(aes(ymin=ifelse(convert_time(df$sunset)==convert_time(df$sunrise),list_time_ratio[1],list_time_ratio[1]), ymax=ifelse(convert_time(df$sunset)==convert_time(df$sunrise),list_time_ratio[25],list_time_ratio[1])), fill="#FFCC33", col="#FFCC33", alpha=opacity,size=0.01)+
    
    geom_ribbon(aes(ymin=list_time_ratio[1], ymax=list_time_ratio[1]), color = thick_line_color, alpha=line_opicaty_1,size=thick_line_width)+
    geom_ribbon(aes(ymin=list_time_ratio[4], ymax=list_time_ratio[4]), color = color_1, alpha=line_opicaty_1,size=line_width_1)+
    geom_ribbon(aes(ymin=list_time_ratio[7], ymax=list_time_ratio[7]), color = color_1, alpha=line_opicaty_1,size=line_width_1)+
    geom_ribbon(aes(ymin=list_time_ratio[10], ymax=list_time_ratio[10]), color = color_1, alpha=line_opicaty_1,size=line_width_1)+
    geom_ribbon(aes(ymin=list_time_ratio[13], ymax=list_time_ratio[13]), color = thick_line_color, alpha=line_opicaty_1,size=thick_line_width)+
    geom_ribbon(aes(ymin=list_time_ratio[16], ymax=list_time_ratio[16]), color = color_1, alpha=line_opicaty_1,size=line_width_1)+
    geom_ribbon(aes(ymin=list_time_ratio[19], ymax=list_time_ratio[19]), color = color_1, alpha=line_opicaty_1,size=line_width_1)+
    geom_ribbon(aes(ymin=list_time_ratio[22], ymax=list_time_ratio[22]), color = color_1, alpha=line_opicaty_1,size=line_width_1)+
    geom_ribbon(aes(ymin=list_time_ratio[25], ymax=list_time_ratio[25]), color = thick_line_color, alpha=line_opicaty_1,size=thick_line_width)+
    
    geom_text(data = data.frame(x = df$datetime[seq(1,365,91)[1:4]],
                                y = rep(list_time_ratio[1]+num_offset,4), label = rep(list_time_str[1],4)),
              aes(x,y, label=label), color = "#FFFFFF", alpha=num_opacity,size=num_size)+
    geom_text(data = data.frame(x = df$datetime[seq(1,365,91)[1:4]],
                                y = rep(list_time_ratio[4]+num_offset,4), label = rep(list_time_str[4],4)),
              aes(x,y, label=label), color = "#FFFFFF", alpha=num_opacity,size=num_size)+
    geom_text(data = data.frame(x = df$datetime[seq(1,365,91)[1:4]],
                                y = rep(list_time_ratio[7]+num_offset,4), label = rep(list_time_str[7],4)),
              aes(x,y, label=label), color = color_3, alpha=num_opacity,size=num_size)+
    geom_text(data = data.frame(x = df$datetime[seq(1,365,91)[1:4]],
                                y = rep(list_time_ratio[10]+num_offset,4), label = rep(list_time_str[10],4)),
              aes(x,y, label=label), color = color_3, alpha=num_opacity,size=num_size)+
    geom_text(data = data.frame(x = df$datetime[seq(1,365,91)[1:4]],
                                y = rep(list_time_ratio[13],4), label = rep(list_time_str[13],4)),
              aes(x,y, label=label), color = "#000000", alpha=num_opacity,size=num_size)+
    geom_text(data = data.frame(x = df$datetime[seq(1,365,91)[1:4]],
                                y = rep(list_time_ratio[16],4), label = rep(list_time_str[16],4)),
              aes(x,y, label=label), color = color_3, alpha=num_opacity,size=num_size)+
    geom_text(data = data.frame(x = df$datetime[seq(1,365,91)[1:4]],
                                y = rep(list_time_ratio[19],4), label = rep(list_time_str[19],4)),
              aes(x,y, label=label), color = color_3, alpha=num_opacity,size=num_size)+
    geom_text(data = data.frame(x = df$datetime[seq(1,365,91)[1:4]],
                                y = rep(list_time_ratio[22],4), label = rep(list_time_str[22],4)),
              aes(x,y, label=label), color = color_3, alpha=num_opacity,size=num_size)+
    geom_text(data = data.frame(x = df$datetime[seq(1,365,91)[1:4]],
                                y = rep(list_time_ratio[25],4), label = rep(list_time_str[25],4)),
              aes(x,y, label=label), color = color_3, alpha=num_opacity,size=num_size)+
    
    geom_text(
      data = data.frame(x = df$datetime[1],
                        y = 0, label = city_name),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 35/.pt,
      color = "#333333",
      inherit.aes = FALSE
    )+
    scale_x_date(labels = date_format("%B"), breaks = date_breaks("month"))+
    scale_y_time(limits= c(0,offset+1))+theme_bw()+
    theme( # remove the vertical grid lines
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(size=.5, color="lightgray"),
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line(size=.6, color="gray"),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )+ 
    coord_curvedpolar(theta="x",start=0)+
    theme(axis.text.x=element_text(margin = margin(t = 0.1), size = 20,colour="#18191a"))+
    labs(
      title = paste(city_name,"Daytime Radial 2022"),
      subtitle = " ",
      caption = " ",
      x = NULL,
      y = NULL, 
      size = 50
    )+
    theme(plot.title = element_text(hjust = 0.5, size = 25))
  
  step_1 = 0.02
  step_2 = 0.05
  block_opacity = 0.9
  block_size = 15
  
  image_2 = ggdraw(image)+
    geom_linerange(aes(x=0.82,ymin=0.80, ymax=0.80+step_2-0.02), color = "#000000", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=0.82,ymin=0.80+step_2, ymax=0.80+2*step_2-0.02), color = "#FFCC33", alpha=block_opacity,size=block_size)+
    geom_text(
      data = data.frame(x = 0.85+rep(0,1),
                        y = 0.813+to_vec(for(i in 0:1) i*step_2),
                        label = c("night time","day time")),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 12/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )
  return (image_2)
}


## -------------------------------------------------------------------------------------------------------------
plot_weather_condition_return=function(filname){
  options(warn=-1)
  
  df = read.csv(filname)
  city_name = str_to_title(sub(".*/", "", gsub(" *[0-9].*$", "", filname)))
  
  df$datetime=as.Date(df$datetime)
  weather_conditions = c("Clear","Partially cloudy","Overcast","Rain","Snow","Ice",
                         "Freezing Drizzle/Freezing Rain")
  df[weather_conditions] = 0
  for (i in 1:nrow(df)){
    for (j in weather_conditions){
      if (j %in% unlist(strsplit(df$conditions[i],", "))){
        df[[j]][i] = 1
      } 
    }
  }
  
  step_2 = 3
  myAngle <-seq(0,-360,length.out = 13)
  opacity_1=1
  opacity_2=0.7
  opacity=0.7
  size_ratio_1 = 8
  size_ratio_clear = 7.9
  size_ratio = 8.3
  size_ratio_ui = 8.5
  inner_radius = 10
  radius_list = to_vec(for (i in 0:6) i*step_2)+inner_radius
  uv_categories =  c("#FFFF00", "#FFEF00", "#FFDF00", "#FFCF00", "#FFBF00", "#FFAF00", "#FF9F00", "#FF8F00", "#FF7F00","#FF4500", "#FF0000")
  uv_breaks = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12)
  
  image = ggplot(df,aes(datetime))+
    
    geom_ribbon(aes(ymin=radius_list[7]-0.2, ymax=radius_list[7]-0.1), color = "#D3D3D3", alpha=1,size=1.5)+
    geom_ribbon(aes(ymin=radius_list[6]-0.2, ymax=radius_list[6]-0.1), color = "#D3D3D3", alpha=1,size=1.5)+
    geom_ribbon(aes(ymin=radius_list[5]-0.1, ymax=radius_list[5]+0.1), color = "#D3D3D3", alpha=1,size=1.5)+
    geom_ribbon(aes(ymin=radius_list[4]-0.1, ymax=radius_list[4]+0.1), color = "#D3D3D3", alpha=1,size=1.5)+
    geom_ribbon(aes(ymin=radius_list[3]-0.1, ymax=radius_list[3]+0.1), color = "#D3D3D3", alpha=0.2,size=1)+
    geom_ribbon(aes(ymin=radius_list[2]-0.1, ymax=radius_list[2]+0.1), color = "#D3D3D3", alpha=0.2,size=1)+
    geom_ribbon(aes(ymin=radius_list[1]-0.1, ymax=radius_list[1]+0.1), color = "#D3D3D3", alpha=0.2,size=1)+
    
    geom_linerange(aes(datetime, ymin = radius_list[1], ymax = radius_list[2]), size = size_ratio_ui*radius_list[1]*2*pi/365, color = cut(df$uvindex, breaks = uv_breaks, labels = uv_categories), alpha = 0.9)+
    geom_linerange(aes(datetime, ymin = radius_list[2], ymax = radius_list[3]), size = size_ratio_1*radius_list[2]*2*pi/365, color = "#ADD8E6", alpha = opacity_1*df[["Partially cloudy"]])+
    geom_linerange(aes(datetime, ymin = radius_list[2], ymax = radius_list[3]), size = size_ratio_1*radius_list[2]*2*pi/365, color = "#3A9BDC", alpha = opacity_1*df[["Overcast"]])+
    geom_linerange(aes(datetime, ymin = radius_list[2], ymax = radius_list[3]), size = size_ratio_clear*radius_list[2]*2*pi/365, color = "#F9D71C", alpha = opacity_1*df[["Clear"]])+ 
    geom_linerange(aes(datetime, ymin = radius_list[3], ymax = radius_list[4]), size = size_ratio*radius_list[3]*2*pi/365, color = "#0047ab", alpha = opacity*df[["Rain"]])+  
    geom_linerange(aes(datetime, ymin = radius_list[3], ymax = radius_list[4]), size = size_ratio*radius_list[3]*2*pi/365, color = "#7034fa", alpha = opacity*df[["Snow"]])+  
    geom_linerange(aes(datetime, ymin = radius_list[4], ymax = radius_list[5]), size = size_ratio*radius_list[4]*2*pi/365, color = "#1d2951", alpha = opacity*df[["Ice"]])+ 
    geom_linerange(aes(datetime, ymin = radius_list[4], ymax = radius_list[5]), size = size_ratio*radius_list[4]*2*pi/365, color = "#330044", alpha = opacity_2*df[["Freezing Drizzle/Freezing Rain"]])+
    
    scale_x_date(labels = date_format("%B"), breaks = date_breaks("month"))+
    ylim(0, inner_radius+12) +
    
    geom_text(
      data = data.frame(x = df$datetime[1],
                        y = 0, label = city_name),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 30/.pt,
      color = "#333333",alpha=0.7,
      inherit.aes = FALSE
    )+
    coord_curvedpolar()+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_text(margin = margin(t = 10), size = 20,colour="#18191a"))+
    theme( # remove the vertical grid lines
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )+
    labs(
      title = paste(city_name,"Weather Condition Radial 2022"),
      subtitle = " ",
      caption = " ",
      x = NULL,
      y = NULL, 
      size = 20
    )+
    theme(plot.title = element_text(hjust = 0.5, size=25))
  
  step = 0.02
  step_2 = 0.017
  weather_conditions_1 = c("Clear","Partially cloudy","Overcast","Rain","Snow","Ice",
                           "Freezing Rain")
  
  block_size = 15
  block_opacity = 0.8
  block_offset = 0.002
  block_offset_2 = 0
  block_x = 0.81
  start_x_2 = 0.03
  
  image_1 = ggdraw(image)+
    geom_text(
      data = data.frame(x = 0.85+rep(0,7),
                        y = 0.8+to_vec(for(i in 1:length(weather_conditions)) i*step),
                        label = weather_conditions_1),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 15/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_text(
      data = data.frame(x = 0.84+rep(0,10),
                        y = start_x_2+0.008+to_vec(for(i in 1:10) i*step_2),
                        label = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10")),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 15/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+geom_text(
      data = data.frame(x = 0.78+rep(0,1),
                        y = start_x_2+0.012+to_vec(for(i in 11:11) i*step_2),
                        label = c("Ultraviolet Index")),
      aes(x, y, label = label),
      hjust = 0, angle = 0, size = 18/.pt,
      color = "black",
      inherit.aes = FALSE,alpha=0.8
    )+
    geom_linerange(aes(x=block_x,ymin=0.81, ymax=0.81+step-block_offset), color = "#F9D71C", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+step, ymax=0.81+2*step-block_offset), color = "#ADD8E6", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+2*step, ymax=0.81+3*step-block_offset), color = "#3A9BDC", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+3*step, ymax=0.81+4*step-block_offset), color = "#0047ab", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+4*step, ymax=0.81+5*step-block_offset), color = "#7034fa", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+5*step, ymax=0.81+6*step-block_offset), color = "#1d2951", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+6*step, ymax=0.81+7*step-block_offset), color = "#330044", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81, ymax=0.81+step-block_offset), color = uv_categories[1], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+step_2, ymax=start_x_2+2*step_2-block_offset_2), color = uv_categories[2], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+2*step_2, ymax=start_x_2+3*step_2-block_offset_2), color = uv_categories[3], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+3*step_2, ymax=start_x_2+4*step_2-block_offset_2), color = uv_categories[4], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+4*step_2, ymax=start_x_2+5*step_2-block_offset_2), color = uv_categories[5], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+5*step_2, ymax=start_x_2+6*step_2-block_offset_2), color = uv_categories[6], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+6*step_2, ymax=start_x_2+7*step_2-block_offset_2), color = uv_categories[7], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+7*step_2, ymax=start_x_2+8*step_2-block_offset_2), color = uv_categories[8], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+8*step_2, ymax=start_x_2+9*step_2-block_offset_2), color = uv_categories[9], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+9*step_2, ymax=start_x_2+10*step_2-block_offset_2), color = uv_categories[10], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=start_x_2+10*step_2, ymax=start_x_2+11*step_2-block_offset_2), color = uv_categories[11], alpha=block_opacity,size=block_size)
  return (image_1)
}



## -------------------------------------------------------------------------------------------------------------
plot_air_quality_return=function(filname){
  options(warn=-1)
  df = read.csv(filname)
  filname_list = strsplit(filname, "\\\\")
  filname = filname_list[[1]][length(filname_list[[1]])]
  city_name = str_to_title(sub(".*/", "", gsub("-air-quality.csv", "", filname)))
  start_date = as.Date("2022-01-01")
  end_date = as.Date("2022-12-31")
  df$date=as.Date(df$date)
  colnames(df)[1] <- "datetime"
  
  df = df %>%
    filter(df$datetime %between% c(start_date, end_date))
  all_dates = seq(start_date, end_date, by = "day")
  missing_dates = as.Date(setdiff(all_dates,c(df$datetime)), origin = "1970-01-01")
  if (length(missing_dates) > 0) {
    na_matrix <- matrix(-1, nrow = length(missing_dates), ncol = ncol(df)-1)
    na_df <- as.data.frame(na_matrix)
    date_df = data.frame(date = missing_dates)
    new_rows = cbind(date_df,na_df)
    colnames(new_rows) <- colnames(df)
    df = rbind(df, new_rows)
  }
  df = df[order(df$datetime),]
  
  if(! "pm25" %in% colnames(df)) {
    df$pm25 = -1
  }
  if(! "pm10" %in% colnames(df)) {
    df$pm10 = -1
  }
  if(! "o3" %in% colnames(df)) {
    df$o3 = -1
  }
  if(! "no2" %in% colnames(df)) {
    df$no2 = -1
  }
  if(! "so2" %in% colnames(df)) {
    df$so2 = -1
  }
  if(! "co" %in% colnames(df)) {
    df$co = -1
  }
  
  df[colnames(df)[-which(colnames(df) == "datetime")]] <- data.frame(lapply(df[colnames(df)[-which(colnames(df) == "datetime")]], function(x) ifelse(is.na(x), -1, x)))
  
  categories = c("#00787E","#059A65","#85BD4B","#FFDD33","#FFBA33","#FE9633","#E44933","#CA0035","#970068","#78003F","#4E0016")
  breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 300, 400, 600)
  step_2 = 4
  myAngle <-seq(0,-360,length.out = 13)
  opacity_1=0.2
  opacity_2=0.8
  size_ratio_1 = 4.2
  size_ratio_clear = 4.1
  size_ratio = 5
  inner_radius = 10
  radius_list = to_vec(for (i in 0:9) i*step_2)+inner_radius
  text_y_1 = 0.185
  text_y_2 = 0.22
  text_y_3 = 0.22
  offset_1 = 0.2
  
  image = ggplot(df,aes(datetime))+
    
    geom_ribbon(aes(ymin=radius_list[7]-offset_1, ymax=radius_list[7]+offset_1), color = "#D3D3D3", alpha=1,size=1.5)+
    geom_ribbon(aes(ymin=radius_list[6]-offset_1, ymax=radius_list[6]+offset_1), color = "#D3D3D3", alpha=0.2,size=1)+
    geom_ribbon(aes(ymin=radius_list[5]-offset_1, ymax=radius_list[5]+offset_1), color = "#D3D3D3", alpha=0.2,size=1)+
    geom_ribbon(aes(ymin=radius_list[4]-offset_1, ymax=radius_list[4]+offset_1), color = "#D3D3D3", alpha=1,size=1.5)+
    geom_ribbon(aes(ymin=radius_list[3]-offset_1, ymax=radius_list[3]+offset_1), color = "#D3D3D3", alpha=0.2,size=1)+
    geom_ribbon(aes(ymin=radius_list[2]-offset_1, ymax=radius_list[2]+offset_1), color = "#D3D3D3", alpha=0.2,size=1)+
    
    geom_linerange(aes(datetime, ymin = radius_list[1]+0.5, ymax = radius_list[2]-0.5), size = size_ratio_1*radius_list[1]*2*pi/365, color = cut(df$pm25, breaks = breaks, labels = categories), alpha = ifelse(df$pm25<51,opacity_1,opacity_2))+
    geom_linerange(aes(datetime, ymin = radius_list[2]+0.5, ymax = radius_list[3]-0.5), size = size_ratio_1*radius_list[2]*2*pi/365, color = cut(df$pm10, breaks = breaks, labels = categories), alpha = ifelse(df$pm10<51,opacity_1,opacity_2))+
    geom_linerange(aes(datetime, ymin = radius_list[3]+0.5, ymax = radius_list[4]-0.5), size = size_ratio_clear*radius_list[3]*2*pi/365, color = cut(df$o3, breaks = breaks, labels = categories), alpha = ifelse(df$o3<51,opacity_1,opacity_2))+
    geom_linerange(aes(datetime, ymin = radius_list[4]+0.5, ymax = radius_list[5]-0.5), size = size_ratio_clear*radius_list[4]*2*pi/365, color = cut(df$no2, breaks = breaks, labels = categories), alpha = ifelse(df$no2<51,opacity_1,opacity_2))+ 
    geom_linerange(aes(datetime, ymin = radius_list[5]+0.5, ymax = radius_list[6]-0.5), size = size_ratio_clear*radius_list[5]*2*pi/365, color = cut(df$so2, breaks = breaks, labels = categories), alpha = ifelse(df$so2<51,opacity_1,opacity_2))+ 
    geom_linerange(aes(datetime, ymin = radius_list[6]+0.5, ymax = radius_list[7]-0.5), size = size_ratio_clear*radius_list[6]*2*pi/365, color = cut(df$co, breaks = breaks, labels = categories), alpha = ifelse(df$co<51,opacity_1,opacity_2))+ 
    scale_x_date(labels = date_format("%B"), breaks = date_breaks("month"))+
    ylim(0, inner_radius+7*step_2) +  
    geom_text(
      data = data.frame(x = df$datetime[1],
                        y = 0, label = city_name),
      aes(x, y, label = label),
      hjust = 0.5, vjust = 0.1, angle = 0, size = 28/.pt,
      color = "#333333",alpha=0.7,
      inherit.aes = FALSE
    )+
    coord_curvedpolar()+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_text(margin = margin(t = 10), size = 20,colour="#18191a"))+
    theme( # remove the vertical grid lines
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )+
    labs(
      title = paste(city_name,"Air Quality Index Radial 2022"),
      subtitle = " ",
      caption = " ",
      x = NULL,
      y = NULL, 
      size = 20
    )+
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  df_2 = data.frame(x = rep(0.486,6),y = 0.598+rev(seq(0,text_y_1,text_y_1/5)), label = rev(c("PM25","PM10"," O3","NO2","SO2"," CO")))
  df_3 = data.frame(x = rep(0.77,7),y = 0.749+rev(seq(0,text_y_3,text_y_3/6)), label = c("#FFFFFF","#00e400","#ffff00","#ff7e00","#ff0000","#8f3f97","#7e0023"))
  df_4 = data.frame(x = rep(0.805,7),y = 0.75+rev(seq(0,text_y_2,text_y_2/6)), label = c("AQI \n Numbers","0 - 50","51 - 100","101 - 150","151 - 200","201 - 300","301 - 500"))
  df_5 = data.frame(x = rep(0.88,7),y = 0.75+rev(seq(0,text_y_2,text_y_2/6)), label = c("AQI \n Category \n Descriptor","Good","Moderate","Unhealthy for \n Sensitive Groups","Unhealthy","Very \n Unhealthy","Hazardous"))
  
  block_y_offset = 0.0186
  block_opacity = 0.8
  block_size = 15
  
  color_list = c("#FFFFFF","#00787E","#059A65","#85BD4B","#FFDD33","#FFBA33","#FE9633","#E44933","#CA0035","#970068","#78003F","#4E0016")
  
  image_1 = ggdraw(image)+geom_text(data = df_2, aes(x, y, label = label), color = "#333333", inherit.aes = FALSE, hjust = 0, angle = 0, size = 15/.pt, alpha=0.7)+
    geom_text(data = df_4, aes(x, y, label = label), color = "#333333", inherit.aes = FALSE, hjust = 0, angle = 0, size = 12/.pt, alpha=1)+
    geom_text(data = df_5, aes(x, y, label = label), color = "#333333", inherit.aes = FALSE, hjust = 0, angle = 0, size = 12/.pt, alpha=1)+
    geom_linerange(aes(x=df_3$x[1],ymin=df_3$y[1]-block_y_offset, ymax=df_3$y[1]+block_y_offset), color = color_list[1], alpha=0,size=block_size)+
    geom_linerange(aes(x=df_3$x[2],ymin=df_3$y[2], ymax=df_3$y[2]+block_y_offset), color = color_list[2], alpha=0.4,size=block_size)+
    geom_linerange(aes(x=df_3$x[2],ymin=df_3$y[2]-block_y_offset, ymax=df_3$y[2]), color = color_list[3], alpha=0.4,size=block_size)+
    geom_linerange(aes(x=df_3$x[3],ymin=df_3$y[3], ymax=df_3$y[3]+block_y_offset), color = color_list[4], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=df_3$x[3],ymin=df_3$y[3]-block_y_offset, ymax=df_3$y[3]), color = color_list[5], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=df_3$x[4],ymin=df_3$y[4], ymax=df_3$y[4]+block_y_offset), color = color_list[6], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=df_3$x[4],ymin=df_3$y[4]-block_y_offset, ymax=df_3$y[4]), color = color_list[7], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=df_3$x[5],ymin=df_3$y[5], ymax=df_3$y[5]+block_y_offset), color = color_list[8], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=df_3$x[5],ymin=df_3$y[5]-block_y_offset, ymax=df_3$y[5]), color = color_list[9], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=df_3$x[6],ymin=df_3$y[6]-block_y_offset, ymax=df_3$y[6]+block_y_offset), color = color_list[10], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=df_3$x[7],ymin=df_3$y[7], ymax=df_3$y[7]+block_y_offset), color = color_list[11], alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=df_3$x[7],ymin=df_3$y[7]-block_y_offset, ymax=df_3$y[7]), color = color_list[12], alpha=block_opacity,size=block_size)
  
  return (image_1)
}

plot_weather_precipitation_return_new <- function(city,unit){
  plot_weather_precipitation_return(paste(weather_files_path,
                                          weather_cities_dict[city],
                                          sep="/"),unit)}

plot_weather_micelaneous_return_new <- function(city,unit){
  plot_weather_micelaneous_return(paste(weather_files_path,
                                        weather_cities_dict[city],
                                        sep="/"),unit)}

plot_daytime_return_new <- function(city){
  plot_daytime_return(paste(weather_files_path,
                            weather_cities_dict[city],
                            sep="/"))}

plot_weather_condition_return_new <- function(city){
  plot_weather_condition_return(paste(weather_files_path,
                                      weather_cities_dict[city],
                                      sep="/"))}

plot_air_quality_return_new <- function(city){
  plot_air_quality_return(paste(air_files_path,
                                air_cities_dict[city],
                                sep="/"))}

plot_precipitation_snow_return_new <- function(city,unit){
  plot_precipitation_snow_return(paste(weather_files_path,
                                        weather_cities_dict[city],
                                        sep="/"),unit)}

plot_wind_return_new <- function(city,unit){
  plot_wind_return(paste(weather_files_path,
                                       weather_cities_dict[city],
                                       sep="/"),unit)}

plot_humidity_return_new <- function(city,unit){
  plot_humidity_return(paste(weather_files_path,
                         weather_cities_dict[city],
                         sep="/"),unit)}

contrast_color <- function(bg_color) {
  # Convert the background color to RGB values
  bg_rgb <- col2rgb(bg_color)
  
  # Calculate the luminance of the background color
  luminance <- (0.299 * bg_rgb[1] + 0.587 * bg_rgb[2] + 0.114 * bg_rgb[3]) / 255
  
  # Choose either black or white as the contrasting color
  if (luminance > 0.5) {
    return("#000000") # Use black on light background
  } else {
    return("#FFFFFF") # Use white on dark background
  }
}

calculate_index <- function(df,column,val_1,val_2) {
  if (column == "temp"){
    mean = (val_1 + val_2)/2
    sd = (val_2 - val_1)/2+2+1/((val_2 - val_1)/2+1)
    result_1 = (mean(na.omit(dnorm(df[[column]], mean = mean, sd = sd))))
    k = 6
    tempmin = na.omit(df[["tempmin"]])
    tempmax = na.omit(df[["tempmax"]])
    incremental_values <- vector()
    for (i in 1:(k-1)){
      incremental_values = c(incremental_values, list(((k-i)*tempmin+(i)*tempmax)/(k)))
    }
    new_matrix <- cbind(tempmin, do.call(cbind, incremental_values), tempmax)
    result_2 = mean(na.omit(dnorm(new_matrix, mean = mean, sd = sd)))
    result = ((1)*result_1+(k+2)*result_2)/(k+2+1)
  } else {
    result = (mean(na.omit(dnorm(df[[column]], mean = (val_1 + val_2)/2, sd = (val_2 - val_1)/2+2+1/((val_2 - val_1)/2+1)))))
  }
  return (result)
}

calculate_indices <- function(df,columns,val_1s,val_2s) {
  indices = vector()
  if (length(columns)==1){
    indices[1] = calculate_index(df,columns[1],val_1s[1],val_2s[1])
  } else {
  for (i in 1:length(columns)){
    indices[i] = calculate_index(df,columns[i],val_1s[i],val_2s[i])
    }
  }
  return (indices)
}

get_indices_df <- function(columns,val_1s,val_2s) {
  name_dict <- setNames(c("temp","humidity","windspeed","cloudcover", "solarradiation", "snow", "precip"), 
                        c("Temperature","Humidity","Wind Speed","Cloud Cover","Solar Radiation","Snow","Precipitation"))
  data_list = list()
  for (filname in list.files(path="data/weather data/", pattern="\\.csv$", all.files=FALSE, full.names=FALSE)){
    city_name = str_to_title(sub(".*/", "", gsub(" *[0-9].*$", "", filname)))
    df = read.csv(paste("data/weather data/",filname,sep=""), header = TRUE, check.names = FALSE)
    new_columns = vector()
    for (column in columns){
      new_columns = c(new_columns, name_dict[[column]])
    }
    result = c(city_name,calculate_indices(df,new_columns,val_1s,val_2s))
    data_list = c(data_list, list(result))
  }
  my_df <- data.frame(do.call(rbind, data_list))
  if (length(columns)!=1){
    df_norm <- as.data.frame(
      lapply(
        my_df[, -1], function(x) round(((as.numeric(x) - min(as.numeric(x))) / (max(as.numeric(x)) - min(as.numeric(x))))*100,3)
      )
    )
  } else {
    df_norm <- data.frame(
      round((as.numeric(my_df[, 2])-min(as.numeric(my_df[, 2])))/(max(as.numeric(my_df[, 2]))-min(as.numeric(my_df[, 2])))*100,3)
    )
  }                        
  new_df = cbind(my_df[, 1],df_norm)
  colnames(new_df) = c("City",paste(columns,rep("Index",length(columns)))) 
  return (new_df)
}

add_final_score <- function(df, weights) {
  if (length(colnames(df)) == 2){
    df[["Final Score"]] = df[,2] * weights/100
  } else {
    df[["Final Score"]] = apply(df[,-1], 1, function(row) (sum(row * weights)/100))
  }
  return (df)
}



