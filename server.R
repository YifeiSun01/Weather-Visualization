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
library(extrafont)
library(ggforce)
library(comprehenr)
library(dplyr)
library(stringr)
library(LaplacesDemon)
library(data.table)
library(lutz)
library(lubridate)

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

df_index = read.csv("data/Climate 123.csv", row.names = 1)
colnames(df_index) = c(" "," "," ")
rownames(df_index) <- 1:nrow(df_index)

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
    coord_polar() +
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
    theme(axis.text.x=element_text(margin = margin(t = 0.1), size = 17,colour="#18191a",angle=myAngle))+
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
                        label = c("Precipitation","Snow","Solar Radiation","Humidity","Snow Depth", "Wind")),
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
    coord_polar(theta="x",start=0)+
    theme(axis.text.x=element_text(margin = margin(t = 0.1), size = 20,colour="#18191a",angle=myAngle))+
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
  
  step_2 = 4
  myAngle <-seq(0,-360,length.out = 13)
  opacity_1=1
  opacity_2=0.7
  opacity=0.7
  size_ratio_1 = 7.7
  size_ratio_clear = 7.5
  size_ratio = 7.6
  inner_radius = 10
  radius_list = to_vec(for (i in 0:5) i*step_2)+inner_radius
  
  image = ggplot(df,aes(datetime))+
    
    geom_ribbon(aes(ymin=radius_list[4]-0.1, ymax=radius_list[4]), color = "#D3D3D3", alpha=1,size=1.5)+
    geom_ribbon(aes(ymin=radius_list[3]-0.1, ymax=radius_list[3]), color = "#D3D3D3", alpha=0.2,size=1)+
    geom_ribbon(aes(ymin=radius_list[2]-0.1, ymax=radius_list[2]), color = "#D3D3D3", alpha=0.2,size=1)+
    
    geom_linerange(aes(datetime, ymin = radius_list[1], ymax = radius_list[2]), size = size_ratio_1*radius_list[1]*2*pi/365, color = "#ADD8E6", alpha = opacity_1*df[["Partially cloudy"]])+
    geom_linerange(aes(datetime, ymin = radius_list[1], ymax = radius_list[2]), size = size_ratio_1*radius_list[1]*2*pi/365, color = "#3A9BDC", alpha = opacity_1*df[["Overcast"]])+
    geom_linerange(aes(datetime, ymin = radius_list[1], ymax = radius_list[2]), size = size_ratio_clear*radius_list[1]*2*pi/365, color = "#F9D71C", alpha = opacity_1*df[["Clear"]])+ 
    geom_linerange(aes(datetime, ymin = radius_list[2], ymax = radius_list[3]), size = size_ratio*radius_list[2]*2*pi/365, color = "#0047ab", alpha = opacity*df[["Rain"]])+  
    geom_linerange(aes(datetime, ymin = radius_list[2], ymax = radius_list[3]), size = size_ratio*radius_list[2]*2*pi/365, color = "#7034fa", alpha = opacity*df[["Snow"]])+  
    geom_linerange(aes(datetime, ymin = radius_list[3], ymax = radius_list[4]), size = size_ratio*radius_list[3]*2*pi/365, color = "#1d2951", alpha = opacity*df[["Ice"]])+ 
    geom_linerange(aes(datetime, ymin = radius_list[3], ymax = radius_list[4]), size = size_ratio*radius_list[3]*2*pi/365, color = "#330044", alpha = opacity_2*df[["Freezing Drizzle/Freezing Rain"]])+
    
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
    coord_polar()+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_text(margin = margin(t = 10), size = 20,colour="#18191a",angle=myAngle))+
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
  weather_conditions_1 = c("Clear","Partially cloudy","Overcast","Rain","Snow","Ice",
                           "Freezing Rain")
  
  block_size = 15
  block_opacity = 0.8
  block_offset = 0.002
  block_x = 0.81
  
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
    geom_linerange(aes(x=block_x,ymin=0.81, ymax=0.81+step-block_offset), color = "#F9D71C", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+step, ymax=0.81+2*step-block_offset), color = "#ADD8E6", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+2*step, ymax=0.81+3*step-block_offset), color = "#3A9BDC", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+3*step, ymax=0.81+4*step-block_offset), color = "#0047ab", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+4*step, ymax=0.81+5*step-block_offset), color = "#7034fa", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+5*step, ymax=0.81+6*step-block_offset), color = "#1d2951", alpha=block_opacity,size=block_size)+
    geom_linerange(aes(x=block_x,ymin=0.81+6*step, ymax=0.81+7*step-block_offset), color = "#330044", alpha=block_opacity,size=block_size)
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
    coord_polar()+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_text(margin = margin(t = 10), size = 20,colour="#18191a",angle=myAngle))+
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
  
  df_8 <- reactive({
    df_index
  })
  
  output$df_8 = DT::renderDataTable({
    DT::datatable(df_8(), escape = FALSE, 
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
  
  output$contribution_text = renderUI({
    HTML("<p>This project is created by Yifei Sun.</p>
<p>Contribute to this project by uploading more data to <a href='https://github.com/YifeiSun01/Weather-Visualization/tree/main/data'>this Github</a>.</p>
<p>Data Source:</p> 
<p><a href='https://www.visualcrossing.com/weather/'>VISUAL CROSSING WEATHER</a></p>
<p><a href='https://aqicn.org/data-platform/register/#form'>Air Quality Historical Data Platform</a></p>")
  })
}