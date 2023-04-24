
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

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # headerPanel('Visualize Weathers of 300 Cities Around the World'),
  titlePanel("     Visualize Weathers of 400 Cities Around the World"),
  navbarPage(
    "Visualize Weathers throughout 2022",
    tabPanel("Climates Around the World",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country_2", "Filter by Country or Region:", 
                             choices = c("(All)",sort(unique(df$Country))),
                             selected = "(All)"),
                 selectInput("climate_3", "Filter by Climate:", 
                             choices = c("(All)",names(climate_name_dict)),
                             selected = "(All)"),
               ),
               mainPanel(
                 leafletOutput("map_1"),
                 p(),
                 br(),
                 textOutput("text_5"),
                 DT::dataTableOutput("df_5")
               )
             )
    ),
    tabPanel("Compare Two Cities",
             sidebarLayout(
               sidebarPanel(
                 selectInput("unit_compare", "Choose a unit of measurement:", 
                             choices = c("Metric","Imperial"),
                             selected = "Metric"),
                 selectInput("city_1", "Choose the first city:", 
                             choices = weather_cities,
                             selected = ""),
                 selectInput("city_2", "Choose the second city:", 
                             choices = weather_cities,
                             selected = ""),
                 selectInput("climate_1", "Show a list of cities of the same climate:", 
                             choices = paste(df$Climate.Names, ", ", df$Koppen.climate),
                             selected = ""),
                 textOutput("text_1"),
                 DT::dataTableOutput("df_1"),
                 br(),
                 selectInput("country_1", "Show a list of cities of the same country or region:", 
                             choices = sort(unique(df$Country)),
                             selected = ""),
                 textOutput("text_4"),
                 DT::dataTableOutput("df_4")
               ),
               mainPanel(
                 textOutput("text_2"),
                 DT::dataTableOutput("df_2"),
                 br(),
                 textOutput("text_3"),
                 DT::dataTableOutput("df_3"),
                 br(),
                 br(),
                 leafletOutput("map_2"),
                 br(),
                 br(),
                 tabsetPanel(
                   tabPanel("Temperature and Precipitation", 
                            plotOutput("svg_1_1", width = "900px", height = "900px")),
                   tabPanel("Daytime and Nighttime", 
                            plotOutput("svg_2_1", width = "800px", height = "800px")),
                   tabPanel("Weather Conditions", 
                            plotOutput("svg_3_1", width = "800px", height = "800px")),
                   tabPanel("Air Quality", 
                            plotOutput("svg_4_1", width = "800px", height = "800px"))
                 ),
                 tabsetPanel(
                   tabPanel("Temperature and Precipitation", 
                            plotOutput("svg_1_2", width = "900px", height = "900px")),
                   tabPanel("Daytime and Nighttime", 
                            plotOutput("svg_2_2", width = "800px", height = "800px")),
                   tabPanel("Weather Conditions", 
                            plotOutput("svg_3_2", width = "800px", height = "800px")),
                   tabPanel("Air Quality",
                            plotOutput("svg_4_2", width = "800px", height = "800px"))
               ))
             ) 
    ),
    tabPanel("Explore the Dataset",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plot_1_x_axis", "Choose x axis for plot 1:", 
                             choices = keys(viz_cols_mapping),
                             selected = "Latitude (\u00B0)"),
                 selectInput("plot_1_x_transform", "Choose x axis transformation for plot 1:", 
                             choices = c("Linear","Logarithmic","Absolute","Cosine"),
                             selected = "Linear"),
                 selectInput("plot_1_y_axis", "Choose y axis for plot 1:", 
                             choices = keys(viz_cols_mapping),
                             selected = "Annual Mean Temperature (\u00B0C)"),
                 selectInput("plot_1_y_transform", "Choose y axis transformation for plot 1:", 
                             choices = c("Linear","Logarithmic","Absolute","Cosine"),
                             selected = "Linear"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 selectInput("plot_2_x_axis", "Choose x axis for plot 2:", 
                             choices = keys(viz_cols_mapping),
                             selected = "Latitude (\u00B0)"),
                 selectInput("plot_2_x_transform", "Choose x axis transformation for plot 2:", 
                             choices = c("Linear","Logarithmic","Absolute","Cosine"),
                             selected = "Linear"),
                 selectInput("plot_2_y_axis", "Choose y axis for plot 2:", 
                             choices = keys(viz_cols_mapping),
                             selected = "Elevation (m)"),
                 selectInput("plot_2_y_transform", "Choose y axis transformation for plot 2:", 
                             choices = c("Linear","Logarithmic","Absolute","Cosine"),
                             selected = "Logarithmic"),
                 selectInput("plot_2_z_axis", "Choose z axis for plot 2:", 
                             choices = keys(viz_cols_mapping),
                             selected = "Annual Mean Temperature (\u00B0C)"),
                 selectInput("plot_2_z_transform", "Choose z axis transformation for plot 2:", 
                             choices = c("Linear","Logarithmic","Absolute","Cosine"),
                             selected = "Linear"),
               ),
               mainPanel(
                 plotlyOutput("plot_1", width = 700, height = 700),
                 br(),
                 textOutput("regression_text_1"),
                 br(),
                 br(),
                 plotlyOutput("plot_2", width = 700, height = 700)
               ))),
    tabPanel("Contribute to This Project",
             sidebarLayout(
               sidebarPanel(
               ),
               mainPanel(
                 htmlOutput("contribution_text")
               )
             )
    )
  )
)
