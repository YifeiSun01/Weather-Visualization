source("functions.R")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("     Visualize Weathers of 430 Cities Around the World"),
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
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 selectInput("climate_group_1", "Select a climate zone to see its distribution map: ", 
                             choices = c("(All)","A, Tropical","B, Arid","C, Temperate","D, Cold","E, Polar"),
                             selected = "(All)"),
               ),
               mainPanel(
                 leafletOutput("map_1"),
                 p(),
                 imageOutput("mapImage"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                bsCollapse(
                  id = "collapsible_box_6",open = NULL,
                  bsCollapsePanel("What is Köppen–Geiger climate classification?",
                                  htmlOutput("explain_text_6")
                  )
                ),
                tabsetPanel(
                  tabPanel("Climate Descriptions", 
                reactableOutput("df_8")),
                tabPanel("Climates and Cities",
                reactableOutput("df_5"))
               )
             )
    )),
    tabPanel("Compare Two Cities",
             sidebarLayout(
               sidebarPanel(
                 selectInput("unit_compare", "Choose a unit of measurement:", 
                             choices = c("Metric","Imperial"),
                             selected = "Metric"),
                 selectInput("city_1", "Choose the first city:", 
                             choices = weather_cities,
                             selected = "New York"),
                 selectInput("city_2", "Choose the second city:", 
                             choices = weather_cities,
                             selected = "Los Angeles"),
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
                 reactableOutput("df_2"),
                 br(),
                 textOutput("text_3"),
                 reactableOutput("df_3"),
                 br(),
                 br(),
                 leafletOutput("map_2"),
                 br(),
                 br(),
                 tabsetPanel(
                   tabPanel("Temperature, Precipitation, Wind, Humidity, and Solar Radiation", 
                            plotOutput("svg_1_1", width = "900px", height = "900px"),
                             bsCollapse(
                               id = "collapsible_box_1",open = NULL,
                               bsCollapsePanel("What is Relative Humidity and Feelslike Temperature?",
                               htmlOutput("explain_text_1")
                             )
                           )
                         ),
                   tabPanel("Precipitation, Snow", 
                            plotOutput("svg_6_1", width = "900px", height = "900px"),
                            bsCollapse(
                              id = "collapsible_box_21",open = NULL,
                              bsCollapsePanel("Precipitation Level",
                                              htmlOutput("explain_text_7")
                              )
                            )
                   ),
                   tabPanel("Wind", 
                            plotOutput("svg_7_1", width = "900px", height = "900px"),
                            reactableOutput("df_10")
                   ),
                   tabPanel("Relavtive Humidity", 
                            plotOutput("svg_8_1", width = "900px", height = "900px"),
                   ),
                   tabPanel("Dew Point, Cloud, Pressure, Visibility, and Solar Energy", 
                            plotOutput("svg_5_1", width = "900px", height = "900px"),
                            bsCollapse(
                              id = "collapsible_box_2",open = NULL,
                              bsCollapsePanel("What is Dew Point?",
                              htmlOutput("explain_text_2")
                              )
                            )
                          ),
                   tabPanel("Daytime and Nighttime", 
                            plotOutput("svg_2_1", width = "800px", height = "800px"),
                            bsCollapse(
                              id = "collapsible_box_5",open = NULL,
                              bsCollapsePanel("What affects sunrise and sunset time?",
                                              htmlOutput("explain_text_5")
                              )
                            )
                          ),
                   tabPanel("Weather Conditions and Ultraviolet Index", 
                            plotOutput("svg_3_1", width = "800px", height = "800px"),
                            bsCollapse(
                              id = "collapsible_box_3",open = NULL,
                              bsCollapsePanel("What is Ultra Violet Index?",
                                              htmlOutput("explain_text_3")
                              )
                            )
                          ),
                   tabPanel("Air Quality", 
                            plotOutput("svg_4_1", width = "800px", height = "800px"),
                            bsCollapse(
                              id = "collapsible_box_4",open = NULL,
                              bsCollapsePanel("What is Air Quality Index?",
                                              htmlOutput("explain_text_4")
                              )
                            )
                          )
                 ),
                 tabsetPanel(
                   tabPanel("Temperature, Precipitation, Wind, Humidity, Solar Radiation", 
                            plotOutput("svg_1_2", width = "900px", height = "900px")),
                   tabPanel("Dew Point, Cloud, Pressure, Visibility, Solar Energy", 
                            plotOutput("svg_5_2", width = "900px", height = "900px")),
                   tabPanel("Precipitation, Snow", 
                            plotOutput("svg_6_2", width = "900px", height = "900px")),
                   tabPanel("Wind", 
                            plotOutput("svg_7_2", width = "900px", height = "900px")),
                   tabPanel("Relavtive Humidity", 
                            plotOutput("svg_8_2", width = "900px", height = "900px")),
                   tabPanel("Daytime and Nighttime", 
                            plotOutput("svg_2_2", width = "800px", height = "800px")),
                   tabPanel("Weather Conditions and Ultraviolet Index", 
                            plotOutput("svg_3_2", width = "800px", height = "800px")),
                   tabPanel("Air Quality",
                            plotOutput("svg_4_2", width = "800px", height = "800px"))
                 ))
             ) 
    ),
    tabPanel("Find Your Favorite Cities",
             sidebarLayout(
               sidebarPanel(
                 selectInput("unit_2", "Choose a unit of measurement:", 
                             choices = c("Metric","Imperial"),
                             selected = "Metric"),
                 selectInput("factors","Choose the factors you would like to consider",
                             c("Temperature","Humidity","Wind Speed","Cloud Cover","Solar Radiation","Snow","Precipitation"), multiple = TRUE),
                 conditionalPanel(
                   condition = "input.factors.length > 0 ",
                   uiOutput("input_factors_ui")
                 ),
                 selectInput("weight_method", "Choose a method to determine weight:", 
                             choices = c("Self-Defined Weights Vector","AHP (Analytical Hierarchy Process)"),
                             selected = "Self-Defined Weights Vector")
                 
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Define Weights Vector",
                 conditionalPanel(
                   condition = "input.factors.length > 0 && input.weight_method == 'Self-Defined Weights Vector' ",
                   uiOutput("input_self_weights_ui"),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   htmlOutput("self_weight_text")
                 ),
                 conditionalPanel(
                   condition = "input.factors.length > 0 && input.weight_method == 'AHP (Analytical Hierarchy Process)' ",
                   uiOutput("input_AHP_weights_ui"),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   tabsetPanel(
                     tabPanel(" ",
                              htmlOutput("AHP_weight_text"))),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   tabsetPanel(
                     tabPanel("AHP Matrix",
                              reactableOutput("AHP_matrix"),
                              reactableOutput("test_df"))),
                   # h4("AHP Matrix"),
                   # reactableOutput("AHP_matrix"),
                   br(),
                   br(),
                   tabsetPanel(
                     tabPanel("AHP Weights",
                              reactableOutput("AHP_weights"))),
                   # h4("AHP Weights"),
                   # reactableOutput("AHP_weights")
                 ),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 actionButton("button1", "Submit"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 tabsetPanel(
                   tabPanel("See City Ranking",
                        reactableOutput("city_ranking")    
                     )
                   )
                 ),
                 tabPanel("See Your Favorite Cities on the World Map",
        sliderInput(inputId = "ranking_range",label = "Choose the quantile range of cities you would like to see  (0-most favorite, 100-least favorite)", min=0, max=100, value=c(0,5), step = 1),  
        leafletOutput("map_4", width = 700, height = 700),  
        br(),
        br(),
        tabsetPanel(
          tabPanel("General Information",
                reactableOutput("df_23")), 
          tabPanel("Climate Details",
                reactableOutput("df_22"))
        )
        
        )
                 )
               )
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
                 selectInput("country_3", "Filter by Country or Region:", 
                             choices = c("(All)",sort(unique(df$Country))),
                             selected = "(All)"),
                 selectInput("climate_4", "Filter by Climate:", 
                             choices = c("(All)",names(climate_name_dict)),
                             selected = "(All)"),
                 selectInput("favorite_1", "Filter by the Quantile Range in the Previous Page:", 
                             choices = c("No","Yes"),
                             selected = "No"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
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
                 selectInput("country_4", "Filter by Country or Region:", 
                             choices = c("(All)",sort(unique(df$Country))),
                             selected = "(All)"),
                 selectInput("climate_5", "Filter by Climate:", 
                             choices = c("(All)",names(climate_name_dict)),
                             selected = "(All)"),
                 selectInput("favorite_2", "Filter by the Quantile Range in the Previous Page:", 
                             choices = c("No","Yes"),
                             selected = "No"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 selectInput("map_3_feature", "Choose a feature to visualize on the map:", 
                             choices = keys(viz_cols_mapping),
                             selected = "Annual Mean Temperature (\u00B0C)"),
                 selectInput("country_5", "Filter by Country or Region:", 
                             choices = c("(All)",sort(unique(df$Country))),
                             selected = "(All)"),
                 selectInput("climate_6", "Filter by Climate:", 
                             choices = c("(All)",names(climate_name_dict)),
                             selected = "(All)"),
                 selectInput("favorite_3", "Filter by the Quantile Range in the Previous Page:", 
                             choices = c("No","Yes"),
                             selected = "No"),
               ),
               mainPanel(
                 plotlyOutput("plot_1", width = 700, height = 700),
                 br(),
                 textOutput("regression_text_1"),
                 br(),
                 br(),
                 h3("What	Causes	Different	Climates?"),
                 tabsetPanel(
                   tabPanel("Latitude", 
                            
                            bsCollapse(
                              id = "collapsible_box_10",open = "Latitude",
                              bsCollapsePanel("Latitude",
                                              htmlOutput("reason_text_1")
                              )
                            )
                          ),
                   tabPanel("Elevation", 
                            
                            bsCollapse(
                              id = "collapsible_box_11",open = "Elevation",
                              bsCollapsePanel("Elevation",
                                              htmlOutput("reason_text_2")
                              )
                            )
                          ),
                   tabPanel("Topography", 
                            bsCollapse(
                              id = "collapsible_box_12",open = "Topography",
                              bsCollapsePanel("Topography",
                                              htmlOutput("reason_text_3")
                              )
                            )
                          ),
                   tabPanel("Water	Bodies", 
                            bsCollapse(
                              id = "collapsible_box_13",open = "Water	Bodies",
                              bsCollapsePanel("Water	Bodies",
                                              htmlOutput("reason_text_4")
                              )
                            )
                          ),
                   tabPanel("Atmospheric	Circulation",
                            bsCollapse(
                              id = "collapsible_box_14",open = "Atmospheric	Circulation",
                              bsCollapsePanel("Atmospheric	Circulation",
                                              htmlOutput("reason_text_5")
                              )
                            )
                          ),
                   tabPanel("Vegetation",
                            bsCollapse(
                              id = "collapsible_box_15",open = "Vegetation",
                              bsCollapsePanel("Vegetation",
                                              htmlOutput("reason_text_6")
                              )
                            )
                   ),
                 ),
                 br(),
                 br(),
                 plotlyOutput("plot_2", width = 700, height = 700),
                 br(),
                 br(),
                 leafletOutput("map_3", width = 700, height = 700)
               ))),
    
    tabPanel("Contribute to This Project",
             mainPanel(
               htmlOutput("contribution_text")
             )
    )
  )
)