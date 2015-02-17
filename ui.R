# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/

library(shiny)
library(rCharts)

shinyUI(
  navbarPage("Severe Weather Events in USA - Data Explorer",
             tabPanel(p(icon("list-alt"), "Readme First"),
                      mainPanel(
                        h2('Synopsis of data analysis'),
                        p('This data visualization explores NOAA Storm database and answer a couple of important questions regarding severe weather events, such as:'),
                        p('1. Across the United States, which types of events are most harmful with respect to population health?'),
                        p('2. Across the United States, which types of events have the greatest economic consequences?'),
                        h3('How to use this data explorer?'),
                        p('* Click on Plot Parameters tab and then it is shown range of years in a slider and event types as check boxes on the left side'),
                        p('* The right side has been divided on three sections [By State] [By Year] [Data visualizer]'),
                        p('* [By State] it is shown two graphs, the first one to show population impact and the second one to show economic impact.'),
                        p('* [By Year] it is shown three graphs (1) number of events by year (2) population impact by year (3) economic impact by year'),
                        p('* [Data visualizer] it is shwon a grid containing the sample data for this web application'),
                        p('All the graphs which are shown on this web page are affected by changing data on the slides, radio buttons or checkboxes.'),
                        p('Happy exploring!')
                        )
                      ),
             
             tabPanel(p(icon("bar-chart-o"), "Plot Parameters"),
                      sidebarPanel(
                        sliderInput("range", 
                                    "Range of Years:", 
                                    min = 1950, 
                                    max = 2011, 
                                    value = c(1993, 2011)),
                        uiOutput("evtypeControls"),
                        actionButton(inputId = "clear_all", label = "Clear all checkboxes"),  
                        actionButton(inputId = "select_all", label = "Select all checkboxes") 
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          # Data by state
                          tabPanel(p(icon("map-marker"), "Graphs By State"),
                                   column(5,
                                          wellPanel(
                                            radioButtons(
                                              "populationCategory",
                                              "Population impact:",
                                              c("Both" = "both", "Injuries" = "injuries", "Fatalities" = "fatalities"))
                                          )
                                   ),
                                   column(5,
                                          wellPanel(
                                            radioButtons(
                                              "economicCategory",
                                              "Economic impact:",
                                              c("Both" = "both", "Property damage" = "property", "Crops damage" = "crops"))
                                          )
                                   ),
                                   column(10,
                                          plotOutput("populationImpactByState"),
                                          plotOutput("economicImpactByState")
                                   )
                          ),
                          
                          # Time series data
                          tabPanel(p(icon("line-chart"), "Graphs By Year"),
                                   h3('Number of events by year', align = "center"),
                                   showOutput("eventsByYear", "nvd3"),
                                   h3('Population impact by year', align = "center"),
                                   showOutput("populationImpact", "nvd3"),
                                   h3('Economic impact by year', align = "center"),
                                   showOutput("economicImpact", "nvd3")
                          ),
                          
                          # Data 
                          tabPanel(p(icon("table"), "Sample Data visualizer"),  
                                   dataTableOutput(outputId="table")
                          )
                        )
                      )
                      
             )
  )
)