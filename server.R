# This is the server-backend program of a Shiny web application.
# Developing data products - course project
# by LF

library(shiny)

# Plotting 
library(ggplot2)
library(rCharts)
library(ggvis)

# Data processing libraries
library(data.table)
library(reshape2)
library(dplyr)

# Required by includeMarkdown
library(markdown)

# It has to loaded to plot ggplot maps on shinyapps.io
library(mapproj)
library(maps)

aggregate_by_state <- function(dt, year_min, year_max, evtypes) {
  # Aggregate dataset by state
  # @param dt data.table
  # @param year_min integer
  # @param year_max integer
  # @param evtypes character vector
  # @return data.table
  replace_na <- function(x) ifelse(is.na(x), 0, x)
  round_2 <- function(x) round(x, 2)
  
  states <- data.table(STATE=sort(unique(dt$STATE)))
  
  aggregated <- dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVTYPE %in% evtypes) %>%
    group_by(STATE) %>%
    summarise_each(funs(sum), COUNT:CROPDMG)
  
  # We want all states to be present even if nothing happened
  left_join(states,  aggregated, by = "STATE") %>%
    mutate_each(funs(replace_na), FATALITIES:CROPDMG) %>%
    mutate_each(funs(round_2), PROPDMG, CROPDMG)    
}

aggregate_by_year <- function(dt, year_min, year_max, evtypes) {
  # Aggregate dataset by year
  # @param dt data.table
  # @param year_min integer
  # @param year_max integer
  # @param evtypes character vector
  # @return data.table
  round_2 <- function(x) round(x, 2)
  
  # Filter
  dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVTYPE %in% evtypes) %>%
    # Group and aggregate
    group_by(YEAR) %>% summarise_each(funs(sum), COUNT:CROPDMG) %>%
    # Round
    mutate_each(funs(round_2), PROPDMG, CROPDMG) %>%
    rename(
      Year = YEAR, Count = COUNT,
      Fatalities = FATALITIES, Injuries = INJURIES,
      Property = PROPDMG, Crops = CROPDMG
    )
}

compute_affected <- function(dt, category) {
  # Add Affected column based on category
  #
  # @param dt data.table
  # @param category character
  # @return data.table
  dt %>% mutate(Affected = {
    if(category == 'both') {
      INJURIES + FATALITIES
    } else if(category == 'fatalities') {
      FATALITIES
    } else {
      INJURIES
    }
  })
}

compute_damages <- function(dt, category) {
  #Add Damages column based on category 
  # @param dt data.table
  # @param category character
  # @return data.table
  dt %>% mutate(Damages = {
    if(category == 'both') {
      PROPDMG + CROPDMG
    } else if(category == 'crops') {
      CROPDMG
    } else {
      PROPDMG
    }
  })
}

plot_impact_by_state <- function (dataset, states_map, year_min, year_max, fill, title, low = "#fff5eb", high = "#d94801") {
  # Prepare map of economic or population impact 
  # @param dt data.table
  # @param states_map data.frame returned from map_data("state")
  # @param year_min integer
  # @param year_max integer
  # @param fill character name of the variable
  # @param title character
  # @param low character hex
  # @param high character hex
  # @return ggplot
  title <- sprintf(title, year_min, year_max)
  p <- ggplot(dataset, aes(map_id = STATE))
  p <- p + geom_map(aes_string(fill = fill), map = states_map, colour='black')
  p <- p + expand_limits(x = states_map$long, y = states_map$lat)
  p <- p + coord_map() + theme_bw()
  p <- p + labs(x = "Long", y = "Lat", title = title)
  p + scale_fill_gradient(low = low, high = high)
}

plot_impact_by_year <- function(dataset, dom, yAxisLabel, desc = FALSE) {
  # Prepare plots of impact by year
  # @param dt data.table
  # @param dom
  # @param yAxisLabel
  # @param desc
  # @return plot
  impactPlot <- nPlot(
    value ~ Year, group = "variable",
    data = melt(dataset, id="Year") %>% arrange(Year, if (desc) { desc(variable) } else { variable }),
    type = "stackedAreaChart", dom = dom, width = 650
  )
  impactPlot$chart(margin = list(left = 100))
  impactPlot$yAxis(axisLabel = yAxisLabel, width = 80)
  impactPlot$xAxis(axisLabel = "Year", width = 70)
  
  impactPlot
}

plot_events_by_year <- function(dt, dom = "eventsByYear", yAxisLabel = "Count") {
  # Prepare plot of number of events by year
  # @param dt data.table
  # @param dom
  # @param yAxisLabel
  # @return plot
  eventsByYear <- nPlot(
    Count ~ Year,
    data = dt,
    type = "lineChart", dom = dom, width = 650
  )
  
  eventsByYear$chart(margin = list(left = 100))
  eventsByYear$yAxis( axisLabel = yAxisLabel, width = 80)
  eventsByYear$xAxis( axisLabel = "Year", width = 70)
  eventsByYear
}

prepare_downloads <- function(dt) {
  # Prepare dataset for downloads
  # @param dt data.table
  # @return data.table
  dt %>% rename(
    State = STATE, Count = COUNT,
    Injuries = INJURIES, Fatalities = FATALITIES,
    Property.damage = PROPDMG, Crops.damage = CROPDMG
  ) %>% mutate(State=state.abb[match(State, tolower(state.name))])
}

# Loading and pre-formatting data
states_map <- map_data("state")
dataset <- fread('./data/datausa.csv') %>% mutate(EVTYPE = tolower(EVTYPE))
evtypes <- sort(unique(dataset$EVTYPE))

# Shiny server 
shinyServer(function(input, output) {
  # Define and initialize reactive values
  values <- reactiveValues()
  values$evtypes <- evtypes
  
  # Create event type checkbox
  output$evtypeControls <- renderUI({
    checkboxGroupInput('evtypes', 'Event types', evtypes, selected=values$evtypes)
  })
  
  # Add observers on clear and select all buttons
  observe({
    if(input$clear_all == 0) return()
    values$evtypes <- c()
  })
  
  observe({
    if(input$select_all == 0) return()
    values$evtypes <- evtypes
  })
  
  # Preapre datasets #
  # Prepare dataset for maps
  dataset.agg <- reactive({
    aggregate_by_state(dataset, input$range[1], input$range[2], input$evtypes)
  })
  
  # Prepare dataset for time series
  dataset.agg.year <- reactive({
    aggregate_by_year(dataset, input$range[1], input$range[2], input$evtypes)
  })
  
  # Prepare dataset for downloads
  dataTable <- reactive({
    prepare_downloads(dataset.agg())
  })
  
  # Render Plots  
  # Population impact by state
  output$populationImpactByState <- renderPlot({
    print(plot_impact_by_state (
      dataset = compute_affected(dataset.agg(), input$populationCategory),
      states_map = states_map, 
      year_min = input$range[1],
      year_max = input$range[2],
      title = "Population impact %d - %d (numbers of affected)",
      fill = "Affected"
    ))
  })
  
  # Economic impact by state
  output$economicImpactByState <- renderPlot({
    print(plot_impact_by_state(
      dataset = compute_damages(dataset.agg(), input$economicCategory),
      states_map = states_map, 
      year_min = input$range[1],
      year_max = input$range[2],
      title = "Economic impact %d - %d (Million USD)",
      fill = "Damages"
    ))
  })
  
  # Events by year
  output$eventsByYear <- renderChart({
    plot_events_by_year(dataset.agg.year())
  })
  
  # Population impact by year
  output$populationImpact <- renderChart({
    plot_impact_by_year(
      dataset = dataset.agg.year() %>% select(Year, Injuries, Fatalities),
      dom = "populationImpact",
      yAxisLabel = "Affected",
      desc = TRUE
    )
  })
  
  # Economic impact by state
  output$economicImpact <- renderChart({
    plot_impact_by_year(
      dataset = dataset.agg.year() %>% select(Year, Crops, Property),
      dom = "economicImpact",
      yAxisLabel = "Total damage (Million USD)"
    )
  })
  
  # Render data table and create download handler
  output$table <- renderDataTable(
  {dataTable()}, options = list(searching = FALSE, pageLength = 50))
}
)