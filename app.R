library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(sf)
library(jsonlite)
library(plotly)
library(rsconnect)

# Main cdc data file

cdc_data <- read.csv("heart_2020_cleaned.csv")

cdc_data$PhysicalHealth <- as.numeric(unlist(cdc_data$PhysicalHealth))

# Loading in the geographic json file 

geojson_url <- "https://github.com/PublicaMundi/MappingAPI/raw/master/data/geojson/us-states.json"
geojson <- st_read(geojson_url)


## Cleaning the geographic data 

geo_data_raw <- read.csv("data-table.csv") %>%
  rename_at(c("YEAR", "STATE", "RATE", "DEATHS"), .funs = tolower) %>%
  filter(year == "2021")

state_mapping <- data.frame(
  initial = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  full_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

geo_data <- data.frame(
  state = geo_data_raw$state,
  deaths = geo_data_raw$deaths
)

geo_data$deaths <- as.numeric(geo_data$deaths)


# Merging the geographic json data with the heart disease statistics per state 

choropleth_data_raw <- merge(geo_data, state_mapping, by.x = "state", by.y = "initial")

choropleth_data_unordered <- choropleth_data_raw[, -1] %>% rename("state" = "full_name")

choropleth_data <- choropleth_data_unordered[ , c("state", "deaths")]

merged_data <- left_join(geojson, choropleth_data, by = c("name" = "state"))

merged_data <- merged_data %>%
  filter(!name %in% c("District of Columbia", "Puerto Rico"))


# UI that organizes the 3 main tabs 

ui <- dashboardPage(
  dashboardHeader(title = "Heart Disease in the United States", titleWidth = 450),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram", tabName = "Histogram", icon = icon("bars-staggered")),
      menuItem("Pie Chart", tabName = "Pie_Chart", icon = icon("bars-staggered")),
      menuItem("Choropleth Map", tabName = "Choropleth_Map", icon = icon("bars-staggered"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Histogram",
              
              h2("Histogram of Quantitative Variables"),
              h4("The Histogram below can be used to illustrate the distribution of some of the quantitative variables of our dataset. You can also use the bin slider to change the shape of the histogram in order to get a better understanding of the variables."),
              
              fluidRow(
                box(plotOutput("histplot", height = 450)),
                
                box(
                  title = "Bin Slider",
                  sliderInput("bins", "Number of Bins:", 1, 50, 30)
                ),
                
                box(selectInput("var",
                                label = "Choose a variable to display:",
                                choices = c("BMI", "Physical Health", "Mental Health", "Sleep Time"),
                                selected = "BMI"))
              )
      ),
      tabItem(tabName = "Pie_Chart",
              h2("Pie Chart"),
              h4("You can use this tool to view the breakdown of 5 of the variables of the CDC data. For example, if you want to see how many of the subjects in the data are Diabetic, you can select the Diabetic variable in the dropdown menu, and the pie chart will illustrate the different groups within the Diabetic variable."),
              fluidRow(
                box(plotOutput("piechart", height = 600, width = 800),
                    width = 8),
                
                box(selectInput("choice",
                                label = "Choose a variable to display:",
                                choices = c("Smoking", "Stroke", "Sex", "Race", "Diabetic"),
                                selected = "Smoking",
                                width = 300),
                    width = 4
                )
              )
      ),
      
      tabItem(tabName = "Choropleth_Map",
              h2("Choropleth Map of Heart Disease Deaths in the United States"),
              h4("The Choropleth Map below depicts the amount of deaths from heart disease in the United States in 2021. States with higher population, like California and Texas, have the highest amount of deaths from heart disease. You can also hover over a state to see how many deaths have occured in that state"),
              fluidRow(
                box(plotlyOutput("heatmap", height = 600, width = 1000),
                    width = 10)
              )
      )
    ) 
  )
)

# Western Hemisphere object to provide bounds for the Choropleth Map 

west_hem <- c(xmin = -180, xmax = -60, ymin = 20, ymax = 70)


# Server Function that produces the data visualizations 

server <- function(input, output){
  
  output$histplot <- renderPlot({
    
    data2 <- switch(input$var,
                    "BMI" = cdc_data$BMI,
                    "Physical Health" = cdc_data$PhysicalHealth,
                    "Mental Health" = cdc_data$MentalHealth,
                    "Sleep Time" = cdc_data$SleepTime)
    
    hist(data2, breaks = input$bins, col = "#007bc2", xlab = input$var, main = paste("Histogram of", input$var))
  })
  
  output$heatmap <- renderPlotly({
    us_states <- sf::st_read("us-states.json")
    choro_map <- ggplot() +
      geom_sf(data = merged_data, aes(fill = deaths, text = paste("State: ", name, "<br>Deaths: ", deaths))) +
      scale_fill_distiller(palette = "RdYlBu") +
      theme_minimal() + 
      coord_sf(xlim = c(west_hem["xmin"], west_hem["xmax"]), ylim = c(west_hem["ymin"],  west_hem["ymax"]))
    
    ggplotly(choro_map, tooltip = "text", dynamicTicks = TRUE)
    
  })
  
  output$piechart <- renderPlot({
    
    data4 <- switch (input$choice,
                     "Smoking" = cdc_data$Smoking,
                     "Stroke" = cdc_data$Stroke,
                     "Sex" = cdc_data$Sex,
                     "Race" = cdc_data$Race,
                     "Diabetic" = cdc_data$Diabetic,
    )
    
    selected_choice <- reactive({input$choice})
    
    count <- table(cdc_data[[selected_choice()]])
    
    pie(count, labels = names(count), col = c("lightcoral", "steelblue", "darkgreen", "yellow", "brown", "purple"),border = "white", main = paste("Pie Chart of", input$choice))
  })
  
}
shinyApp(ui, server)