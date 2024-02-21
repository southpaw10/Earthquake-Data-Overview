library(shiny)
library(dplyr)
library(plotly)
library(rworldmap)
library(ggplot2)

# Load the data
earthquake_data <- read.csv("/Users/shubham/earthquake_dataset.csv", stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  
  titlePanel("Earthquake Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Histograms of Latitude and Longitude Ranges"),
      sliderInput("lat_range", "Latitude Range", min = -90, max = 90, value = c(-90, 90)),
      sliderInput("lon_range", "Longitude Range", min = -180, max = 180, value = c(-180, 180)),
      hr(),
      h4("Top 5 Entries by Count"),
      selectInput("top_count", "Select Count Category",
                  choices = c("Place", "Country", "Continent")),
      hr(),
      h4("Pie Charts")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Latitude Histogram", plotOutput("histogram_latitude")),
        tabPanel("Longitude Histogram", plotOutput("histogram_longitude")),
        tabPanel("Top 5 Entries", tableOutput("top_entries")),
        tabPanel("Choropleth Map", plotlyOutput("choropleth_map")),
        tabPanel("Bubble Chart", plotlyOutput("bubble_chart")),
        tabPanel("Pie Charts", plotlyOutput("pie_chart_country"), plotlyOutput("pie_chart_continent"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Latitude Histogram
  output$histogram_latitude <- renderPlot({
    hist(earthquake_data$Latitude,
         xlim = input$lat_range,
         main = "Latitude Distribution",
         xlab = "Latitude",
         col = "lightblue")
  })
  
  # Longitude Histogram
  output$histogram_longitude <- renderPlot({
    hist(earthquake_data$Longitude,
         xlim = input$lon_range,
         main = "Longitude Distribution",
         xlab = "Longitude",
         col = "lightgreen")
  })
  
  # Top 5 Entries
  output$top_entries <- renderTable({
    top_entries <- earthquake_data %>% 
      count(!!sym(input$top_count)) %>% 
      arrange(desc(n)) %>% 
      slice(1:5)
    top_entries
  })
  
  # Choropleth Map
  output$choropleth_map <- renderPlotly({
    world <- map_data("world")
    
    choropleth <- earthquake_data %>%
      count(Country) %>%
      mutate(percent = n / sum(n))
    
    plot_ly(choropleth, z = ~percent, text = ~Country, locations = ~Country, type = "choropleth", locationmode = "country names") %>%
      layout(title = "Country Distribution")
  })
  
  # Bubble Chart
  output$bubble_chart <- renderPlotly({
    bubble_data <- earthquake_data %>%
      group_by(Place) %>%
      summarise(total_magnitude = sum(Magnitude),
                Latitude = first(Latitude),
                Longitude = first(Longitude))
    
    bubble_data <- bubble_data %>% top_n(10, total_magnitude)
    
    ggplot(bubble_data, aes(x = Longitude, y = Latitude, size = total_magnitude, label = Place)) +
      geom_point(alpha = 0.6, color = "blue") +
      geom_text(aes(label = Place), size = 3, vjust = 1, hjust = 1) +
      scale_size_continuous(range = c(1, 20)) +
      labs(title = "Top 10 Earthquake Bubble Chart",
           x = "Longitude",
           y = "Latitude",
           size = "Total Magnitude") +
      theme_minimal()
  })
  
  # Pie Charts - Country
  output$pie_chart_country <- renderPlotly({
    pie_data <- earthquake_data %>%
      count(Country) %>%
      mutate(percent = n / sum(n))
    
    plot_ly(pie_data, labels = ~Country, values = ~percent, type = "pie") %>%
      layout(title = "Country Distribution")
  })
  
  # Pie Charts - Continent
  output$pie_chart_continent <- renderPlotly({
    pie_data <- earthquake_data %>%
      count(Continent) %>%
      mutate(percent = n / sum(n))
    
    plot_ly(pie_data, labels = ~Continent, values = ~percent, type = "pie") %>%
      layout(title = "Continent Distribution")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
