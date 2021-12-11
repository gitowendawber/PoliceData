library("shiny")
library("ukpolice")
library("tidyverse")
library("leaflet")

nbd <- ukc_neighbourhoods("durham")
nbd2 <- nbd$id
names(nbd2) <- nbd$name

# Define UI for application
ui <- fluidPage(
  titlePanel("UK Police Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("nbd", "Choose Durham Constabulary Neighborhood", nbd2),
      textInput("date", "Enter the desired year and month in the format YYYY-MM", value = "2021-09")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barchart"),
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  bdy <- reactive({
    bdy <- ukc_neighbourhood_boundary("durham", input$nbd)
    bdy |>
      mutate(latitude = as.numeric(latitude),
             longitude = as.numeric(longitude))
  })
  
  # Get crimes for selected neighbourhood
  # Also wrapped in a reactive because we need this to trigger a
  # change when the boundary above, or date, changes
  crimes <- reactive({
    bdy2 <- bdy() |>
      select(lat = latitude,
             lng = longitude)
    
    ukc_crime_poly(bdy2[round(seq(1, nrow(bdy2), length.out = 100)), ], input$date)
  })
  
  # First do plot
  output$barchart <- renderPlot({
    ggplot(crimes()) +
      geom_bar(aes(y = category, fill = outcome_status_category)) +
      labs(y = "Crime", fill = "Outcome Status")
  }, res = 96)
  
  # Then do map
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(lng = bdy()$longitude, lat = bdy()$latitude) |>
      addCircles(lng = as.numeric(crimes()$longitude), lat = as.numeric(crimes()$latitude), label = crimes()$category, color = "red")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
