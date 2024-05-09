install.packages(c("shiny", "ggplot2", "readr"))
library(shiny)
library(ggplot2)
library(readr)
data <- read_csv("C:/Users/karth/Downloads/reviews.csv")
ui <- fluidPage(
  titlePanel("Restaurant Review Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("restaurant", "Select a Restaurant:", choices = unique(data$Restaurant))
    ),
    
    mainPanel(
      plotOutput("pieChart"),
      verbatimTextOutput("reviewOverview")
    )
  )
)
server <- function(input, output) {
  output$pieChart <- renderPlot({
    selected_restaurant_data <- data[data$Restaurant == input$restaurant, ]
    
    selected_restaurant_data$Rating <- as.factor(selected_restaurant_data$Rating)
    
    rating_distribution <- table(selected_restaurant_data$Rating)
    
    ggplot(selected_restaurant_data, aes(x = "", fill = Rating)) +
      geom_bar(width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = paste("Rating Distribution for", input$restaurant),
           fill = "Rating",
           x = NULL,
           y = NULL)
  })
  
  output$reviewOverview <- renderPrint({
    paste("Selected Restaurant:", input$restaurant, "\n\nReviews Overview:\n",
          paste("Total Reviews:", length(unique(data[data$Restaurant == input$restaurant, ]$Reviewer)), "\n",
                "Average Rating:", mean(data[data$Restaurant == input$restaurant, ]$Rating), "\n",
                "Number of Followers:", sum(data[data$Restaurant == input$restaurant, ]$Metadata)))
  })
}
print("hello")
shinyApp(ui = ui, server = server)
