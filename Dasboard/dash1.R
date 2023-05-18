# Install and load necessary packages
library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)

# Load data from Excel file
train <- read_excel("train.xlsx")

# Define the UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    # Add dropdown selectors for column names
    selectInput(inputId = "input1", label = "Select Input 1", choices = names(train)),
    selectInput(inputId = "input2", label = "Select Input 2", choices = names(train))
  ),
  dashboardBody(
    # Add a box with a plot showing the data
    box(
      title = "My Plot",
      plotOutput("my_plot")
    )
  )
)

# Define the server for the dashboard
server <- function(input, output) {
  
  # Generate the plot based on the selected inputs
  output$my_plot <- renderPlot({
    ggplot(train, aes_string(x = input$input1)) +
      geom_histogram(aes_string(y = "..density..", colour = "black", fill = input$input2)) +
      geom_density(alpha = .2, fill = "#FF6666")
  })
  
}

# Run the app
shinyApp(ui, server)
