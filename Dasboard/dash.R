library(shiny)
library(dplyr)

data <- df <- read.csv("train.csv")

ui <- fluidPage(
  titlePanel("Train Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select a variable:",
                  choices = names(data), selected = "Survived")
    ),
    mainPanel(
      plotOutput("plot", height = "400px"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    req(input$variable)
    
    if (!is.numeric(data[[input$variable]])) {
      variable_counts <- table(data[[input$variable]])
      colors <- c("skyblue", "lightgreen", "orange", "pink", "yellow", "purple", "red")  # Add more colors as needed
      labels <- paste(names(variable_counts), "(", variable_counts, ")", sep = " ")
      pie(variable_counts, main = input$variable, col = colors, labels = labels)
    } else {
      hist(data[[input$variable]], main = input$variable, xlab = input$variable, col = "skyblue", border = "white")
      box()
    }
  })
  
  output$table <- renderTable({
    req(input$variable)
    
    if (is.numeric(data[[input$variable]])) {
      data %>%
        group_by({{input$variable}}) %>%
        summarise(count = n())
    } else {
      NULL
    }
  })
}

shinyApp(ui = ui, server = server)
