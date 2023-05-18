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
      conditionalPanel(
        condition = "typeof input.variable !== 'undefined' && isNaN(input.variable)",
        plotOutput("pie_chart", height = "400px")
      ),
      conditionalPanel(
        condition = "typeof input.variable !== 'undefined' && !isNaN(input.variable)",
        plotOutput("histogram_plot", height = "400px")
      ),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  output$pie_chart <- renderPlot({
    req(input$variable)
    
    if (!is.character(data[[input$variable]])) {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
           main = "Invalid Variable", xlab = "", ylab = "")
      text(0.5, 0.5, "Please select a non-numeric variable.", cex = 1.5, col = "red", font = 2)
    } else {
      table_data <- data %>%
        count(input$variable)
      
      pie(table_data$n, labels = table_data[[input$variable]], main = input$variable,
          col = rainbow(length(table_data$n)), border = "white")
    }
  })
  
  output$histogram_plot <- renderPlot({
    req(input$variable)
    
    if (!is.numeric(data[[input$variable]])) {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
           main = "Invalid Variable", xlab = "", ylab = "")
      text(0.5, 0.5, "Please select a numeric variable.", cex = 1.5, col = "red", font = 2)
    } else {
      hist(data[[input$variable]], main = input$variable,
           xlab = input$variable, col = "skyblue", border = "white")
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
