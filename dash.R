library(shiny)
library(readxl)

data <- df <- read_excel("D:/EDA/train.xlsx")
ui <- fluidPage(
  titlePanel("Train Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select a variable:",
                  choices = names(data), selected = "Survived")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)
server <- function(input, output) {
  output$plot <- renderPlot({
    hist(data[[input$variable]], main = input$variable)
  })
  
  output$table <- renderTable({
    data %>% 
      group_by({{input$variable}}) %>% 
      summarise(count = n())
  })
}
shinyApp(ui = ui, server = server)
