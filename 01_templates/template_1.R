library(shiny)
library(ggplot2)

graph_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      inputId = ns("plottype"),
      label = "plot type",
      choices = c("boxplot", "histogram")
    ),
    plotOutput(
      outputId = ns("plot_1")
    )
  )
}

graph_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot_1 <- renderPlot({
        p <- ggplot(mtcars, aes(x = mpg))
        
        if (input$plottype == "boxplot") {
          p <- p + geom_boxplot()
        } else {
          p <- p + geom_histogram()
        }
        
        p
        
      })
    }
  )
}

ui <- fluidPage(
  graph_UI("my_module")
)

server <- function(input, output, session) {
  graph_server("my_module")
}

shinyApp(ui, server)