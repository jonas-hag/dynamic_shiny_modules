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
  actionButton(
    inputId = "add_module",
    label = "Add a module"
  ),
  div(
    id = "add_here"
  )
)

server <- function(input, output, session) {
  observeEvent(input$add_module, {
    graph_server(
      id = paste0("id_", input$add_module)
    )
    
    insertUI(
      selector = "#add_here",
      ui = graph_UI(id = paste0("id_", input$add_module))
    )
  })
}

shinyApp(ui, server)