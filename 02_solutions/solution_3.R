library(shiny)
library(ggplot2)

graph_UI <- function(id) {
  ns <- NS(id)
  
  div(
    id = id,
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
        req(input$plottype)
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

remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

ui <- fluidPage(
  actionButton(
    inputId = "add_module",
    label = "Add a module"
  ),
  actionButton(
    inputId = "remove_module",
    label = "Remove a module"
  ),
  div(
    id = "add_here"
  )
)

server <- function(input, output, session) {
  
  module_counter <- reactiveVal(value = 0)
  
  observeEvent(input$add_module, {
    # update the number of currently shown modules
    module_counter(module_counter() + 1)
    current_id <- paste0("id_", module_counter())
    
    graph_server(
      id = current_id
    )
    
    insertUI(
      selector = "#add_here",
      ui = graph_UI(id = current_id)
    )
  })
  
  observeEvent(input$remove_module, {
    
    # only remove a module if there is at least one module shown
    if (module_counter() > 0) {
      current_id <- paste0("id_", module_counter())
      removeUI(
        selector = paste0("#", current_id)
      )
      
      # remove the inputs
      remove_shiny_inputs(
        id = current_id,
        .input = input
      )
      
      # update the number of currently shown modules
      module_counter(module_counter() - 1)
    }
  })
}

shinyApp(ui, server)