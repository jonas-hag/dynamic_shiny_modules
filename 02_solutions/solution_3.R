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
  
  active_modules <- reactiveVal(value = NULL)
  
  observeEvent(input$add_module, {
    # update the list of currently shown modules
    current_id <- paste0("id_", input$add_module)
    active_modules(c(current_id, active_modules()))
    
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
    if (length(active_modules()) > 0) {
      current_id <- active_modules()[1]
      removeUI(
        selector = paste0("#", current_id)
      )
      
      # remove the inputs
      remove_shiny_inputs(
        id = current_id,
        .input = input
      )
      
      # update the list of currently shown modules
      active_modules(active_modules()[-1])
    }
  })
}

shinyApp(ui, server)