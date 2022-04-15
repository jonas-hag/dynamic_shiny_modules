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
    actionButton(
      inputId = ns("change_colour"),
      label = "change colour"
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
      plot_colour <- reactiveVal(value = "black")
      default_colours <- c("black", "red", "green", "blue")
      
      session$userData[[paste0(id, "_observer_", "1")]] <-
        observeEvent(input$change_colour, {
          colour_index <- input$change_colour %% 4 + 1
          new_colour <- default_colours[colour_index]
          plot_colour(new_colour)
        })
      
      output$plot_1 <- renderPlot({
        p <- ggplot(mtcars, aes(x = mpg))
        
        if (input$plottype == "boxplot") {
          p <- p + geom_boxplot(fill = plot_colour())
        } else {
          p <- p + geom_histogram(fill = plot_colour())
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

remove_observers <- function(id, .session) {
  invisible(
    lapply(grep(paste0(id, "_observer"), names(.session$userData), value = TRUE),
           function(i) {
             .subset2(.session$userData, i)$destroy()
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
  max_module_used <- reactiveVal(value = 0)
  
  observeEvent(input$add_module, {
    # update the number of currently shown modules
    max_module_used(max_module_used() + 1)
    active_modules(c(max_module_used(), active_modules()))
    current_id <- paste0("id_", max_module_used())
    
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
      current_id <- paste0("id_", active_modules()[1])
      removeUI(
        selector = paste0("#", current_id)
      )
      
      # remove the inputs
      remove_shiny_inputs(
        id = current_id,
        .input = input
      )
      
      # remove the observers
      remove_observers(
        id = current_id,
        .session = session
      )
      
      # update the number of currently shown modules
      active_modules(active_modules()[-1])
    }
  })
}

shinyApp(ui, server)