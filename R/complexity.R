complexityUI <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = selectInput(
      ns("state"),
      "Select State",
      choices = unique(complexity_exports$location_code),
      selected = "SA"
    ),
    card(
      card_header("Complexity Rank"),
      plotOutput(ns("complexityrank"))
    ),
    layout_columns(
      card(
        card_header("Export Composition"),
        plotOutput(ns("complexitytree"))
      ),
      card(
        card_header("Opportunities"),
        plotlyOutput(ns("complexityopps"))
        
      )
    )
  )
}

complexityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$complexitytree <- renderPlot(
      graph_complexity_tree(data = complexity_exports, region = input$state, year = 2022) 
    )
    
    output$complexityrank <- renderPlot(
      graph_complexity_rank(data = complexity_rankings)
    )
    
    output$complexityopps <- renderPlotly(
      graph_complexity_opportunities(complexity_opportunities, region = input$state, industry = NULL) |> 
        ggplotly()
    )
  })
}



