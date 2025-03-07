##Demographic Module


labourMarketDemogUI <- function(id, data) {
  ns <- NS(id)
  
  min_date <- min(data$year)
  max_date <- max(data$year)
  
  layout_sidebar(
    sidebar = accordion(
      accordion_panel(
        "Customise Chart",
        selectizeInput(
          ns('indicator'), 
          "Select Indicator",
          labour_market_indicators(),
          selected = "Unemployment rate",
          multiple = F
        ),
        selectizeInput(
          ns('series_type'),
          "Select Series Type", 
          series_choices(),
          selected = "Seasonally Adjusted",
          multiple = F
        ),
        selectizeInput(
          ns("demographic"),
          "Select Demographic Variable",
          choices = c("Age", "Gender")
        ),
        numericInput(
          ns('years'),
          'Select Start Year',
          value = max_date - 5,
          min = min_date,
          max = max_date
        ),
        selectizeInput(
          ns("state"),
          "Select Comparison Region",
          choices = regions(),
          multiple = F
        )
      )
    ),
    plotlyOutput(ns("plot"))
  )
  
  
}

labourMarketDemogServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) { 
      
      # indicator_choices <- c("Employed total",
      #                        "Employed full-time",
      #                        "Unemployed total",
      #                        "Underemployed total",
      #                        "Underutilised total",
      #                        "Unemployment rate",
      #                        "Underemployment rate (proportion of labour force)",
      #                        "Underutilisation rate",
      #                        "Participation rate") 
      # 
      genders <- reactive({
        if(input$demographic == "Gender") {
          genders <- c("Males", "Females")
        } else {genders <- "Persons"}
      })

      ages <- reactive({
        if(input$demographic == "Age" & all(input$state == "Australia")) {
          ages <- c("15-24 years",
                    "25-34 years",
                    "35-44 years",
                    "45-54 years",
                    "55 years and over")
        } else {ages <- "Total (age)"}
      })

      choices <- reactive({
        if (all(input$state == "Australia")) {
          choices <- c("Age", "Gender")
        } else {choices <- "Gender"}
      })
      # 
      # observeEvent(input$state, {
      #   updateRadioGroupButtons(session, "series_type", choices = data %>%
      #                             filter(indicator == input$indicator,
      #                                    state %in% input$state) %>%
      #                             pull(series_type) %>%
      #                             unique() %>%
      #                             sort())
      #   updateRadioGroupButtons(session, "demographic", 
      #                           choices = choices())
      #   
      # })
      # 
      # observeEvent(input$demographic, {
      #   updateSelectInput(session, "indicator", choices = labour_market_indicators()[!labour_market_indicators() %in% "Participation rate"])
      # })
      # 
      create_data <- reactive({
        df <- data %>%
          filter(indicator == input$indicator,
                 state %in% input$state,
                 series_type == input$series_type,
                 sex %in% genders(),
                 age %in% ages(),
                 year >= input$years)
      })
      
      create_plot <- reactive({
        p <- abs_plot(data = create_data(),
                      over = list(indicator = input$indicator,
                                  state = input$state,
                                  series_type = input$series_type,
                                  sex = genders(),
                                  age = ages()),
                      years = input$years,
                      compare_aus = FALSE,
                      plotly = TRUE)
      })
      
      
      output$plot <- renderPlotly({create_plot()})
      
      output$download_plot <- downloadHandler(
        filename = function(){
          paste0(input$filename, "-plot.", input$filetype)
        },
        content = function(file) {
          plotly_IMAGE(create_plot(), format = input$filetype, width = input$width, height = input$height, out_file = file)
        }
      )
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste0(input$indicator, "-data.csv")
        },
        content = function(file) {
          write.csv(create_data(), file, row.names = FALSE)
        }
      )
    }
  )
}



