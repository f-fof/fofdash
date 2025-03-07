labourMarketUI <- function(id, data) {
  
  min_date <- min(data$year)
  max_date <- max(data$year)
  layout_sidebar(
  sidebar = accordion(
    accordion_panel(
      "Customise Chart",
      selectizeInput(
        NS(id, "indicator"),
        "Select Indicator",
        choices = labour_market_indicators(),
        multiple = F
      ),
      selectizeInput(
        NS(id,"series_type"),
        "Select Series Type",
        choices = series_choices(),
        selected = "Seasonally Adjusted"
      ),
      numericInput(
        NS(id,"years"),
        "Select Start Year",
        value = year(max(labour_force$date)) - 5,
        min = min(labour_force$date),
        max = max(labour_force$date)
      ),
      selectizeInput(
        NS(id, "state"),
        "Select States",
        choices = regions(),
        selected = "Australia",
        multiple = T
      )
      
    )
  ),
  plotlyOutput(NS(id, "plot"))
  )
  
}


labourMarketServer <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
    create_data <- reactive({
        df <- data |> 
          filter(indicator == input$indicator,
                 year >= input$years,
                 state %in%  input$state,
                 age == "Total (age)",
                 sex == "Persons",
                 series_type == input$series_type) |> 
          select(date, month, year, indicator, value, unit, sex, age, state)
        
      })
      
      create_plot <- reactive({
        p <- abs_plot(data = create_data(),
                      over = list(indicator = input$indicator,
                                  state = input$state,
                                  series_type = input$series_type),
                      years = input$years,
                      compare_aus = FALSE,
                      plotly = TRUE) 
      })
      
      
      output$plot <- renderPlotly({
        create_plot()
        
      })
      
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
          paste(input$indicator, "-data.csv", sep = '')
        },
        content = function(file) {
          write.csv(create_data(), file, row.names = FALSE)
        }
      )
      
      
      
      report_url <- reactive({
        paste0("https://www.flinders.edu.au/content/dam/documents/research/aiti/monthly-employment-insights/",
               tolower(gsub(x = input$state, pattern = " ", replacement = "-")),
               ".pdf")
      })
      
      output$download_report_button <- renderUI({
        if(all(input$state != "Australia") & length(input$state) == 1)
          downloadButton(
            outputId = session$ns("download_report"),
            label = paste("Download report for ", input$state),
            class = "download-button"
          )
      })
      
      output$download_report <- downloadHandler(
        filename = function() {
          paste0("AITI Labour Market Brief - ", input$state, ".pdf")
        },
        content = function(file) {
          download.file(report_url(), file, mode = "wb")
        }
      )
    }
  )
}
