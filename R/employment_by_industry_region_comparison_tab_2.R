#Employment by industry

industryEmploymentComparisonUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- c("Employed total",
                         "Employed full-time",
                         "Employed part-time")
  
  series_choices <- "Original"
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  layout_sidebar(
    sidebar = accordion(
      accordion_panel(
        "Customise Chart",
        selectizeInput(
          ns("indicator"),
          "Select Indicator", 
          indicator_choices
        ),
        sliderTextInput(
          ns("date"),
          "Select Date",
          choices = as.yearqtr(sort(unique(data$date))),
          selected = as.yearqtr(date_max)
        ),
        selectizeInput(
          ns("region_one"),
          "Select Region One",
          regions(),
          multiple = FALSE
        ),
        selectizeInput(
          ns("region_two"),
          "Select Region Two",
          regions(),
          multiple = FALSE
        )
      )
    ),
    plotlyOutput(ns("plot"))
  )
}

industryEmploymentComparisonServer <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      

      
      create_data <- reactive({
        df <- data %>%
          filter(indicator == input$indicator,
                 industry != "Total (industry)") %>% 
          group_by(date, state, industry) %>% 
          summarise(value = mean(value)) %>% 
          mutate(share = 100*value/sum(value)) %>%
          ungroup() %>%
          filter(state %in% c(input$region_one, input$region_two),
                 date == as.Date(zoo::as.yearqtr(input$date)) + months(1)) %>%
          select(-value) %>% 
          tidyr::pivot_wider(names_from = state, values_from = share) %>%
          arrange(!!as.name(input$region_one)) %>%
          mutate(industry = forcats::as_factor(industry)) 
      })
      
      create_plot <- reactive({
        p <- ggplot(create_data()) + 
          geom_segment(aes(x = industry, 
                           xend = industry, 
                           y = !!as.name(input$region_one), 
                           yend = !!as.name(input$region_two)), 
                       colour = 'darkblue') + 
          geom_point(aes(x = industry, 
                         y = !!as.name(input$region_one),
                         text = paste0(input$region_one,
                                       "<br>",industry, 
                                       "<br>", as_percent(!!as.name(input$region_one))),
                         fill = input$region_one),
                     shape = 21, 
                     colour = 'yellow') +
          geom_point(aes(x = industry, 
                         y = !!as.name(input$region_two),
                         text = paste0(input$region_two, 
                                       "<br>", industry,
                                       "<br>", as_percent(!!as.name(input$region_two))),
                         fill = input$region_two), 
                     shape = 21,
                     colour = 'blue') +
          scale_y_continuous(labels = percent_format(scale = 1)) +
          scale_fill_manual(breaks = c(input$region_one, input$region_two), values = c('yellow', 'blue')) + 
          coord_flip() + 
          labs(
            y = NULL,
            x = NULL,
            title = toupper(paste0("share of industry employment: ", input$region_one, " & ", input$region_two, " (", input$date, ")"))
          ) +
          theme_fof()
        
        
        ggplotly(p, tooltip = 'text') %>%
          layout(autosize = TRUE,
                 legend = list(orientation = "h",
                               y = -0.15,
                               title = ""),
                 annotations = list(
                   x = 1,
                   y = -0.2,
                   showarrow = FALSE,
                   xref = "paper",
                   yref = "paper",
                   xanchor = "right",
                   yanchor = "auto",
                   text = "Source: AITI Economic Indicators"
                 ))
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
          paste(input$indicator, "-data.csv", sep = '')
        },
        content = function(file) {
          write.csv(create_data(), file, row.names = FALSE)
        }
      )
    }
  )
}