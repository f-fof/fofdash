#Employment by industry

industryEmploymentUI <- function(id, data) {
  
  ns <- NS(id)
  
  indicator_choices <- c("Employed total",
                         "Employed full-time",
                         "Employed part-time",
                         "Underemployed total")
  
  industry_choices <- data |> 
    filter(industry != "Total (industry)") |> 
    pull(industry) |> 
    unique() |> 
    sort()
  
  series_choices <- sort(unique(data$series_type))
  
  layout_sidebar(
    sidebar = accordion(
      accordion_panel(
        "Customise Chart",
        selectizeInput(
          ns("indicator"),
          "Select Indicator",
          choices = indicator_choices,
          selected = "Employed Total"
        ),
        selectizeInput(
          ns("industry"),
          "Select Industry (max 9)",
          choices = industry_choices,
          multiple = TRUE
        ),
        radioButtons(
          ns("share"),
          "Display as:",
          choices = c("Share", "Value"),
          selected = "Value"
        ),
        selectizeInput(
          ns("state"),
          "Select State",
          regions(),
          selected = "Australia",
          multiple = F
        ),
        uiOutput(ns("date"))
      )
    ),
    plotlyOutput(ns("plot"))
  )
  
}

industryEmploymentServer <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      date_min <- min(data$date)
      date_max <- max(data$date)
      
    
      output$date <- renderUI({
        if(is.null(input$industry)) {
          sliderTextInput(
            width = "100%",
            inputId = session$ns("date"),
            label = "Select Date",
            choices = zoo::as.yearqtr(sort(unique(data$date))),
            selected = zoo::as.yearqtr(date_max))
        }
      })
      
      

      
      create_data <- reactive({
        if(is.null(input$industry)) {
          df <- data %>%
            filter(industry != "Total (industry)",
                   state == input$state, 
                   indicator == input$indicator) %>% 
            group_by(date, industry) %>% 
            summarise(value = mean(value),
                      .groups = 'drop_last') %>% 
            mutate(value_share = 100*value/sum(value)) %>%
            filter(date == as.Date(zoo::as.yearqtr(input$date)) + months(1)) %>%
            ungroup() %>%
            arrange(desc(industry)) %>%
            mutate(industry = forcats::as_factor(industry)) 
        } else {
          df <- data %>% 
            filter(state == input$state, 
                   indicator == input$indicator,
                   industry != "Total (industry)",
                   series_type == "Original") %>%
            group_by(date) %>%
            mutate(value_share = 100*value/sum(value)) %>%
            ungroup() %>%
            filter(industry %in% input$industry)
        }
      })
      
      create_plot <- reactive({
        
        validate(
          need(length(input$industry < 10), message = FALSE)
        )
        
        if(input$share == "Share") {
          y_var <- "value_share"
          y_labels <- percent_format(scale = 1)
        } else {
          y_var <- "value"
          y_labels <- comma_format(scale = 1/1000, suffix = 'k')
        }
        
        if(length(input$industry) > 1) {
          plot_title <- toupper(paste0(input$state, ": ", input$indicator, " (Multiple industries)"))
          
        } else {
          plot_title <- toupper(paste0(input$state, ": ", input$indicator, " (", input$industry, ")"))
          
        }
        
        if(is.null(input$industry)) {
          
          
          
          p <- ggplot(create_data(), aes_(x = ~reorder(industry, value), 
                                          y =  as.name(y_var),
                                          text = ~paste0(input$indicator, ": ", as_comma(value),
                                                         " (", as_percent(value_share), ")"))) + 
            geom_bar(stat='identity') + 
            labs(
              y = NULL,
              x = NULL,
              title = toupper(paste0(input$indicator, ": ", input$state, " (", input$date, ")"))
            ) +
            scale_y_continuous(expand = c(0,0), labels = y_labels) +
            coord_flip() +
            theme_fof(flipped = TRUE)
        } else {
          
          p <- ggplot(create_data(), aes_(x = ~date, 
                                          y = as.name(y_var),
                                          colour = ~industry, 
                                          text = ~paste0("Date: ", format(date, "%Y-%b"),
                                                         "<br>",industry, ": ", as_comma(value),
                                                         " (", as_percent(value_share), ")"),
                                          group = ~industry)) + 
            geom_line() + 
            labs(
              x = NULL, 
              y = NULL,
              title = plot_title
            ) + 
            scale_colour_fof() +
            scale_y_continuous(labels = y_labels)  +
            theme_fof()
          
        }
        
        ggplotly(p, tooltip = "text") %>%
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
      
      
      output$plot <- renderPlotly({
        validate(
          need(input$date, message = FALSE)
        )
        
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
          write.csv(create_data() %>%
                      select(date, industry, value, value_share), file, row.names = FALSE)
        }
      )
      
    }
  )
  
}