summaryUI <- function(id) {
  
  ns <- NS(id)
  lf_release <- list("current" = current_release(),
                     "nxt" = next_release())
  
  
  
  layout_sidebar(
    sidebar = selectInput(
      ns("state"),
      "Select State",
      choices = unique(dashboard_data$state),
      selected = "Australia"
    ),
    h1(textOutput(ns("region_selected"))),
    h2(paste0("Employment Insights - ", reportabs::release(labour_force, "month"), " ", reportabs::release(labour_force, "year"))),
    p(paste0("Last updated on: ", format(lf_release$current, "%A, %d %B %Y"))),
    p(paste0("The next update is: ", format(lf_release$nxt, "%A, %d %B %Y"))),
    htmlOutput(ns("table"))
  )
}

summaryServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$region_selected <- renderText({
        input$state
      })
      
      
      output$table <- renderUI({
        
        using <- data %>%
          dplyr::filter(sex == "Persons",
                        age == "Total (age)",
                        state == input$state,
                        indicator %in% dashboard_summary$indicator,
                        series_type == "Trend") %>%
          distinct(state, indicator, series_type) %>%
          as.list()
        
        sparklines <- create_sparklines(data,  input$state)
        
        current <- data %>%
          dplyr::filter(indicator %in% using$indicator,
                        state == input$state,
                        series_type == "Trend",
                        age == "Total (age)",
                        sex == "Persons",
                        date == max(.$date)) %>%
          select(indicator, unit, current = value)
        
        over_month <- data %>%
          value_at(data = ., filter_with = using, at_month = month(max(.$date) - months(1), label = T, abbr = F)) %>%
          select(indicator, last_month = value)
        
        over_year <- data %>%
          value_at(data = ., filter_with = using, at_year = max(.$year) - 1) %>% 
          select(indicator, last_year = value)
        
        table_data <- left_join(current, over_month, by = "indicator") %>%
          left_join(over_year, by = "indicator") %>%
          left_join(dashboard_summary, by = "indicator") %>% 
          mutate(change_over_month = current - last_month,
                 change_over_year = current - last_year) %>%
          rowwise() %>%
          mutate(colour_month = add_colours(change_over_month, reverse),
                 colour_year = add_colours(change_over_year, reverse),
                 arrow_month = add_arrows(change_over_month, reverse),
                 arrow_year = add_arrows(change_over_year, reverse)) %>%
          ungroup() %>%
          left_join(sparklines, by = "indicator") %>%
          mutate(current = case_when(
            unit == "000" ~ as_comma_group(., group = "indicator", value = "current"),
            unit == "Percent" ~ as_percent(current)),
            change_over_month = case_when(
              unit == "000" ~ as_comma_group(., group = "indicator", value = "change_over_month"),
              unit == "Percent" ~ as_percent(change_over_month)),
            change_over_year = case_when(
              unit == "000" ~ as_comma_group(., group = "indicator", value = "change_over_year"),
              unit == "Percent" ~ as_percent(change_over_year)))  %>%
          arrange(factor(indicator, levels = dashboard_summary$indicator)) %>%
          select(name, 
                 min_date, 
                 max_date,
                 current, 
                 change_over_month, 
                 change_over_year,
                 sparkline, 
                 colour_month, 
                 colour_year, 
                 arrow_month, 
                 arrow_year,
                 -c(unit, last_month, last_year, reverse))
        
        out <- format_table(
          table_data,
          align = c("l", rep("c", NCOL(table_data) - 1)),
          col.names = c("Indicator", "Current Value", "Monthly Change", "Yearly Change", "Trend"),
          list(min_date = F,
               max_date = F,
               colour_month = F,
               colour_year = F,
               arrow_month = F,
               arrow_year = F,
               indicator = F,
               current = formatter("span", style = style(font.weight = "bold")),
               change_over_month = formatter("span",
                                             style = ~ style(color = ifelse(colour_month == "red",  "#ffb24d", "#64b478")),
                                             ~ icontext(ifelse(arrow_month == "arrow-down", "arrow-down", "arrow-up"), change_over_month)),
               change_over_year = formatter("span",
                                            style = ~ style(color = ifelse(colour_year == "red",  "#ffb24d", "#64b478")),
                                            ~ icontext(ifelse(arrow_year == "arrow-down", "arrow-down", "arrow-up"), change_over_year))
          )) %>%
          htmltools::HTML() %>%
          div() %>%
          spk_add_deps()
        
        out
        
      })
      
      
      
    }
  )
}
