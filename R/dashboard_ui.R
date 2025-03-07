download_graph_ui <- function(id) {
  fluidRow(
    column(width = 6,
           textInput(NS(id, "filename"),
                     "Filename",
                     placeholder = "Type a filename")
    ),
    column(width = 6,
           radioButtons(NS(id, "filetype"),
                        "File extension",
                        choices = c("png",
                                    "jpeg")
           )
    ),
    column(width = 6,
           numericInput(NS(id, "width"),
                        "Plot width (in pixels)",
                        value = 1000,
                        min = 800
           )
    ),
    column(width = 6,
           numericInput(NS(id, "height"),
                        "Plot height (in pixels)",
                        value = 500,
                        min = 300
           )
    ),
    column(width = 12,
           downloadButton(NS(id, "download_plot"), "Download chart", class = "download-button"),
           downloadButton(NS(id, "download_data"), "Download chart data", class = "download-button")
    )
  )
}



#### Specfic Instructions ####
specific_instructions <- function(...)  {
  
  tabPanel(title = "Module Specific Information",
           fluidPage(
             h2("Summary"),
             p("The boxes on the Dashboard show the current value, monthly change, and yearly change for 12 key labour market indicators.
Those shown in ",
               HTML("<b style = 'color:#64b478'>green</b>"), " represent an improvement since last month, and those shown in ",
               HTML("<b style = 'color:#ffb24d'>orange</b>"), "represent a deterioration over the previous month.",
               "The arrows show in which direction the indicator has moved over the previous month. Note that for indicators such as the Unemployment Rate, a
               decrease (downward arrow) is considered an improvement (shown in green)."),
             h2("Employment Insights"),
             h3("Tab 1"),
             p("This tab shows the time series of an indicator for the region selected in the sidebar menu."),
             h3("Regional Comparison"),
             p("This tab allows for the comparison of a given indicator across as many regions as you like. In order to compare States with Territories
  the selected Series Type must be 'Original'"),
             h3("Demography"),
             p("This tab allows for a breakdown of a given indicator by demographic variables. Age is only available for Australia."),
             h3("Small Area Unemployment Rates"),
             p("This tab shows smoothed unemployment rates for all SA2 Statistical Areas in Australia, or only those in a region if a region
               is selected using data from the Small Area Labour Markets publication produced by the Department of Employment. The download button
               on this page will download the map as it is visible to you. However please not that there may be some delays if attempting to download
               a map with all SA2 Statistical Areas shown."),
             h2("Industry Insights"),
             h3("Tab 1"),
             p("This tab shows the breakdown of employment by industry for the region selected in the sidebar menu, for a given year, and month. 
    Selecting an industry shows the historic time series data for the selected indicator in a specific industry. Up to 9 industries can be compared."),
             h3("Regional Comparison"),
             p("This tab shows the differences between the share of employment in an industry between the region selected in the sidebar menu, and the comparison
    region selected for a given year."),
             h2("COVID-19: Maps"),
             p("This tab shows how the COVID-19 global pandemic has affected small regions within Australia. It shows at the Statistical Area 2 level: "),
             tags$ul(
               tags$li("Approximate number of businesses receiving JobKeeper"),
               tags$li("Number of individuals receiving JobSeeker"),
               tags$li("Approximate proportion of businesses receiving JobKeeper"),
               tags$li("Proportion of individuals receiving JobSeeker")
             ),
             p("Note that the JobKeeper analysis is approximate as only PostCode data has been made available which does not directly convert to a Statistical Area. 
               In addition, the latest business count data is for June 2019."),
             p("At the SA4 Level, it shows the change in the number of payroll jobs - indexed to the week ending the 14th of March 2020"),
             h2("COVID-19: Payroll Jobs"),
             h3("Regional Comparison, Demography"),
             p("These tabs use data from the Weekly Payroll Jobs in Australia dataset published by the ABS. It allows comparison across States
             and Territories, with additional drilldowns based on gender and age"),
             h3("Industry"),
             p("This tab shows the change in employment by industry for a selected region, indexed to March 14th - the week when Australia recorded
             its 100th COVID-19 case. As such, the bar chart will show nothing until the weeks slider is adjusted. Selecting an industry shows
             the change in Payroll jobs since the week ending January 6th 2020."),
             p("Note that the payroll jobs index is measured differently to the number of people employed. For more information see: ",
               a(href = "https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6160.0.55.001Explanatory%20Notes1Week%20ending%2025%20July%202020?OpenDocument","Weekly Payroll Jobs and Wages in Australia"))
           )
  )
}

#### User Guide ####
user_guide <- function(...) {
  
  tabPanel(title = "User Guide",
           fluidPage(
             h2("Welcome"),
             p("Welcome to the Australian Industrial Transformation Institute's Economic Indicator Dashboard. 
    You can navigate through the different modules by selecting them from the side menu."),
             p("This dashboard is currently in beta, and may occasionally show errors to the user."),
             p("The most common cause for an error is the selection of a combination of", em('Region,', 'Indicator,', 'Series Type'), "and", em('Date'), "for which data is not available. Selecting another combination
    of variables should solve any problems."),
             p("Additional modules are currently in development, and will be added as they are completed. The Summary, and Employment Insights modules 
are updated on approximately the third thursday of the month when the ABS Labour Force Survey is released. The Industry Insights module is updated 
every quarter when the ABS Detailed Labour Force Survey is released. Check back regularly for access to the latest data."),
             p("Any chart, as it appears on screen, as well as the data behind them, can be downloaded in the Downloads section of each module."),
             p("For any comments, requests, or issues, please contact", a(href = 'mailto:aiti@flinders.edu.au', "aiti@flinders.edu.au"),"."),
             h2("Definitions"),
             p(tags$b("Indicator: "), "A time series variable, measured and collected by the ABS."),
             p(tags$b("Series Type: "), "How the observed data has been processed by the ABS."),
             p("Original: The observed, unprocessed data."),
             p("Seasonally Adjusted: Observed data processed to remove influences that are systematic and calendar related."),
             p("Trend: Observed data processed to remove calendar related, and other irregular effects, to show the long term movement of an indicator. 
    Note that the Trend Series is currently suspended for many Indicators."),
             p(tags$b("Region: "), "States and Territories in Australia, or Australia itself."),
             h2("Notes on Data Availability"),
             p("Data for this dashboard is sourced from the Australian Bureau of Statistics. There is not universal coverage across regions,
    economic indicators, or series types. Due to the impact of the Coronavirus, there is currently no Seasonally Adjusted Series available for the Territories.")))
}
#### User Guide Tab ####
user_guide_tab <- function(...) {
  tabItem(
    tabName = "user_guide",
    fluidRow(
      tabBox(
        id = "user_guide_tab_id",
        width = 12,
        user_guide(),
        specific_instructions()
      )
    )
  )
}
#### Summary UI ####


#### UI ####
dash_ui <- function(...) {
  page_navbar(
    theme = bs_theme(
      preset = "shiny",
      "primary" = "#DC5000"
    ),
    lang = "en",
    title = "Economic Indicators Dashboard",
    nav_panel(title = "Dashboard", summaryUI("table")),
    nav_panel(title = "Labour Force",
              navset_card_tab(title = "Labour Force",
                              nav_panel("Regional Comparison",   labourMarketUI("lm_ts", data = labour_force)),
                              nav_panel("Demographic Comparison",   labourMarketDemogUI("lm_demog", data = labour_force))
                              
              )
    ),
    nav_panel(title = "Industry Composition",
              navset_card_tab(title = "Industry Composition", 
                              nav_panel("Employment by Industry", industryEmploymentUI("empInd_ts", data = industry_employment)),
                              nav_panel("Regional Comparison", industryEmploymentComparisonUI("empInd_region", data = industry_employment))
              )
    ),
    nav_panel(title = "Economic Complexity",
              complexityUI("complexity")
    )
  )
}


