#'@import shiny
#'@import shinyWidgets
#'@import dplyr
#'@import forcats
#'@import bslib
#'@import ggplot2
#'@import lubridate
#'@import scales
#'@import zoo
#'@import reportabs
#'@import pkgload
#'@import strayr
#'@import pkgload
#'@import sparkline
#'@import formattable
#'@import htmltools
#'@import htmlwidgets
#'@import ecomplexity
#'@importFrom plotly ggplotly layout plotlyOutput renderPlotly plotly_IMAGE
#'@importFrom stats reorder setNames
#'@importFrom utils download.file tail write.csv
#'@importFrom tidyr tribble
#'
#'@export aiti_dashboard


#### Preamble ####
Sys.setenv("plotly_username" = "hamgamb")
Sys.setenv("plotly_api_key" = 'SDYMDyK3YM0eZrTNpyoa')





#### Server ####
dash_server <- function(input, output, session) {


  #Labour Market 
  labourMarketServer("lm_ts", data = labour_force)
  labourMarketDemogServer("lm_demog", data = labour_force)
  
  #Employment by Industry
  industryEmploymentServer("empInd_ts", data = industry_employment)
  industryEmploymentComparisonServer("empInd_region", data = industry_employment)

  #Dashboard Summary
  summaryServer("table", data = dashboard_data)
  
  complexityServer("complexity")
  
}

aiti_dashboard <- function(...) {
  shinyApp(ui = dash_ui(), server = dash_server)

}
