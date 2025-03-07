box_map <- function(...) {
  data.frame(
    id = c("unemployment_rate",
           "unemployed",
           "employment_total",
           "participation_rate", 
           "employment_ft",
           "employment_pt",
           "underemployed",
           "underutilised",
           "underemployment_rate",
           "underutilisation_rate", 
           "hours_worked_total", 
           "labour_force_total"),
    indicator = c("Unemployment rate",
                  "Unemployed total", 
                  "Employed total", 
                  "Participation rate",
                  "Employed full-time", 
                  "Employed part-time", 
                  "Underemployed total",
                  "Underutilised total",
                  "Underemployment rate (proportion of labour force)",
                  "Underutilisation rate",
                  "Monthly hours worked in all jobs",
                  "Labour force total"),
    reverse = c(TRUE, 
                FALSE,
                FALSE,
                FALSE,
                FALSE,
                FALSE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                FALSE,
                FALSE),
    percent = c(TRUE,
                FALSE,
                FALSE,
                TRUE,
                FALSE,
                FALSE,
                FALSE,
                FALSE,
                TRUE,
                TRUE,
                FALSE,
                FALSE),
    footer =  paste0("Data last updated: ")
  )
}

labour_market_indicators <- function() {
  labour_force %>%
    dplyr::distinct(indicator) %>%
    dplyr::filter(!indicator %in% c("Monthly hours worked in all jobs (employed full-time)",
                                    "Monthly hours worked in all jobs (employed part-time)",
                                    "Unemployed looked for full-time work",
                                    "Unemployed looked for only part-time work",
                                    "Unemployment rate looked for full-time work",
                                    "Unemployment rate looked for only part-time work")) %>% 
    dplyr::pull()
}

series_choices <- function() {
  c("Original",
    "Seasonally Adjusted",
    "Trend")
}

regions <- function() {
  sort(c("South Australia",
         "Western Australia",
         "Tasmania",
         "Australia",
         "Queensland",
         "Victoria",
         "New South Wales",
         "Northern Territory",
         "Australian Capital Territory"))
}

boxes_names <- function() {
  tibble::tribble(
    ~indicator, ~label, 
    "Employed total", "Employed Total",
    "Employed full-time", "Employed Full Time",
    "Employed part-time", "Employed Part Time",
    "Unemployment rate", "Unemployment Rate",
    "Unemployed total", "Unemployed Total",
    "Labour force total", "Labour Force Total",
    "Underemployed total", "Underemployed Total",
    "Underutilised total", "Underutilised Total",
    "Participation rate", "Participation Rate",
    "Employment to population ratio", "Employment to Population Ratio",
    "Underemployment rate (proportion of labour force)", "Underemployment Rate",
    "Underutilisation rate", "Underutilisation Rate",
    "Monthly hours worked in all jobs", "Hours Worked",
    "Jobkeeper applications", "JobKeeper Applications",
    "Jobkeeper proportion", "JobKeeper Rate",
    "Jobseeker payment", "JobSeeker Recipients",
    "Youth allowance other", "Youth Allowance"
  )
  
}

create_sparklines <- function(data, region) {
  data  %>%
    dplyr::filter(sex == "Persons", 
           age == "Total (age)",
           state == {{region}},
           indicator %in% dashboard_summary$indicator,
           series_type == "Trend",
           between(date, last(date) - months(12), last(date))) %>%
    dplyr::group_by(indicator) %>%
    dplyr::summarise(min_date = format(min(date), "%B %Y"),
                     max_date = format(max(date), "%B %Y"),
                     sparkline = sparkline::spk_chr(value,
                               disableInteraction = TRUE,
                               type = "line",
                               width = "160px",
                               height = "50px", 
                               lineColor = unname(fof_cols("Midnight Navy")),
                               fillColor = FALSE, 
                               spotRadius = 3,
                               spotColor = unname(fof_cols("Summer Red")),
                               minSpotColor = FALSE,
                               maxSpotColor = FALSE)) %>%
    dplyr::ungroup()
}
dashboard_summary <- data.frame(
  indicator = forcats::as_factor(c("Unemployment rate",
              "Unemployed total",
              "Employed total",
              "Participation rate",
              "Employed full-time",
              "Employed part-time",
              "Underemployed total",
              "Underutilised total",
              "Underemployment rate (proportion of labour force)",
              "Underutilisation rate", 
              "Monthly hours worked in all jobs")),
    reverse = c(TRUE,
                TRUE,
                FALSE,
                FALSE,
                FALSE,
                FALSE,
                TRUE,
                TRUE,
                TRUE, 
                TRUE,
                FALSE),
  name = c("Unemployment Rate",
           "Unemployed Total",
           "Employed Total",
           "Participation Rate",
           "Employed Full-Time",
           "Employed Part-Time",
           "Underemployed Total",
           "Underutilised Total",
           "Underemployment Rate",
           "Underutilisation Rate",
           "Hours Worked")

)

add_colours <- function(value, reverse) {
  if (reverse) {
    case_when(value > 0 ~ "red",
              value < 0 ~ "green")
  } else {
    case_when(value > 0 ~ "green",
              value < 0 ~ "red")
  }
  
}

add_arrows <- function(value, reverse) {
  if (reverse) {
    case_when(value > 0 ~ "arrow-up",
              value < 0 ~ "arrow-down")
  } else {
    case_when(value > 0 ~ "arrow-up",
              value < 0 ~ "arrow-down")
  }
  
  
}

current_release <- function() {
  release_page <- xml2::read_html("https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release")
  
  current_release <- release_page |> 
    rvest::html_nodes(xpath = '//*[@id="release-date-section"]/div[1]/div[2]') |> 
    rvest::html_text() |> 
    trimws()
  
  as.Date(current_release, format = "%d/%m/%Y")
}

next_release <- function() {
  release_page <- xml2::read_html("https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release")
  
  next_release <- release_page |> 
    rvest::html_nodes(xpath = '//*[@id="release-date-section"]/div[2]/div/div/ul/li[1]/span/text()') |> 
    rvest::html_text() 
  
  next_release <- next_release |> 
    gsub(x = next_release, pattern = "([A-Z])\\w+", replacement = "") |> 
    trimws()
  
  next_release <- as.Date(next_release, format = "%d/%m/%Y")
  
  if(length(next_release) == 0) {
    next_release <- release_page |> 
      rvest::html_nodes(xpath = '//*[@id="release-date-section"]/div[2]/div/text()') |> 
      rvest::html_text() %>% 
      .data[2] |> 
      trimws()
    
    next_release <- as.Date(next_release, format = "%d/%m/%Y") 
  } 
  
  if(length(next_release) == 0) {
    next_release <- NA
  } 
  
  return(next_release)
}
