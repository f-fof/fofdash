## code to prepare `dashboard_data` dataset goes here
library(reportabs)
library(readabs)
library(tidyverse)
library(ecomplexity)

labour_force <- read_absdata("labour_force") |> 
  select(date, indicator, sex, age, state, value, series_type, unit) |> 
  mutate(month = month(date, label = TRUE, abbr = FALSE),
         year = year(date),
         value = ifelse(unit == "000", 1000*value, value))

usethis::use_data(labour_force, compress = 'xz', overwrite = TRUE)

industry_employment <- read_absdata("industry_employment") |> 
  select(date, indicator, industry, state, series_type, value, value_share, unit)

usethis::use_data(industry_employment, compress = 'xz', overwrite = TRUE)

dashboard_data <- bind_rows(
  labour_force,
  read_absdata("hours_worked")
) 

relevant_dates <- c(max(dashboard_data$date),
                    max(dashboard_data$date - years(1)))

dashboard_data <- dashboard_data |> 
  filter(between(date, min(relevant_dates), max(relevant_dates))) |> 
  mutate(year = year(date))

usethis::use_data(dashboard_data, compress = 'xz', overwrite = TRUE)  
         
# Economic Complexity -----------------------------------------------------



complexity_exports <- read_complexitydata("state_economic_complexity") |> 
  select(year, location_code, hs_product_code, export_value, rca) |> 
  inner_join(read_complexitydata("atlas_pci"), by = c("hs_product_code", "year"))

usethis::use_data(complexity_exports, compress = 'xz', overwrite = TRUE)

complexity_rankings <- read_complexitydata("state_economic_complexity") |> 
  distinct(year, location_code, eci_rank) |> 
  mutate(year = as.Date(paste0(year, "0101"), format = "%Y%d%m"),
         location_code = clean_state(location_code, to = "state_name"))

complexity_rankings_final <- complexity_rankings |> 
  slice_max(order_by = year) |> 
  distinct(location_code, eci_rank_final = eci_rank) |> 
  mutate(location_code = clean_state(location_code, to = "state_name"))

complexity_rankings_first <- complexity_rankings |> 
  slice_min(order_by = year) |> 
  distinct(location_code, eci_rank_first = eci_rank) |> 
  mutate(location_code = clean_state(location_code, to = "state_name")) 

complexity_rankings <- complexity_rankings |> 
  inner_join(complexity_rankings_first) |> 
  inner_join(complexity_rankings_final)

usethis::use_data(complexity_rankings, overwrite = TRUE)

complexity_opportunities <- read_complexitydata("state_economic_complexity") |> 
  filter(year == max(year)) |> 
  select(year, location_code, hs_product_code, export_value, rca, product_complexity_index, cog, density)


complexity_opportunities <- complexity_opportunities |> 
  inner_join(complexity_classification) 

usethis::use_data(complexity_opportunities, compress = 'xz', overwrite = TRUE)