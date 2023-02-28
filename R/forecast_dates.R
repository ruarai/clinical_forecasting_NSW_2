

get_forecast_dates <- function(
    hospital_linelist_path,
    case_linelist_path
) {
  
  date_hospital_linelist <- ymd(str_extract(hospital_linelist_path, "\\d{4}_\\d{2}_\\d{2}"))
  date_case_linelist <- ymd(str_extract(case_linelist_path, "\\d{8}"))
  
  date_data_min <- min(date_hospital_linelist, date_case_linelist)
  
  
  date_estimates_start <- date_data_min - days(90)
  
  tibble(
    date_estimates_start = date_estimates_start,
    
    date_hospital_linelist = date_hospital_linelist,
    date_case_linelist = date_case_linelist
  )
}