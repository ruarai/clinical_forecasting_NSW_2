

make_case_trajectory <- function(
    case_forecast,
    ascertainment,
    
    case_linelist,
    
    forecast_dates,
    
    scenario_label,
    plot_dir
) {
  # Count the number of detected cases for each onset date, bounded by our simulation and forecast dates
  nsw_cases_count <- case_linelist %>%
    count(date_onset, name = "count") %>%
    filter(date_onset < forecast_dates$date_case_linelist - days(2)) %>% 
    complete(date_onset = seq(forecast_dates$date_estimates_start, forecast_dates$date_case_linelist - days(2), by = "days"),
             fill = list(count = 0)) %>%
    
    arrange(date_onset) %>% 
    
    mutate(count = zoo::rollmean(count, 3, fill = "extend", align = "c"))  %>%
    filter(date_onset >= min(ascertainment$date_onset)) %>%
    left_join(ascertainment, by = c("date_onset")) %>%
    mutate(count = count / asc) %>%
    select(-asc)
  
  nsw_cases_count$count[(nrow(nsw_cases_count) - 1):nrow(nsw_cases_count)] <- NA_real_
  
  # Combined the count into the backcast with that of the forecast
  case_trajectory_unimputed <- bind_rows(
    case_forecast %>% 
      filter(date_onset > max(nsw_cases_count$date_onset, na.rm = TRUE)) %>%
      select(date_onset, count), 
    
    nsw_cases_count
  ) %>%
    arrange(date_onset) %>%
    
    mutate(count = round(count))
  
  # Make sure our trajectory is complete, imputing missing values with simple linear interpolation
  # This might cause issues if there's a large enough gap between the backcast and forecast, so be careful with this
  case_trajectory <- case_trajectory_unimputed %>%
    complete(date_onset = seq(min(date_onset), max(date_onset), by = "days")) %>%
    
    mutate(count = zoo::na.approx(count))
  
  
  ggplot() +
    geom_point(aes(x = date_onset, y = count, colour = "unimputed"),
               case_trajectory_unimputed, size = 0.4) +
    geom_line(aes(x = date_onset, y = count, colour = "final"),
              case_trajectory) +
    geom_line(aes(x = date_onset, y = count, colour = "forecast"),
              case_forecast)  +
    geom_line(aes(x = date_onset, y = count, colour = "nsw_cases"),
              nsw_cases_count) +
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    
    theme_minimal() +
    
    coord_cartesian(ylim = c(0, NA))
  
  
  ggsave(str_c(plot_dir, "/case_trajectory_", scenario_label, ".png"), width = 6, height = 6, bg = "white")
  
  case_trajectory
}