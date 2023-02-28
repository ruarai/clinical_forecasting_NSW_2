

get_time_varying_estimates <- function(
    case_linelist,
    hospital_linelist,
    
    forecast_dates,
    
    n_bootstraps
) {
  
  case_linelist_subset <- case_linelist %>%
    
    filter(date_onset >= forecast_dates$date_estimates_start,
           date_onset < forecast_dates$date_case_linelist - days(2))
  
  cases_bs <- map(
    1:n_bootstraps,
    \(i) case_linelist_subset %>% sample_n(n(), replace = TRUE)
  ) %>%
    bind_rows(.id = "bootstrap") %>%
    mutate(bootstrap = as.numeric(bootstrap))
  
  
  hospital_linelist_subset <- hospital_linelist  %>%
    
    filter(date_onset >= forecast_dates$date_estimates_start,
           date_onset < forecast_dates$date_hospital_linelist - days(2)) %>% 
    
    arrange(date_admit) %>%
    group_by(person_id) %>%
    slice(1) %>%
    ungroup()
  
  hosp_bs <- map(
    1:n_bootstraps,
    \(i) hospital_linelist_subset %>% sample_n(n(), replace = TRUE)
  ) %>%
    bind_rows(.id = "bootstrap") %>%
    mutate(bootstrap = as.numeric(bootstrap))
  
  
  prob_estimates <- left_join(
    cases_bs %>%
      count(bootstrap, age_group, date_onset,
            name = "n_case"),
    
    hosp_bs %>%
      count(bootstrap, age_group, date_onset,
            name = "n_hosp"),
    
    by = c("bootstrap", "age_group", "date_onset")
  ) %>%
    left_join(
      hosp_bs %>%
        filter(!is.na(date_ICU_admit)) %>% 
        count(bootstrap, age_group, date_onset,
              name = "n_ICU"),
      
      by = c("bootstrap", "age_group", "date_onset")
    ) %>% 
    
    filter(date_onset < min(forecast_dates$date_hospital_linelist, forecast_dates$date_case_linelist) - days(2)) %>% 
    
    complete(bootstrap, age_group,
             date_onset = seq(min(date_onset), max(date_onset), by = "days"),
             fill = list(n_case = 0, n_hosp = 0, n_ICU = 0)) %>%
    
    arrange(date_onset) %>% 
    group_by(bootstrap, age_group) %>%
    
    mutate(
      #n_hosp = pmin(n_hosp, n_case),

      pr_hosp = zoo::rollsum(n_hosp, 7, fill = "extend") / zoo::rollsum(n_case, 7, fill = "extend"),
      pr_ICU = zoo::rollsum(n_ICU, 7, fill = "extend") / zoo::rollsum(n_hosp, 7, fill = "extend"),

      n_cases_smooth = zoo::rollsum(n_case, 7, fill = "extend")
      
      # pr_hosp = n_hosp / n_case,
      # pr_ICU = n_ICU / n_hosp
    ) %>%
    
    group_by(bootstrap, date_onset) %>%
    
    mutate(
      pr_age_given_case = n_cases_smooth / sum(n_cases_smooth)
      #pr_age_given_case = n_case / sum(n_case)
    ) %>%
    
    ungroup() %>%
    
    mutate(pr_ICU = if_else(is.nan(pr_ICU), 0, pr_ICU)) %>%
    
    select(bootstrap, age_group, date_onset, pr_hosp, pr_ICU, pr_age_given_case)
  
  prob_estimates
}








