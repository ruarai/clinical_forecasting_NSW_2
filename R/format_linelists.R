

format_case_linelist <- function(case_linelist_raw, forecast_dates) {
  source("R/age_groups.R")
  
  case_linelist_raw %>%
    mutate(age_group = assign_10yr_age_group(AGE_AT_EVENT_YEARS)) %>% 
    select(date_onset = CALCULATED_ONSET_DATE,
           age_group) %>% 
    
    drop_na(age_group, date_onset) %>%
    
    filter(
      date_onset >= ymd("2022-01-01")
    )
  
}

format_hospital_linelist <- function(hospital_linelist_raw, forecast_dates) {
  source("R/age_groups.R")
  
  hospital_linelist_raw %>%
    mutate(age_group = assign_10yr_age_group(age),
           date_onset = as_date(ncims_calc_onset_dt)) %>% 
    
    drop_na(age_group, date_onset) %>%
    
    select(person_id,
           date_onset,
           age_group,
           
           date_admit = admit_date,
           date_discharge = discharge_date,
           
           date_ICU_admit = first_icu_date,
           date_ICU_discharge = last_icu_date) %>%
    
    filter(
      date_onset >= ymd("2022-01-01")
    ) %>% 
    
    mutate(
      date_admit = as_date(date_admit),
      date_discharge = as_date(date_discharge),
      date_ICU_admit = as_date(date_ICU_admit),
      date_ICU_discharge = as_date(date_ICU_discharge),
      
      
      date_admit = pmax(date_admit, date_onset),
      date_ICU_admit = pmax(date_ICU_admit, date_onset)
    ) %>% 
    
    mutate(
      drop_ICU = date_ICU_discharge < date_ICU_admit,
      date_ICU_admit = if_else(drop_ICU, NA_Date_, date_ICU_admit),
      date_ICU_discharge = if_else(drop_ICU, NA_Date_, date_ICU_discharge)
    ) %>%
    select(-drop_ICU) %>% 
    
    filter(date_discharge >= date_admit)
}
