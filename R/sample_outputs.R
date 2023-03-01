
sample_outputs <- function(
    case_trajectory,
    hospital_linelist,
    ED_linelist,
    time_varying_estimates,
    forecast_dates,
    ascertainment
) {
  
  
  
  source("R/age_groups.R")
  
  ward_days <- hospital_linelist %>%
    
    filter(date_admit < forecast_dates$date_hospital_linelist - days(14)) %>% 
    
    select(person_id, age_group, 
           date_onset, date_admit, date_discharge) %>%
    
    rowwise() %>%
    mutate(date = list(as_date(seq(date_admit, date_discharge, "days")))) %>%
    
    unnest(date) %>%
    
    select(person_id, age_group, date_onset, date) %>%
    mutate(days = as.numeric(date - date_onset)) %>%
    arrange(days)
  
  
  
  ward_days_counts <- ward_days %>% 
    mutate(days = if_else(days < 0 | days > 14, NA_real_, days)) %>% 
    distinct(age_group, person_id, days) %>% 
    
    group_by(age_group, person_id) %>%
    summarise(days = list(days), .groups = "drop") %>%
    
    count(age_group, days) %>%
    mutate(p = n / sum(n))
  
  time_varying_estimates_expand <- time_varying_estimates %>%
    filter(date_onset >= min(case_trajectory$date_onset), date_onset <= max(case_trajectory$date_onset)) %>% 
    complete(
      date_onset = seq(min(case_trajectory$date_onset), max(case_trajectory$date_onset), "days"),
      bootstrap,
      age_group
    ) %>%
    arrange(date_onset) %>%
    group_by(bootstrap, age_group) %>%
    fill(pr_hosp, pr_ICU, pr_ED, pr_age_given_case, .direction = "downup")
  
  asc_expand <- ascertainment %>%
    filter(date_onset >= min(case_trajectory$date_onset), date_onset <= max(case_trajectory$date_onset)) %>% 
    complete(
      date_onset = seq(min(case_trajectory$date_onset), max(case_trajectory$date_onset), "days")
    ) %>%
    arrange(date_onset) %>%
    fill(asc, .direction = "downup")
  
  
  patient_tbl <- case_trajectory %>%
    left_join(time_varying_estimates_expand, by = "date_onset") %>%
    left_join(asc_expand, by = "date_onset") %>%
    
    mutate(n_hosp = count * pr_age_given_case * pr_hosp * asc) %>% 
    select(bootstrap, date_onset, age_group, n_hosp) %>%
    
    mutate(n_hosp = if_else(n_hosp >= runif(n()), n_hosp + 0.5, n_hosp - 0.5) %>% round()) %>% 
    
    rowwise() %>%
    mutate(patient = list(seq_len(round(n_hosp))))  %>%
    unnest(patient)
  
  
  sampled_ward <- map(
    age_groups,
    function(i_age) {
      patient_tbl_age <- patient_tbl %>% 
        filter(age_group == i_age)
      
      bind_cols(
        patient_tbl_age,
        sample_n(
          ward_days_counts %>% filter(age_group == i_age),
          nrow(patient_tbl_age),
          replace = TRUE,
          weight = p
        ) %>%
          ungroup() %>%
          select(days)
      )
    } 
  ) %>%
    bind_rows()
  
  sampled_ward_counts <- sampled_ward %>%
    unnest(days) %>%
    
    drop_na(days) %>%
    
    count(age_group, bootstrap, date = date_onset + days) %>%
    complete(date = seq(min(date), max(date), "days"),
             age_group,
             bootstrap,
             fill = list(n = 0)) %>% 
    
    filter(date <= max(case_trajectory$date_onset),
           date >= forecast_dates$date_estimates_start + days(14))
  
  
  sampled_ward_admissions <- sampled_ward %>%
    rowwise() %>% 
    mutate(days = days[first(which(!is.na(days)))]) %>% 
    unnest(days) %>% 
    drop_na(days) %>%
    mutate(date = date_onset + days) %>% 
    count(bootstrap, age_group, date)
  
  
  
  ICU_days <- hospital_linelist %>%
    
    filter(date_admit < forecast_dates$date_hospital_linelist - days(14)) %>%
    filter(!is.na(date_ICU_admit)) %>%
    
    select(person_id, age_group, 
           date_onset, date_ICU_admit, date_ICU_discharge) %>%
    
    rowwise() %>%
    mutate(date = list(as_date(seq(date_ICU_admit, date_ICU_discharge, "days")))) %>%
    
    unnest(date) %>%
    
    select(person_id, age_group, date_onset, date) %>% 
    mutate(days = as.numeric(date - date_onset)) %>%
    arrange(days)
  
  
  
  
  ICU_days_counts <- ICU_days %>% 
    mutate(days = if_else(days < 0 | days > 14, NA_real_, days)) %>% 
    distinct(age_group, person_id, days) %>% 
    
    group_by(age_group, person_id) %>%
    summarise(days = list(days), .groups = "drop") %>%
    
    count(age_group, days) %>%
    mutate(p = n / sum(n))
  
  
  
  ICU_patient_tbl <- case_trajectory %>%
    left_join(time_varying_estimates_expand, by = "date_onset") %>%
    left_join(asc_expand, by = "date_onset") %>%
    
    mutate(n_ICU = count * pr_age_given_case * pr_hosp * pr_ICU * asc) %>% 
    select(bootstrap, date_onset, age_group, n_ICU) %>%
    
    mutate(n_ICU = if_else(n_ICU >= runif(n()), n_ICU + 0.5, n_ICU - 0.5) %>% round()) %>% 
    
    rowwise() %>%
    mutate(patient = list(seq_len(round(n_ICU)))) %>%
    unnest(patient)
  
  
  
  
  sampled_ICU <- map(
    age_groups,
    function(i_age) {
      patient_tbl_age <- ICU_patient_tbl %>% 
        filter(age_group == i_age)
      
      bind_cols(
        patient_tbl_age,
        sample_n(
          ICU_days_counts %>% filter(age_group == i_age),
          nrow(patient_tbl_age),
          replace = TRUE,
          weight = p
        ) %>%
          ungroup() %>%
          select(days)
      )
    } 
  ) %>%
    bind_rows()
  
  sampled_ICU_counts <- sampled_ICU %>%
    unnest(days) %>%
    
    drop_na(days) %>% 
    
    count(age_group, bootstrap, date = date_onset + days) %>%
    
    complete(date = seq(min(date), max(date), "days"),
             age_group,
             bootstrap,
             fill = list(n = 0)) %>% 
    
    filter(date <= max(case_trajectory$date_onset),
           date >= forecast_dates$date_estimates_start + days(14))
  
  
  sampled_ICU_admissions <- sampled_ICU %>%
    rowwise() %>% 
    mutate(days = days[first(which(!is.na(days)))]) %>% 
    unnest(days) %>% 
    drop_na(days) %>%
    mutate(date = date_onset + days) %>% 
    count(bootstrap, age_group, date)
  
  
  
  ED_days <- ED_linelist %>%
    
    filter(date_presentation < forecast_dates$date_hospital_linelist - days(14)) %>%
    
    select(person_id, age_group, 
           date_onset, date = date_presentation) %>%
    
    select(person_id, age_group, date_onset, date) %>% 
    mutate(days = as.numeric(date - date_onset)) %>%
    arrange(days)
  
  
  ED_days_counts <- ED_days %>% 
    mutate(days = if_else(days < 0 | days > 14, NA_real_, days)) %>% 
    distinct(age_group, person_id, days) %>% 
    
    count(age_group, days) %>%
    mutate(p = n / sum(n))
  
  
  
  
  ED_patient_tbl <- case_trajectory %>%
    left_join(time_varying_estimates_expand, by = "date_onset") %>%
    left_join(asc_expand, by = "date_onset") %>%
    
    mutate(n_ED = count * pr_age_given_case * pr_ED * asc) %>% 
    select(bootstrap, date_onset, age_group, n_ED) %>%
    
    mutate(n_ED = if_else(n_ED >= runif(n()), n_ED + 0.5, n_ED - 0.5) %>% round()) %>% 
    
    rowwise() %>%
    mutate(patient = list(seq_len(n_ED))) %>%
    unnest(patient)
  
  sampled_ED <- map(
    age_groups,
    function(i_age) {
      patient_tbl_age <- ED_patient_tbl %>% 
        filter(age_group == i_age)
      
      bind_cols(
        patient_tbl_age,
        sample_n(
          ED_days_counts %>% filter(age_group == i_age),
          nrow(patient_tbl_age),
          replace = TRUE,
          weight = p
        ) %>%
          ungroup() %>%
          select(days)
      )
    } 
  ) %>%
    bind_rows()
  
  
  sampled_ED_presentations <- sampled_ED %>%
    drop_na(days) %>%
    mutate(date = date_onset + days) %>% 
    count(bootstrap, age_group, date) %>% 
    
    filter(date <= max(case_trajectory$date_onset),
           date >= forecast_dates$date_estimates_start + days(14))
  
  
  
  bind_rows(
    sampled_ward_counts %>%
      rename(count = n) %>% 
      mutate(group = "ward"),
    sampled_ICU_counts  %>%
      rename(count = n) %>% 
      mutate(group = "ICU")
  ) %>%
    left_join(
      bind_rows(
        sampled_ward_admissions %>%
          rename(admissions = n) %>% 
          mutate(group = "ward"),
        sampled_ICU_admissions %>%
          rename(admissions = n) %>% 
          mutate(group = "ICU")
      ),
      
      by = c("date", "age_group", "bootstrap", "group")
    ) %>%
    
    bind_rows(
      sampled_ED_presentations %>%
        rename(admissions = n) %>% 
        mutate(group = "ED", count = 0) %>%
        complete(date = seq(min(sampled_ward_counts$date), max(sampled_ward_counts$date), "days"),
                 age_group,
                 fill = list(admissions = 0))
    ) %>% 
    
    select(bootstrap, date, age_group, group, count, admissions) %>%
    
    mutate(
      admissions = replace_na(admissions, 0),
      count = replace_na(count, 0),
    )
}





