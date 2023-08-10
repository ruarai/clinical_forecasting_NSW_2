

read_interim_data <- function(csv_path) {
  fix_age_group <- function(name) {
    case_when(
      is.na(name) ~ NA_character_,
      name == "[0, 10)" ~ "0-9",
      name == "[10, 20)" ~ "10-19",
      name == "[20, 30)" ~ "20-29",
      name == "[30, 40)" ~ "30-39",
      name == "[40, 50)" ~ "40-49",
      name == "[50, 60)" ~ "50-59",
      name == "[60, 70)" ~ "60-69",
      name == "[70, 80)" ~ "70-79",
      TRUE ~ "80+"
    )
  }
  
  linelist <- read_csv(csv_path) %>%
    select(c(day_specimen_date, starts_with("["))) %>% 
    pivot_longer(starts_with("["),
                 names_to = "age_group",
                 values_to = "count") %>%
    mutate(age_group = fix_age_group(age_group)) %>%
    mutate(date = dmy(day_specimen_date)) %>%
    select(date_onset = date, age_group, count) %>%
    rowwise() %>%
    summarise(date_onset = date_onset, age_group = age_group, ix = 1:count) %>%
    select(-ix) %>%
    
    drop_na(date_onset, age_group)
}