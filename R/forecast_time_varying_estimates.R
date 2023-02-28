


forecast_time_varying_estimates <- function(
    time_varying_estimates
) {
  
  # est_endpoints <- time_varying_estimates %>% 
  #   group_by(bootstrap) %>% 
  #   drop_na(pr_hosp) %>%
  #   mutate(date_onset_ix = sample(date_onset, 1)) %>%
  #   
  #   group_by(bootstrap, age_group) %>%
  #   summarise(pr_hosp = pr_hosp[date_onset == date_onset_ix],
  #             pr_age_given_case = pr_age_given_case[date_onset == date_onset_ix],
  #             pr_ICU = pr_ICU[date_onset == date_onset_ix],
  #             
  #             date_onset = max(time_varying_estimates$date_onset) + days(28),
  #             
  #             .groups = "drop")
  
  time_varying_estimates_forecast <- time_varying_estimates# %>% 
    # arrange(date_onset) %>% 
    # group_by(bootstrap, age_group) %>%
    # mutate(pr_hosp_sd = sd(log(pr_hosp) - lag(log(pr_hosp)), na.rm = TRUE),
    #        pr_ICU_sd = sd(qlogis(pr_ICU) - lag(qlogis(pr_ICU)), na.rm = TRUE),
    #        pr_age_sd = sd(qlogis(pr_age_given_case) - lag(qlogis(pr_age_given_case)), na.rm = TRUE)) %>% 
    # ungroup() %>%
    # 
    # filter(date_onset == max(date_onset) - days(4)) %>%
    # 
    # 
    # complete(
    #   date_onset = seq(min(date_onset), max(date_onset) + days(120), "days"),
    #   bootstrap,
    #   age_group
    # ) %>%
    # 
    # arrange(date_onset) %>% 
    # 
    # group_by(bootstrap, age_group) %>% 
    # 
    # mutate(pr_hosp = exp(log(pr_hosp[1]) + cumsum(rnorm(n(), 0, pr_hosp_sd[1]))),
    #        pr_ICU = plogis(qlogis(pr_ICU[1]) + cumsum(rnorm(n(), 0, pr_ICU_sd[1]))),
    #        pr_age_given_case = plogis(qlogis(pr_hosp[1]) + cumsum(rnorm(n(), 0, pr_age_sd[1])))) %>%
    # 
    # ungroup() %>%
    # 
    # bind_rows(
    #   time_varying_estimates %>%
    #     filter(date_onset < max(date_onset) - days(4))
    # )

  
  time_varying_estimates_forecast
}