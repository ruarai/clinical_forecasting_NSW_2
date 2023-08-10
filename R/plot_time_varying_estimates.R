
plot_time_varying_estimates <- function(
  time_varying_estimates,
  forecast_dates,
  plot_dir
) {
  
  
  
  plot_data_age <- time_varying_estimates %>%
    select(bootstrap, date_onset, age_group, pr_age_given_case) %>%
    
    group_by(date_onset, age_group) %>%
    
    summarise(median = median(pr_age_given_case),
              lower_90 = quantile(pr_age_given_case, 0.05),
              upper_90 = quantile(pr_age_given_case, 0.95))
  
  
  p_age <- ggplot(plot_data_age) +
    geom_line(aes(x = date_onset, y = median, color = age_group),
              colour = ggokabeito::palette_okabe_ito(5)) +
    
    geom_ribbon(aes(x = date_onset, ymin = lower_90, ymax = upper_90),
                colour = "white",
                alpha = 0.5,
                fill = ggokabeito::palette_okabe_ito(5),
                plot_data_age) +
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    
    scale_linetype(name = "Type") +
    
    facet_wrap(~age_group) +
    
    theme_minimal() +
    ylab(NULL) + xlab("Date") +
    theme(legend.position = "none") +
    
    ggtitle("Observed age distribution of cases")
  
  
  p_age
  
  ggsave(str_c(plot_dir, "/", forecast_dates$date_forecast, "_age_distribution.png"), width = 10, height = 6, bg = "white")
  
  
  
  
  plot_data_hosp <- time_varying_estimates %>%
    select(bootstrap, date_onset, age_group, pr_hosp) %>%
    filter(date_onset >= forecast_dates$date_estimates_start) %>% 
    
    group_by(date_onset, age_group) %>%
    
    summarise(median = median(pr_hosp),
              lower_90 = quantile(pr_hosp, 0.05, na.rm = TRUE),
              upper_90 = quantile(pr_hosp, 0.95, na.rm = TRUE))
  
  
  
  
  p_hosp <- ggplot(plot_data_hosp) +
    geom_line(aes(x = date_onset, y = median),
              color = "#b53aa0") +
    
    geom_ribbon(aes(x = date_onset, ymin = lower_90, ymax = upper_90),
                fill = "#b53aa0",
                colour = "white",
                alpha = 0.3,
                plot_data_hosp) +
    
    geom_hline(yintercept = 0, colour = "grey80") +
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    
    geom_blank(aes(y = 0)) +
    
    
    facet_wrap(~age_group, scales = "free_y") +
    
    theme_minimal() +
    ylab(NULL) + xlab(NULL) +
    
    ggtitle("Observed conversion rate to hospitalisation")
  
  
  plot_data_ICU <- time_varying_estimates %>%
    select(bootstrap, date_onset, age_group, pr_ICU) %>%
    filter(date_onset >= forecast_dates$date_estimates_start) %>% 
    
    group_by(date_onset, age_group) %>%
    
    summarise(median = median(pr_ICU),
              lower_90 = quantile(pr_ICU, 0.05, na.rm = TRUE),
              upper_90 = quantile(pr_ICU, 0.95, na.rm = TRUE))
  
  
  p_ICU <- ggplot(plot_data_ICU) +
    geom_line(aes(x = date_onset, y = median),
              color = "#008200") +
    
    geom_ribbon(aes(x = date_onset, ymin = lower_90, ymax = upper_90),
                fill = "#008200",
                colour = "white",
                alpha = 0.3,
                plot_data_ICU) +
    
    geom_hline(yintercept = 0, colour = "grey80") +
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    
    geom_blank(aes(y = 0)) +
    
    
    facet_wrap(~age_group, scales = "free_y") +
    
    theme_minimal() +
    ylab(NULL) + xlab(NULL) +
    
    ggtitle("Observed probability of ICU admission")
  
  
  cowplot::plot_grid(
    p_hosp, p_ICU,
    ncol = 1, align = "v"
  )
  
  
  
  ggsave(str_c(plot_dir, "/", forecast_dates$date_forecast, "_time_varying_estimates.png"), width = 10, height = 9, bg = "white")
  
  
  
  plot_data_ED <- time_varying_estimates %>%
    select(bootstrap, date_onset, age_group, pr_ED) %>%
    filter(date_onset >= forecast_dates$date_estimates_start) %>% 
    
    group_by(date_onset, age_group) %>%
    
    summarise(median = median(pr_ED),
              lower_90 = quantile(pr_ED, 0.05, na.rm = TRUE),
              upper_90 = quantile(pr_ED, 0.95, na.rm = TRUE))
  
  
  
  
  p_ED <- ggplot(plot_data_ED) +
    geom_line(aes(x = date_onset, y = median),
              color = ggokabeito::palette_okabe_ito(2)) +
    
    geom_ribbon(aes(x = date_onset, ymin = lower_90, ymax = upper_90),
                fill = ggokabeito::palette_okabe_ito(2),
                colour = "white",
                alpha = 0.3,
                plot_data_ED) +
    
    geom_hline(yintercept = 0, colour = "grey80") +
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    
    geom_blank(aes(y = 0)) +
    
    
    facet_wrap(~age_group, scales = "free_y") +
    
    theme_minimal() +
    ylab(NULL) + xlab(NULL) +
    
    ggtitle("Observed conversion rate to ED presentation")
  
  p_ED
  
  ggsave(str_c(plot_dir, "/", forecast_dates$date_forecast, "_time_varying_estimates_ED.png"), width = 10, height = 6, bg = "white")
  
}





