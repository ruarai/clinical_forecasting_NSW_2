

plot_outputs_aged <- function(
    all_outputs,
    occupancy_data_aged,
    hospital_linelist,
    ED_linelist,
    forecast_dates,
    plot_dir
) {
  occupancy_data_aged <- occupancy_data_aged %>%
    filter(date >= ymd("2022-12-01"))
  
  
  
  source("R/make_results_quants.R")
  
  alpha_vals <- scales::rescale(rev(1/1.7^(1:3)), to = c(0.15, 0.99))
  plot_cols <- c(
    shades::opacity(ggokabeito::palette_okabe_ito(1), alpha_vals), 
    shades::opacity(ggokabeito::palette_okabe_ito(2), alpha_vals), 
    shades::opacity(ggokabeito::palette_okabe_ito(3), alpha_vals), 
    shades::opacity(ggokabeito::palette_okabe_ito(5), alpha_vals)
  )
  
  p_common <- list(
    scale_fill_manual(values = plot_cols),
    scale_colour_manual(values = plot_cols),
    geom_vline(xintercept = forecast_dates$date_case_linelist, linetype = "dashed"),
    xlab(NULL), ylab(NULL),
    theme_minimal(), theme(legend.position = "none"),
    scale_y_continuous(breaks = scales::breaks_extended(4), labels = scales::label_comma()),
    scale_x_date(breaks = scales::date_breaks("months"), labels = scales::label_date_short()),
    coord_cartesian(ylim = c(0, NA))
  )
  
  plot_data_occupancy <- all_outputs %>%
    mutate(scenario_label = factor(scenario_label, scenarios$scenario_label)) %>% 
    group_by(scenario_label, bootstrap, date, group, age_group) %>%
    summarise(count = sum(count), .groups = "drop") %>%
    pivot_wider(names_from = "bootstrap",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants(c(0.5, 0.7, 0.9))
  
  p_ward_occupancy <- plot_data_occupancy %>%
    filter(group == "ward") %>% 
    ggplot() +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper,
                    fill = interaction(quant, scenario_label),
                    group = interaction(quant, scenario_label))) +
    
    geom_point(aes(x = date, y = count),
               size = 0.9, stroke = 0, colour = "white",
               occupancy_data_aged %>% filter(group == "ward")) +
    
    geom_point(aes(x = date, y = count),
               pch = 1, size = 0.9, stroke = 0.7,
               occupancy_data_aged %>% filter(group == "ward")) +
    
    p_common +
    
    facet_wrap(~age_group) +
    
    ggtitle("Daily ward occupancy") 
  
  
  p_ICU_occupancy <- plot_data_occupancy %>%
    filter(group == "ICU") %>% 
    ggplot() +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper,
                    fill = interaction(quant, scenario_label),
                    group = interaction(quant, scenario_label))) +
    
    geom_point(aes(x = date, y = count),
               size = 0.9, stroke = 0, colour = "white",
               occupancy_data_aged %>% filter(group == "ICU")) +
    
    geom_point(aes(x = date, y = count),
               pch = 1, size = 0.9, stroke = 0.7,
               occupancy_data_aged %>% filter(group == "ICU")) +
    
    p_common +
    
    facet_wrap(~age_group) +
    
    ggtitle("Daily ICU occupancy") 
  
  hospital_linelist_subset <- hospital_linelist  %>%
    
    filter(date_onset >= forecast_dates$date_estimates_start,
           date_admit < forecast_dates$date_hospital_linelist - days(2)) %>% 
    
    arrange(date_admit) %>%
    group_by(person_id) %>%
    slice(1) %>%
    ungroup()
  
  admission_counts <- hospital_linelist_subset %>%
    count(date_admit, age_group) %>%
    filter(date_admit >= ymd("2022-12-01"))
  
  
  plot_data_admissions <- all_outputs %>%
    mutate(scenario_label = factor(scenario_label, scenarios$scenario_label)) %>%
    group_by(scenario_label, bootstrap, date, group, age_group) %>%
    summarise(admissions = sum(admissions), .groups = "drop") %>%
    pivot_wider(names_from = "bootstrap",
                names_prefix = "sim_",
                values_from = "admissions") %>%
    mutate(across(starts_with("sim_"), ~ replace_na(., 0))) %>% 
    make_results_quants(c(0.5, 0.7, 0.9))
  
  p_ward_admissions <- plot_data_admissions %>%
    filter(group == "ward") %>% 
    ggplot() +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper,
                    fill = interaction(quant, scenario_label),
                    group = interaction(quant, scenario_label))) +
    
    geom_point(aes(x = date_admit, y = n),
               size = 0.9, stroke = 0, colour = "white",
               admission_counts) +
    
    geom_point(aes(x = date_admit, y = n),
               pch = 1, size = 0.9, stroke = 0.7,
               admission_counts) +
    
    facet_wrap(~age_group) +
    
    p_common +
    
    ggtitle("Daily ward admissions") 
  
  
  
  ICU_admission_counts <- hospital_linelist_subset %>%
    count(date_ICU_admit, age_group) %>%
    filter(date_ICU_admit >= ymd("2022-12-01"))
  
  
  
  p_ICU_admissions <- plot_data_admissions %>%
    filter(group == "ICU") %>% 
    ggplot() +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper,
                    fill = interaction(quant, scenario_label),
                    group = interaction(quant, scenario_label))) +
    
    geom_point(aes(x = date_ICU_admit, y = n),
               size = 0.9, stroke = 0, colour = "white",
               ICU_admission_counts) +
    
    geom_point(aes(x = date_ICU_admit, y = n),
               pch = 1, size = 0.9, stroke = 0.7,
               ICU_admission_counts) +
    
    p_common +
    
    facet_wrap(~age_group) +
    
    ggtitle("Daily ICU admissions") 
  
  
  
  ED_linelist_subset <- ED_linelist %>%
    
    filter(date_onset >= forecast_dates$date_estimates_start,
           date_presentation < forecast_dates$date_hospital_linelist - days(2)) %>% 
    
    arrange(date_presentation) %>%
    group_by(person_id) %>%
    slice(1) %>%
    ungroup()
  
  
  ED_presentation_counts <- ED_linelist_subset %>%
    count(date_presentation, age_group) %>%
    filter(date_presentation >= ymd("2022-12-01"))
  
  
  p_ED_presentations <- plot_data_admissions %>%
    filter(group == "ED") %>% 
    ggplot() +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper,
                    fill = interaction(quant, scenario_label),
                    group = interaction(quant, scenario_label))) +
    
    geom_point(aes(x = date_presentation, y = n),
               size = 0.9, stroke = 0, colour = "white",
               ED_presentation_counts) +
    
    geom_point(aes(x = date_presentation, y = n),
               pch = 1, size = 0.9, stroke = 0.7,
               ED_presentation_counts) +
    
    facet_wrap(~age_group) +
    
    p_common +
    
    ggtitle("Daily ED presentations") 
  
  
  
  forecast_labels <- scenarios$scenario_name %>% 
    `names<-`(scenarios$scenario_label)
  
  cowplot::plot_grid(
    cowplot::plot_grid(
      p_ward_admissions, p_ward_occupancy,
      ncol = 1, align = "v"
    ),
    cowplot::get_legend(
      ggplot(plot_data_admissions %>% filter(quant == 50)) + 
        geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = scenario_label)) +
        scale_fill_manual(values = ggokabeito::palette_okabe_ito(c(1,2,3, 5)), labels = forecast_labels, name = NULL) + 
        theme(legend.position = "bottom")
    ),
    ncol = 1,
    rel_heights = c(12, 1)
  )
  
  
  ggsave(str_c(plot_dir, "/results_combined_ward_aged.png"), width = 10, height = 9, bg = "white")
  
  
  
  
  
  cowplot::plot_grid(
    cowplot::plot_grid(
      p_ICU_admissions, p_ICU_occupancy,
      ncol = 1, align = "v"
    ),
    cowplot::get_legend(
      ggplot(plot_data_admissions %>% filter(quant == 50)) + 
        geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = scenario_label)) +
        scale_fill_manual(values = ggokabeito::palette_okabe_ito(c(1,2,3, 5)), labels = forecast_labels, name = NULL) + 
        theme(legend.position = "bottom")
    ),
    ncol = 1,
    rel_heights = c(12, 1)
  )
  
  
  ggsave(str_c(plot_dir, "/results_combined_ICU_aged.png"), width = 10, height = 9, bg = "white")
  
  
  cowplot::plot_grid(
    p_ED_presentations,
    cowplot::get_legend(
      ggplot(plot_data_admissions %>% filter(quant == 50)) + 
        geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = scenario_label)) +
        scale_fill_manual(values = ggokabeito::palette_okabe_ito(c(1,2,3, 5)), labels = forecast_labels, name = NULL) + 
        theme(legend.position = "bottom")
    ),
    ncol = 1,
    rel_heights = c(12, 1)
  )
  
  ggsave(str_c(plot_dir, "/results_combined_ED_aged.png"), width = 7, height = 5, bg = "white")
  
}
