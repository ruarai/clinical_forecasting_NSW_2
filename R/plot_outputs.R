

library(tidyverse)
library(lubridate)
library(targets)

all_outputs <- tar_read(all_outputs)
occupancy_data <- tar_read(occupancy_data) %>%
  filter(date >= ymd("2022-12-01"))
occupancy_data_aged <- tar_read(occupancy_data_aged) %>%
  filter(date >= ymd("2022-12-01"))

forecast_dates <- tar_read(forecast_dates)


make_results_quants <- function(tbl, probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .)
  
  quant_probs <- c(rev(1 - probs) / 2, 0.5 + probs / 2)
  quant_names <- c(str_c("lower_", rev(probs) * 100), str_c("upper_", probs * 100))
  
  quants <- data_matrix %>%
    matrixStats::rowQuantiles(probs = quant_probs) %>%
    `colnames<-`(quant_names) %>%
    as_tibble() %>%
    bind_cols(id_tbl, .) %>%
    pivot_longer(cols = -all_of(colnames(id_tbl)),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}

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
  group_by(scenario_label, bootstrap, date, group) %>%
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
             occupancy_data %>% filter(group == "ward")) +
  
  geom_point(aes(x = date, y = count),
             pch = 1, size = 0.9, stroke = 0.7,
             occupancy_data %>% filter(group == "ward")) +
  
  p_common +
  
  ggtitle("Daily ward occupancy") 


p_ICU_occupancy <- plot_data_occupancy %>%
  filter(group == "ICU") %>% 
  ggplot() +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper,
                  fill = interaction(quant, scenario_label),
                  group = interaction(quant, scenario_label))) +
  
  geom_point(aes(x = date, y = count),
             size = 0.9, stroke = 0, colour = "white",
             occupancy_data %>% filter(group == "ICU")) +
  
  geom_point(aes(x = date, y = count),
             pch = 1, size = 0.9, stroke = 0.7,
             occupancy_data %>% filter(group == "ICU")) +
  
  p_common +
  
  ggtitle("Daily ICU occupancy") 

p_ICU_occupancy


hospital_linelist <- tar_read(hospital_linelist)

forecast_dates <- tar_read(forecast_dates)

hospital_linelist_subset <- hospital_linelist  %>%
  
  filter(date_onset >= forecast_dates$date_estimates_start,
         date_admit < forecast_dates$date_hospital_linelist - days(2)) %>% 
  
  arrange(date_admit) %>%
  group_by(person_id) %>%
  slice(1) %>%
  ungroup()

admission_counts <- hospital_linelist_subset %>%
  count(date_admit) %>%
  filter(date_admit >= ymd("2022-12-01"))


plot_data_admissions <- all_outputs %>%
  mutate(scenario_label = factor(scenario_label, scenarios$scenario_label)) %>%
  group_by(scenario_label, bootstrap, date, group) %>%
  summarise(admissions = sum(admissions), .groups = "drop") %>%
  pivot_wider(names_from = "bootstrap",
              names_prefix = "sim_",
              values_from = "admissions") %>%
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
  
  p_common +
  
  ggtitle("Daily ward admissions") 



ICU_admission_counts <- hospital_linelist_subset %>%
  count(date_ICU_admit) %>%
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
  
  ggtitle("Daily ICU admissions") 





forecast_labels <- scenarios$scenario_name %>% 
  `names<-`(scenarios$scenario_label)

cowplot::plot_grid(
  cowplot::plot_grid(
    p_ward_admissions, p_ward_occupancy, p_ICU_occupancy,
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


ggsave("results/fc_NSW_2023-02-28/results_combined.png", width = 7, height = 8, bg = "white")





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
