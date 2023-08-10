library(targets)
library(tarchetypes)
library(tidyverse)
library(lubridate)
library(future.callr)

plan(callr)


source("R/forecast_dates.R")
source("R/read_nsw_cases.R")

source("R/format_linelists.R")

source("R/get_time_varying_estimates.R")
source("R/make_case_trajectory.R")

source("R/calculate_occupancy.R")

source("R/sample_outputs.R")

source("R/plot_outputs.R")
source("R/plot_outputs_aged.R")
source("R/plot_time_varying_estimates.R")

source("R/read_interim_data.R")


scenarios <- tribble(
  ~scenario_label, ~scenario_name, ~scenario_column,
  "xbb_null", "XBB.1.16 No waning or seasonality", rlang::sym("XBB.1.16 No waning or seasonality"),
  "xbb_no_season", "XBB.1.16 Waning no seasonality", rlang::sym("XBB.1.16 Waning no seasonality"),
  "xbb_waning_season_10", "XBB.1.16 Waning, 10% Seasonal effect", rlang::sym("XBB.1.16 Waning, 10% Seasonal effect"),
  "xbb_waning_season_20", "XBB.1.16 Waning, 20% Seasonal effect", rlang::sym("XBB.1.16 Waning, 20% Seasonal effect"),
)


targets_map <- tar_map(
  values = scenarios,
  names = scenario_label,
  
  tar_target(
    case_forecast,
    read_csv(forecast_path, show_col_types = FALSE) %>% select(date_onset = Date, count = scenario_column) %>% mutate(date_onset = dmy(date_onset))
  ),
  
  tar_target(case_trajectory, make_case_trajectory(case_forecast, ascertainment, case_linelist, forecast_dates, scenario_label, plot_dir)),
  
  tar_target(outputs, sample_outputs(case_trajectory, hospital_linelist, ED_linelist, time_varying_estimates, forecast_dates, ascertainment) %>%
               mutate(scenario_label = scenario_label))
  
)

list(
  tar_target(forecast_date, ymd("2023-07-12")),
  
  
  tar_target(hospital_linelist_path, "data/NSW_out_episode_2023_07_12.xlsx"),
  tar_target(ED_linelist_path, "data/NSW_out_ED_2023_07_12.xlsx"),
  
  tar_target(forecast_path, "data/Projections_20230712r.csv"),
  tar_target(asc_path, "data/Ascertainment20230712r.csv"),
  
  
  tar_target(case_linelist, read_interim_data("data/RATs_specdate_10ygroups_20230711.csv")),
  
  tar_target(
    plot_dir,
    {
      dir <- str_c("results/fc_NSW_", forecast_date, "/")
      dir.create(dir, showWarnings = FALSE)
      return(dir)
    }
  ),
  
  
  tar_target(
    ascertainment,
    read_csv(asc_path, show_col_types = FALSE) %>%
      mutate(date_onset = dmy(Var1), asc = alpha) %>%
      select(date_onset, asc)
  ),
  
  tar_target(n_bootstraps, 50),
  
  tar_target(forecast_dates, get_forecast_dates(hospital_linelist_path, case_linelist %>% pull(date_onset) %>% max(), forecast_date)),
  
  
  tar_target(hospital_linelist_raw, readxl::read_excel(hospital_linelist_path, sheet = "NSW_out_person")),
  tar_target(ED_linelist_raw, readxl::read_excel(ED_linelist_path, sheet = "NSW_out_ED")),
  
  
  tar_target(hospital_linelist, format_hospital_linelist(hospital_linelist_raw)),
  tar_target(ED_linelist, format_ED_linelist(ED_linelist_raw)),
  
  tar_target(occupancy_data, calculate_occupancy(hospital_linelist)),
  tar_target(occupancy_data_aged, calculate_occupancy_aged(hospital_linelist)),
  
  tar_target(time_varying_estimates, get_time_varying_estimates(case_linelist, hospital_linelist, ED_linelist, forecast_dates, n_bootstraps)),
  
  
  targets_map,
  
  
  tar_combine(
    all_outputs,
    targets_map[["outputs"]],
    command = dplyr::bind_rows(!!!.x)
  ),
  
  
  tar_target(
    output_plots,
    plot_outputs(all_outputs, occupancy_data, hospital_linelist, ED_linelist, forecast_dates, plot_dir)
  ),
  
  
  tar_target(
    output_plots_aged,
    plot_outputs_aged(all_outputs, occupancy_data_aged, hospital_linelist, ED_linelist, forecast_dates, plot_dir)
  ),
  
  tar_target(
    time_varying_estimates_plots,
    plot_time_varying_estimates(time_varying_estimates, forecast_dates, plot_dir)
  )
  
  
  
)
