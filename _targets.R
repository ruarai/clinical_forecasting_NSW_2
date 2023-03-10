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


scenarios <- tribble(
  ~scenario_label, ~scenario_name, ~scenario_column,
  "base", "Base", rlang::sym("Base"),
  "cts", "CTs. evol.", rlang::sym("Cts evol"),
  "486P", "486P only", rlang::sym("486P only"),
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
  tar_target(hospital_linelist_path, "~/source/email_digester/downloads/hospital_linelist/NSW_out_episode_2023_02_28.xlsx"),
  tar_target(ED_linelist_path, "../email_digester/downloads/ED_linelist/NSW_out_ED_2023_02_28.xlsx"),
  
  tar_target(case_linelist_path, "~/source/email_digester/downloads/case_linelist/20230227 - Case list - Freya Shearer.zip"),
  
  tar_target(forecast_path, "~/source/clinical_forecasting_NSW/data/cases/Projections_20230227a.csv"),
  tar_target(asc_path, "~/source/clinical_forecasting_NSW/data/cases/Ascertainment0227a.csv"),
  
  tar_target(forecast_date, ymd("2023-02-28")),
  
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
  
  tar_target(n_bootstraps, 100),
  
  tar_target(forecast_dates, get_forecast_dates(hospital_linelist_path, case_linelist_path)),
  
  
  tar_target(hospital_linelist_raw, readxl::read_excel(hospital_linelist_path, sheet = "NSW_out_episode")),
  tar_target(ED_linelist_raw, readxl::read_excel(ED_linelist_path, sheet = "NSW_out_ED")),
  tar_target(case_linelist_raw, read_nsw_cases(case_linelist_path) %>% filter(TEST_TYPE == "RAT")),
  
  
  tar_target(hospital_linelist, format_hospital_linelist(hospital_linelist_raw)),
  tar_target(ED_linelist, format_ED_linelist(ED_linelist_raw)),
  tar_target(case_linelist, format_case_linelist(case_linelist_raw)),
  
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
