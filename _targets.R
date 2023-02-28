library(targets)
library(tarchetypes)
library(tidyverse)
library(lubridate)
library(future.callr)


source("R/forecast_dates.R")
source("R/read_nsw_cases.R")

source("R/format_linelists.R")

source("R/get_time_varying_estimates.R")
source("R/make_case_trajectory.R")
source("R/forecast_time_varying_estimates.R")

source("R/calculate_occupancy.R")

source("R/sample_outputs.R")


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
  
  tar_target(case_trajectory, make_case_trajectory(case_forecast, ascertainment, case_linelist, forecast_dates)),
  
  tar_target(outputs, sample_outputs(case_trajectory, hospital_linelist, time_varying_estimates_forecast, forecast_dates, ascertainment) %>%
               mutate(scenario_label = scenario_label))
  
)

list(
  tar_target(hospital_linelist_path, "~/source/email_digester/downloads/hospital_linelist/NSW_out_episode_2023_02_28.xlsx"),
  tar_target(case_linelist_path, "~/source/email_digester/downloads/case_linelist/20230227 - Case list - Freya Shearer.zip"),
  
  tar_target(forecast_path, "~/source/clinical_forecasting_NSW/data/cases/Projections_20230227a.csv"),
  tar_target(asc_path, "~/source/clinical_forecasting_NSW/data/cases/Ascertainment0227a.csv"),
  
  
  tar_target(
    ascertainment,
    read_csv(asc_path, show_col_types = FALSE) %>%
      mutate(date_onset = dmy(Var1), asc = alpha) %>%
      select(date_onset, asc)
  ),
  
  tar_target(n_bootstraps, 100),
  
  tar_target(forecast_dates, get_forecast_dates(hospital_linelist_path, case_linelist_path)),
  
  
  tar_target(hospital_linelist_raw, readxl::read_excel(hospital_linelist_path, sheet = "NSW_out_episode")),
  tar_target(case_linelist_raw, read_nsw_cases(case_linelist_path) %>% filter(TEST_TYPE == "RAT")),
  
  
  tar_target(hospital_linelist, format_hospital_linelist(hospital_linelist_raw, forecast_dates)),
  tar_target(case_linelist, format_case_linelist(case_linelist_raw, forecast_dates)),
  
  tar_target(occupancy_data, calculate_occupancy(hospital_linelist)),
  tar_target(occupancy_data_aged, calculate_occupancy_aged(hospital_linelist)),
  
  tar_target(time_varying_estimates, get_time_varying_estimates(case_linelist, hospital_linelist, forecast_dates, n_bootstraps)),
  
  tar_target(time_varying_estimates_forecast, forecast_time_varying_estimates(time_varying_estimates)),
  
  targets_map,
  
  
  tar_combine(
    all_outputs,
    targets_map[["outputs"]],
    command = dplyr::bind_rows(!!!.x)
  )
  
  
  
)
