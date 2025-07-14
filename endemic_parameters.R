library(individual)
library(helios)
library(ggplot2)
library(dplyr)
library(parallel)
library(tidyverse)
library(magrittr)

#Parameters
archetypes <- c("flu", "sars_cov_2")
coverage_values <- c(0,0.3,0.5,0.7)
efficacy_values <- c(0,0.3,0.5,0.7)
coverage_types <- c("random", "targeted_riskiness")
iterations = 1:5

#Combination Grid
parameter_combinations <- expand.grid(
  archetype = archetypes,
  coverage = coverage_values,
  efficacy = efficacy_values,
  coverage_type = coverage_types,
  iteration = iterations,
  riskiness_settings = c("setting_specific_riskiness"),
  stringsAsFactors = FALSE
)

parameter_combinations$simulation_num <- seq_len(nrow(parameter_combinations))

parameter_lists <- list()

for (i in seq_len(nrow(parameter_combinations))) {
  row <- parameter_combinations[i, ]
  
  # Baseline parameters
  endemic_parameters <- get_parameters(archetype = row$archetype)
  endemic_parameters$archetype <- row$archetype 
  endemic_parameters$endemic_or_epidemic <- "endemic"
  endemic_parameters$duration_immune     <- 365
  endemic_parameters$prob_inf_external   <- 0.001
  endemic_parameters$number_initial_S <- 2700
  endemic_parameters$number_initial_E <- 150
  endemic_parameters$number_initial_I <- 280
  endemic_parameters$number_initial_R <- 6870
  
  # UVC intervention
  endemic_parameters <- set_uvc(
    parameters_list   = endemic_parameters,  
    setting = "joint",
    coverage = row$coverage, 
    coverage_target = "square_footage",
    coverage_type = row$coverage_type, 
    efficacy = row$efficacy,  
    timestep = 0
  )
  
  endemic_parameters <- endemic_parameters %>%
    set_setting_specific_riskiness(
      setting = "school",
      mean = 0,
      sd   = 0.3544,
      min  = 1 / sqrt(4.75),
      max  = sqrt(4.75)
    ) %>%
    set_setting_specific_riskiness(
      setting = "workplace",
      mean = 0,
      sd   = 0.5072,
      min  = 1 / sqrt(6.35),
      max  = sqrt(6.35)
    ) %>%
    set_setting_specific_riskiness(
      setting = "household",
      mean = 0,
      sd   = 0.0871,
      min  = 1 / sqrt(2.5),
      max  = sqrt(2.5)
    ) %>%
    set_setting_specific_riskiness(
      setting = "leisure",
      mean = 0,
      sd   = 0.4278,
      min  = 1 / sqrt(5.5),
      max  = sqrt(5.5)
    )

  endemic_parameters$simulation_id    <- row$simulation_num
  endemic_parameters$iteration_number <- row$iteration
  
  parameter_lists[[i]] <- endemic_parameters
}

saveRDS(parameter_lists, "endemic_parameter_list.rds")
