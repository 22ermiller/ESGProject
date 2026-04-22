# ESG ---------------------------------------------------------------------
# All draws for each variable are returned as a matrix except for yield curve draws
# Yield Curve draws are returned as a list
# Helper function get_yield_df utilized in the price_annuity function extracts the 
#      yield curve draws that are needed to price annuities based on time of purchase


ESG_draws <- function(n_years = 50, n_sims = 500) {
  
  # ---- Load required packages ----
  require(MSGARCH)
  require(rugarch)
  require(tidyverse)
  require(forecast)
  require(tsDyn)
  
  # ---- Make sure user is in the right directory ----
  
  check_file <- function(path) {
    if (!file.exists(path)) {
      stop(
        paste0(
          "Missing required input files.\n\n",
          "Expected relative paths like 'models/cpi_mod.rds' and 'data/cpi.csv'.\n",
          "Fix this by either:\n",
          "  1) Setting your working directory to the project folder, OR\n",
          "  2) Editing the file paths inside ESG_draws().\n\n",
          "Current working directory:\n  ", getwd()
        ),
        call. = FALSE
      )
    }
  }
  
  paths <- list(
    cpi_mod = "models/cpi_mod.rds",
    eci_mod = "models/eci_mod.rds",
    med_mod = "models/med_mod.rds",
    ir3mo_mod = "models/interest_garch.rds",
    cpi_data = "data/cpi.csv",
    ir3mo_data = "data/ir3mo.csv",
    full_ir_data = "data/full_ir.csv",
    functions = "functions.R"
  )
  
  invisible(lapply(paths, check_file))
  
  # ---- Load models (from package or relative path) ----
  cpi_mod <- readRDS("models/cpi_mod.rds")
  eci_mod <- readRDS("models/eci_mod.rds")
  med_mod <- readRDS("models/med_mod.rds")
  ir3mo_mod <- readRDS("models/interest_garch.rds")
  yield_var_mod <- readRDS("models/yield_curve_var_mod.rds")
  yield_lms <- readRDS("models/yield_curve_lm_mods.rds")
  equity_mean_mod <- readRDS("models/equity_mean_mod.rds")
  equity_mean_mod_resids <- readRDS("models/mean_mod_resids.rds")
  
  # ---- Load helper functions ----
  source("functions.R")
  
  # ---- Load data ----
  cpi_df <- read_csv("data/cpi.csv") |> 
    filter(date >= "2010-01-01") |> 
    mutate(lagged_cpi = lag(cpi, 1),
           log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
    filter(!is.na(log_dif_cpi))
  
  ir3mo_df <- read_csv("data/ir3mo.csv") |> 
    mutate(lagged_rate = lag(rate, 1),
           rate_rmmean = rate - mean(rate, na.rm = TRUE),
           dif_rate = rate - lagged_rate,
           log_dif_rate = log(rate) - log(lagged_rate))
  
  full_ir_df <- read_csv("data/full_ir.csv") |> 
    mutate(across(three_month:thirty_year, ~ . / 100),
           slope = thirty_year - three_month,
           curve = three_month + thirty_year - (2 * ten_year))
  
  # ---- Simulations ----
  message("Starting CPI simulations...")
  cpi_sims <- cpi_multiple_sims(cpi_mod, n_years * 12, n_sims)
  matrix_cpi_sims <- do.call(rbind, cpi_sims)
  message("Finished CPI simulations.")
  
  message("Starting ECI simulations...")
  final_cpi_val <- get_last_quarterly_cpi_val(cpi_df)
  eci_sims <- eci_multiple_sims(eci_mod, n_years, n_sims, cpi_sims, final_cpi_val)
  matrix_eci_sims <- do.call(rbind, eci_sims)
  message("Finished ECI simulations.")
  
  message("Starting medical simulations...")
  med_sims <- med_multiple_sims(med_mod, n_years * 12, n_sims)
  matrix_med_sims <- do.call(rbind, med_sims)
  message("Finished medical simulations.")
  
  message("Starting 3-month interest rate simulations...")
  mean_3mo_ir <- get_average_3mo_rate(ir3mo_df, cpi_df)
  ir3mo_sims <- ir3mo_multiple_sims(ir3mo_mod, n_years * 12, n_sims, cpi_sims, mean_3mo_ir)
  message("Finished 3-month interest rate simulations.")
  
  message("Starting yield curve simulations...")
  slope_curve_vals <- get_final_slope_curve_vals(full_ir_df)
  yield_curve_sims <- yield_multiple_sims(
    yield_var_mod, yield_lms, n_years * 12, n_sims, ir3mo_sims, slope_curve_vals
  )
  message("Finished yield curve simulations.")
  
  message("Starting equity simulations...")
  equity_rs_mod <- fit_equity_rs_model(equity_mean_mod_resids)
  equity_sims <- equity_multiple_sims(
    equity_mean_mod, equity_rs_mod, n_years * 12, n_sims, cpi_sims, ir3mo_sims
  )
  message("Finished equity simulations.")
  # ---- Return clean output ----
  return(list(
    cpi = matrix_cpi_sims,
    eci = matrix_eci_sims,
    med_cpi = matrix_med_sims,
    ir3mo = ir3mo_sims,
    yield_curve = yield_curve_sims,
    equity_returns = equity_sims
  ))
}

