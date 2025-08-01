# Libraries

library(MSGARCH)

# Read in models

cpi_mod <- readRDS("models/cpi_mod.rds")
eci_mod <- readRDS("models/eci_mod.rds")
med_mod <- readRDS("models/med_mod.rds")
ir3mo_mod <- readRDS("models/interest_garch.rds")
yield_var_mod <- readRDS("models/yield_curve_var_mod.rds")
yield_lms <- readRDS("models/yield_curve_lm_mods.rds")
equity_mean_mod <- readRDS("models/equity_mean_mod.rds")
equity_mean_mod_resids <- readRDS("models/mean_mod_resids.rds")



# Read in functions
source("functions.R")

n_years <- 40
n_sims <- 100

# Load in Necessary Data --------------------------------------------------

cpi_df <- read_csv("data/cpi.csv") |> 
  filter(date >= "2010-01-01") |> # only dates from 2010 onwards
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
  filter(!is.na(log_dif_cpi))

ir3mo_df <- read_csv("data/ir3mo.csv") |> 
  mutate(lagged_rate = lag(rate, n = 1),
         rate_rmmean = rate - mean(rate, na.rm = TRUE)) |> 
  mutate(dif_rate = rate - lagged_rate,
         log_dif_rate = log(rate) - log(lagged_rate))

full_ir_df <- read_csv("data/full_ir.csv") |> 
  mutate(across(three_month:thirty_year, ~./100)) |> 
  mutate(slope = thirty_year - three_month,
         curve = three_month + thirty_year - (2*ten_year))


# Simulations -------------------------------------------------------------

cpi_sim <- cpi_single_sim(cpi_mod, n_years*12)
cpi_sims <- cpi_multiple_sims(cpi_mod, n_years*12, n_sims)

final_cpi_val <- get_last_quarterly_cpi_val(cpi_df)
eci_sim <- eci_single_sim(eci_mod, n_years*4, cpi_sim = cpi_sim, final_cpi_value = final_cpi_val)
eci_sims <- eci_multiple_sims(eci_mod, n_years*4, n_sims, cpi_sims, final_cpi_val)

med_sim <- med_single_sim(med_mod, n_years*12)
med_sims <- med_multiple_sims(med_mod, n_years*12, n_sims)

mean_3mo_ir <- get_average_3mo_rate(ir3mo_df)
ir3mo_sim <- ir3mo_single_sim(ir3mo_mod, n_years*12, cpi_sim, mean_3mo_ir)
ir3mo_sims <- ir3mo_multiple_sims(ir3mo_mod, n_years*12, n_sims, cpi_sims, mean_3mo_ir)

slope_curve_vals <- get_final_slope_curve_vals(full_ir_df)
yield_curve_sim <- yield_single_sim(yield_var_mod, yield_lms, n_years*12, ir3mo_sim, slope_curve_vals)

equity_rs_mod <- fit_equity_rs_model(equity_mean_mod_resids)
equity_sim <- equity_single_sim(equity_mean_mod, equity_rs_mod, n_years*12, cpi_sim, ir3mo_sim)
equity_sims <- equity_multiple_sims(equity_mean_mod, equity_rs_mod, n_years*12, n_sims, cpi_sims, ir3mo_sims)
