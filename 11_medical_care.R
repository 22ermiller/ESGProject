## Portfolio Simulation with Variable Medical Expenses ##
## Script contains derivation of gamma distributions for different health risk levels
## Implement Long Term Care costs
## Run Simulation to show that accounting for medical costs in this way decreases overall portfolio success rate


library(tidyverse)

# medical cost data from https://personal1.vanguard.com/pdf/ISGPLHC_072021.pdf

annual_health_cost <- tibble(
  risk_level = c("Low", "Med", "High"),
  q05 = c(2700,3100,3700),
  q25 = c(2900,3500,4600),
  q50 = c(3100,4000,5700),
  q75 = c(3600,5000,7900),
  q95 = c(5100,8100,15100)
) %>%
  mutate(across(q05:q95, ~./12))

# create distribution

obj <- function(par, qs){
  
  k <- par[1]
  theta <- par[2]
  q <- qgamma(c(.05,.25,.5,.75,.95), shape = k, scale = theta)
  sum((q - qs)^2)
}

low_fit <- optim(par = c(2,100),fn = obj,qs = low_qs,method = "L-BFGS-B",lower = c(.001,.001))

# apply optim to each risk level
fits <- annual_health_cost %>%
  pivot_longer(cols = starts_with("q"),names_to = "quantile",values_to = "cost") %>%
  group_by(risk_level) %>%
  summarise(
    fit = list(optim(par = c(2,100),fn = obj,qs = cost[quantile %in% c("q05","q25","q50","q75","q95")],method = "L-BFGS-B",lower = c(.001,.001)))
  ) %>%
  #extract k_hat and theta_hat
  mutate(
    k_hat = map_dbl(fit, ~ .x$par[1]),
    theta_hat = map_dbl(fit, ~ .x$par[2])
  )

x <- seq(0,2000,by = 10)

low_fit_parms <- fits %>% filter(risk_level == "Low") %>%
  select(k_hat, theta_hat)
gamma_density_low <- dgamma(x, shape = low_fit_parms$k_hat, scale = low_fit_parms$theta_hat)

med_fit_parms <- fits %>% filter(risk_level == "Med") %>%
  select(k_hat, theta_hat)
gamma_density_med <- dgamma(x, shape = med_fit_parms$k_hat, scale = med_fit_parms$theta_hat)

high_fit_parms <- fits %>% filter(risk_level == "High") %>%
  select(k_hat, theta_hat)
gamma_density_high <- dgamma(x, shape = high_fit_parms$k_hat, scale = high_fit_parms$theta_hat)

qgamma(c(.05,.25,.5,.75,.95), shape = high_fit_parms$k_hat, scale = high_fit_parms$theta_hat)
annual_health_cost %>% filter(risk_level == "High")

# plot densities
plot(x, gamma_density_low, type = "l", main = "Gamma Density for Different Risk Groups", xlab = "Monthly Health Cost", ylab = "Density")
lines(x, gamma_density_med, col = "red")
lines(x, gamma_density_high, col = "blue")


# Long Term Care ----------------------------------------------------------

ltc_probs <- tibble(
  lower = c(0,1,10000,25000,50000,75000,100000,150000,200000,250000),
  upper = c(0,9999,24999,49999,74999,99999,149999,199999,249999,350000),
  p = c(.52,.06,.05,.04,.03,.03,.04,.03,.02,.18)
)

row <- sample(1:nrow(ltc_probs), size = 1, prob = ltc_probs$p, replace = TRUE)

total_ltc <- runif(1000,ltc_probs$lower[row],ltc_probs$upper[row])


# Implement ESG -----------------------------------------------------------

library(MSGARCH)
library(rugarch)
library(tidyverse)
library(forecast)
library(tsDyn)

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

n_years <- 50
n_sims <- 500

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

mortality_tbl <- read_csv("data/mortality.csv") |> filter(!is.na(death_pdf))


# Simulations -------------------------------------------------------------

cpi_sim <- cpi_single_sim(cpi_mod, n_years*12)
cpi_sims <- cpi_multiple_sims(cpi_mod, n_years*12, n_sims)

final_cpi_val <- get_last_quarterly_cpi_val(cpi_df)
# eci_sim <- eci_single_sim(eci_mod, n_years, cpi_sim = cpi_sim, final_cpi_value = final_cpi_val)
# eci_sims <- eci_multiple_sims(eci_mod, n_years, n_sims, cpi_sims, final_cpi_val)

med_sim <- med_single_sim(med_mod, n_years*12)
med_sims <- med_multiple_sims(med_mod, n_years*12, n_sims)

mean_3mo_ir <- get_average_3mo_rate(ir3mo_df)
ir3mo_sim <- ir3mo_single_sim(ir3mo_mod, n_years*12, cpi_sim, mean_3mo_ir)
ir3mo_sims <- ir3mo_multiple_sims(ir3mo_mod, n_years*12, n_sims, cpi_sims, mean_3mo_ir)

# slope_curve_vals <- get_final_slope_curve_vals(full_ir_df)
# yield_curve_sim <- yield_single_sim(yield_var_mod, yield_lms, n_years*12, ir3mo_sim, slope_curve_vals)
# yield_curve_sims <- yield_multiple_sims(yield_var_mod, yield_lms, n_years*12, n_sims, ir3mo_sims, slope_curve_vals)

equity_rs_mod <- fit_equity_rs_model(equity_mean_mod_resids)
equity_sim <- equity_single_sim(equity_mean_mod, equity_rs_mod, n_years*12, cpi_sim, ir3mo_sim)
equity_sims <- equity_multiple_sims(equity_mean_mod, equity_rs_mod, n_years*12, n_sims, cpi_sims, ir3mo_sims)

#prices <- price_annuities(start_age = 60, yield_curve_sims, mortality_tbl, 1, 1, .1, n_sims)

# Get Monthly Medical Expenses

monthly_medical <- matrix(rgamma(n_years*12*n_sims, shape = med_fit_parms$k_hat, scale = med_fit_parms$theta_hat),
                            nrow = n_sims,
                            ncol = n_years*12)
monthly_medical_noltc <- monthly_medical

# Get LTC expenses
row <- sample(1:nrow(ltc_probs), size = n_sims, prob = ltc_probs$p, replace = TRUE)

total_ltc <- runif(n_sims,ltc_probs$lower[row],ltc_probs$upper[row])

# assume ltc starts 3 years before death
monthly_ltc <- total_ltc/(12*3)

# get death ages

death_ages <- get_death_ages(60, mortality_tbl, n_sims)

# add ltc to medical expenses
for(i in 1:n_sims){
  
  death_m <- death_ages[i] - (60*12)
  
  start_m <- max(1, death_m - 3*12)
  
  monthly_medical[i, start_m:death_m] <-
    monthly_medical[i, start_m:death_m] + monthly_ltc[i]
  
}

# Multiple sims

equity_gross <- exp(equity_sims)
rowMeans(equity_gross)
matrix_cpi_sims <- do.call(rbind, cpi_sims)
price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
end_cpi <- price_level[,n_years*12]


matrix_med_sims <- do.call(rbind, med_sims)
med_price_level <- t(apply(matrix_med_sims, 1, function(x) cumprod(exp(x))))
end_med_inflation <- med_price_level[,n_years*12]

# Set starting portfolio value
#starting_value <- 10000

# 60% stock 0% annuities
# annuity_amt <- starting_value*0
# stock_market_amt <- starting_value - annuity_amt
# 
# monthly_annuity_payout <- rep(annuity_amt, n_sims) / prices

#withdrawal_amt <- (rep(starting_value, n_sims)*rep((.05/12), n_sims) - monthly_annuity_payout)*.8

year1_medical <- rowSums(monthly_medical_noltc[,1:12])

withdrawal_amt <- rep(35000/12,n_sims)
withdrawal_rate <- .05
starting_value <- (withdrawal_amt*12 + year1_medical)/withdrawal_rate
stock_market_amt <- starting_value # - annuity_amt

# 35,000 in annual non-medical expenses excess of Social Security 
# 39,000 when treating as "all expenses" excess of Social Security

T <- ncol(equity_gross)
n_sims <- nrow(equity_gross)

portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)

# Period 1
portfolio_value[, 1] <- (stock_market_amt -
                           (price_level[, 1]*withdrawal_amt + med_price_level[,1]*monthly_medical[,1]))*equity_gross[, 1]

# Remaining periods
for (t in 2:T) {
  portfolio_value[, t] <- (portfolio_value[, t-1] - (price_level[, 1]*withdrawal_amt + med_price_level[,1]*monthly_medical[,1])) * equity_gross[, t]
  portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
}

port_avg <- colMeans(portfolio_value)

port_ci <- apply(portfolio_value, 2, quantile, probs = c(.05, .95))

plot(port_avg, type = "l", ylim = c(0, 10000000), main = "Portfolio Value", xlab = "Month")
lines(port_ci[2,], col = "red", lty = 2)
lines(port_ci[1,], col = "red", lty = 2)

# Mortality

failure_month_vec <- apply(portfolio_value, 1, function(x) {
  month <- which(x == 0)[1] # get 1st value where portfolio hits 0
  if (is.na(month)) NA else month  # handle portfolios that never hit 0
})


portfolio_df <- as_tibble(portfolio_value) %>%
  mutate(death_month = ifelse(death_ages > 60*12, death_ages-(61*12), 0), # retirement starts at age 60 (people that die before 60 are automatically "successes")
         failure_month = failure_month_vec,
         success = death_month <= failure_month | is.na(failure_month)) %>%  # portfolio is success if death_month is less than failure_month
  select(death_month:success, everything()) %>%
  mutate(success_no_death = ifelse(.[[ncol(.)]] > 0, TRUE, FALSE))

mean(portfolio_df$success)

portfolio_df |> count(success)


# Build Functions ---------------------------------------------------------

get_medical_expenses <- function(med_fit_parms, health_risk, n_years, n_sims) {
  # Get Monthly Medical Expenses
  risk_level_parms <- med_fit_parms %>% filter(risk_level == health_risk) %>%
    select(k_hat, theta_hat)
  
  monthly_medical <- matrix(rgamma(n_years*12*n_sims, shape = risk_level_parms$k_hat, scale = risk_level_parms$theta_hat),
                            nrow = n_sims,
                            ncol = n_years*12)
  return(monthly_medical)
}



simulation_variable_medical <- function(yearly_expenses, withdrawal_rate, health_risk,
                                        equity_sims, cpi_sims, med_sims, death_ages) {
  
  monthly_medical <- get_medical_expenses(fits, health_risk, n_years, n_sims)
  
  # Get LTC expenses
  row <- sample(1:nrow(ltc_probs), size = n_sims, prob = ltc_probs$p, replace = TRUE)
  
  total_ltc <- runif(n_sims,ltc_probs$lower[row],ltc_probs$upper[row])
  
  # assume ltc starts 3 years before death
  monthly_ltc <- total_ltc/(12*3)
  
  # add ltc to medical expenses
  for(i in 1:n_sims){
    
    death_m <- death_ages[i] - (60*12)
    
    start_m <- max(1, death_m - 3*12)
    
    monthly_medical[i, start_m:death_m] <-
      monthly_medical[i, start_m:death_m] + monthly_ltc[i]
    
  }
  
  # Multiple sims
  
  equity_gross <- exp(equity_sims)
  matrix_cpi_sims <- do.call(rbind, cpi_sims)
  price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
  end_cpi <- price_level[,n_years*12]
  
  
  matrix_med_sims <- do.call(rbind, med_sims)
  med_price_level <- t(apply(matrix_med_sims, 1, function(x) cumprod(exp(x))))
  end_med_inflation <- med_price_level[,n_years*12]
  
  #withdrawal_amt <- (rep(starting_value, n_sims)*rep((.05/12), n_sims) - monthly_annuity_payout)*.8
  year1_medical_expense <- rowSums(monthly_medical[,1:12])
  year1_expense <- rep(yearly_expenses, n_sims) + year1_medical_expense
  withdrawal_amt <- yearly_expenses/12
  starting_value <- (yearly_expenses+4000)/withdrawal_rate
  stock_market_amt <- starting_value
  
  # get average expenses
  
  avg_expenses <- colMeans(price_level*withdrawal_amt + med_price_level*monthly_medical)
  
  T <- ncol(equity_gross)
  n_sims <- nrow(equity_gross)
  
  portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)
  
  # Period 1
  portfolio_value[, 1] <- (stock_market_amt -
                             (price_level[, 1]*withdrawal_amt + med_price_level[,1]*monthly_medical[,1]))*equity_gross[, 1]
  
  # Remaining periods
  for (t in 2:T) {
    portfolio_value[, t] <- (portfolio_value[, t-1] - (price_level[, t]*withdrawal_amt + med_price_level[,t]*monthly_medical[,t])) * equity_gross[, t]
    portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
  }
  
  port_avg <- colMeans(portfolio_value)
  
  port_ci <- apply(portfolio_value, 2, quantile, probs = c(.05, .95))
  
  failure_month_vec <- apply(portfolio_value, 1, function(x) {
    month <- which(x == 0)[1] # get 1st value where portfolio hits 0
    if (is.na(month)) NA else month  # handle portfolios that never hit 0
  })
  
  death_month_vec <- death_ages - 60*12
  
  # Clamp death months to max simulation length
  death_month_vec <- pmin(death_month_vec, ncol(portfolio_value))
  
  # Value at death (vectorized)
  value_at_death <- portfolio_value[cbind(1:n_sims, death_month_vec)]
  
  # Success indicators
  success <- (death_month_vec < failure_month_vec) | is.na(failure_month_vec)
  success_no_death <- portfolio_value[, ncol(portfolio_value)] > 0
  
  # End-of-life expected value ignoring death
  expected_value_no_death <- mean(portfolio_value[, -1])
  
  # Success rates
  success_rate <- mean(success)
  success_rate_no_death <- mean(success_no_death)
  expected_value_at_death <- mean(value_at_death)
  value_at_death_ci <- quantile(value_at_death, probs = c(.05, .5, .95))
  value_no_death_ci <- quantile(portfolio_value[, -1], probs = c(.05, .5, .95))
  
  return(list(
    success = success_rate,
    success_no_death = success_rate_no_death,
    expected_value_at_death = expected_value_at_death,
    expected_value_no_death = expected_value_no_death,
    value_at_death_ci = value_at_death_ci,
    value_no_death_ci = value_no_death_ci,
    avg_expenses = avg_expenses
  ))
}

death_ages <- get_death_ages(60, mortality_tbl, n_sims)

# 35,000 in annual non-medical expenses excess of Social Security 
# 39,000 when treating as "all expenses" excess of Social Security

test <- simulation_variable_medical(yearly_expenses = 35000, withdrawal_rate = .04, health_risk = "High",
                                        equity_sims = equity_sims, cpi_sims = cpi_sims, med_sims = med_sims, death_ages = death_ages)
test

df <- expand_grid(
  withdrawal_rate = seq(.01, .2, by = .001),
  health_risk_level = c("Low", "Med", "High")  # Adjust range and step as needed
)

library(progressr)

with_progress({
  
  p <- progressor(along = 1:nrow(df))
  
  df <- df %>%
    mutate(success_rates = pmap(list(withdrawal_rate, health_risk_level),
                                ~{
                                  p()
                                  simulation_variable_medical(yearly_expenses = 35000,
                                                    withdrawal_rate = ..1, 
                                                    health_risk = ..2,
                                                    equity_sims, 
                                                    cpi_sims,
                                                    med_sims,
                                                    death_ages)
                                }))
})

final_df <- df %>%
  mutate(
    success_rate        = map_dbl(success_rates, "success"),
    success_no_death    = map_dbl(success_rates, "success_no_death"),
    mean_value_at_death = map_dbl(success_rates, "expected_value_at_death"),
    mean_value_no_death = map_dbl(success_rates, "expected_value_no_death"),
    value_at_death_p05  = map_dbl(success_rates, ~ .x$value_at_death_ci[["5%"]]),
    value_at_death_p50  = map_dbl(success_rates, ~ .x$value_at_death_ci[["50%"]]),
    value_at_death_p95  = map_dbl(success_rates, ~ .x$value_at_death_ci[["95%"]]),
    value_no_death_p05  = map_dbl(success_rates, ~ .x$value_no_death_ci[["5%"]]),
    value_no_death_p50  = map_dbl(success_rates, ~ .x$value_no_death_ci[["50%"]]),
    value_no_death_p95  = map_dbl(success_rates, ~ .x$value_no_death_ci[["95%"]])
  )

ggplot(data = final_df) +
  geom_line(
    aes(
      x = withdrawal_rate,
      y = success_rate,
      color = factor(health_risk_level)
    ),
    linewidth = 0.3
  )



# Just CPI, no medical costs considered -----------------------------------

portfolio_sim_vec <- function(yearly_expenses, withdrawal_rate, equity_sims, cpi_sims, death_ages) {
  
  n_sims <- nrow(equity_sims)
  T <- ncol(equity_sims)
  
  # Convert log returns to gross returns
  equity_gross <- exp(equity_sims)
  
  # Price level from CPI simulations
  matrix_cpi_sims <- do.call(rbind, cpi_sims)
  price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
  
  
  # Starting portfolio
  withdrawal_amt <- rep(yearly_expenses/12,n_sims)
  starting_value <- yearly_expenses/withdrawal_rate
  stock_market_amt <- starting_value
  
  # get average expenses
  
  avg_expenses <- colMeans(price_level * withdrawal_amt)
  
  # Initialize portfolio matrix
  portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)
  
  # Period 1
  portfolio_value[, 1] <- (stock_market_amt - ((price_level[, 1] * withdrawal_amt))) * equity_gross[, 1]
  
  # Don't allow value to go less than 0
  portfolio_value[, 1] <- pmax(portfolio_value[, 1], 0)
  
  # Remaining periods
  for (t in 2:T) {
    portfolio_value[, t] <- (portfolio_value[, t-1] - ((price_level[, t] * withdrawal_amt))) * equity_gross[, t]
    
    portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
  }
  
  # First month portfolio hits 0
  failure_month_vec <- apply(portfolio_value, 1, function(x) {
    m <- which(x == 0)[1]
    if (is.na(m)) NA else m
  })
  
  # Death month (relative to retirement at 60)
  death_month_vec <- death_ages - 60*12
  
  # Clamp death months to max simulation length
  death_month_vec <- pmin(death_month_vec, ncol(portfolio_value))
  
  # Value at death (vectorized)
  value_at_death <- portfolio_value[cbind(1:n_sims, death_month_vec)]
  
  # Success indicators
  success <- (death_month_vec <= failure_month_vec) | is.na(failure_month_vec)
  success_no_death <- portfolio_value[, ncol(portfolio_value)] > 0
  # temp
  #success_no_death <- portfolio_value[, 360] > 0
  
  
  # End-of-life expected value ignoring death
  expected_value_no_death <- mean(portfolio_value[, -1])
  
  # Success rates
  success_rate <- mean(success)
  success_rate_no_death <- mean(success_no_death)
  expected_value_at_death <- mean(value_at_death)
  value_at_death_ci <- quantile(value_at_death, probs = c(.05, .5, .95))
  value_no_death_ci <- quantile(portfolio_value[, -1], probs = c(.05, .5, .95))
  
  return(list(
    success = success_rate,
    success_no_death = success_rate_no_death,
    expected_value_at_death = expected_value_at_death,
    expected_value_no_death = expected_value_no_death,
    value_at_death_ci = value_at_death_ci,
    value_no_death_ci = value_no_death_ci,
    avg_expenses = avg_expenses
  ))
}

test_base <- portfolio_sim_vec(yearly_expenses = 39000, withdrawal_rate = .04,
                                    equity_sims = equity_sims, cpi_sims = cpi_sims, death_ages = death_ages)


df_base <- expand_grid(
  withdrawal_rate = seq(.01, .2, by = .001)
)

with_progress({
  
  p <- progressor(along = 1:nrow(df_base))
  
  df_base <- df_base %>%
    mutate(success_rates = pmap(list(withdrawal_rate),
                                ~{
                                  p()
                                  portfolio_sim_vec(yearly_expenses = 39000,
                                                              withdrawal_rate = ..1,
                                                              equity_sims, 
                                                              cpi_sims,
                                                              death_ages)
                                }))
})

final_df_base <- df_base %>%
  mutate(
    success_rate        = map_dbl(success_rates, "success"),
    success_no_death    = map_dbl(success_rates, "success_no_death"),
    mean_value_at_death = map_dbl(success_rates, "expected_value_at_death"),
    mean_value_no_death = map_dbl(success_rates, "expected_value_no_death"),
    value_at_death_p05  = map_dbl(success_rates, ~ .x$value_at_death_ci[["5%"]]),
    value_at_death_p50  = map_dbl(success_rates, ~ .x$value_at_death_ci[["50%"]]),
    value_at_death_p95  = map_dbl(success_rates, ~ .x$value_at_death_ci[["95%"]]),
    value_no_death_p05  = map_dbl(success_rates, ~ .x$value_no_death_ci[["5%"]]),
    value_no_death_p50  = map_dbl(success_rates, ~ .x$value_no_death_ci[["50%"]]),
    value_no_death_p95  = map_dbl(success_rates, ~ .x$value_no_death_ci[["95%"]]),
    health_risk_level = "base"
  )

ggplot(data = final_df) +
  geom_line(
    aes(
      x = withdrawal_rate,
      y = success_rate,
      color = factor(health_risk_level)
    ),
    linewidth = 0.3
  ) + 
  geom_line(data = final_df_base,
    aes(
      x = withdrawal_rate,
      y = success_rate
    ),
    linewidth = 0.3
  ) 
  
