# Libraries

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

n_years <- 40
n_sims <- 1000

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
eci_sim <- eci_single_sim(eci_mod, n_years*4, cpi_sim = cpi_sim, final_cpi_value = final_cpi_val)
eci_sims <- eci_multiple_sims(eci_mod, n_years*4, n_sims, cpi_sims, final_cpi_val)

med_sim <- med_single_sim(med_mod, n_years*12)
med_sims <- med_multiple_sims(med_mod, n_years*12, n_sims)

mean_3mo_ir <- get_average_3mo_rate(ir3mo_df)
ir3mo_sim <- ir3mo_single_sim(ir3mo_mod, n_years*12, cpi_sim, mean_3mo_ir)
ir3mo_sims <- ir3mo_multiple_sims(ir3mo_mod, n_years*12, n_sims, cpi_sims, mean_3mo_ir)

slope_curve_vals <- get_final_slope_curve_vals(full_ir_df)
yield_curve_sim <- yield_single_sim(yield_var_mod, yield_lms, n_years*12, ir3mo_sim, slope_curve_vals)
yield_curve_sims <- yield_multiple_sims(yield_var_mod, yield_lms, n_years*12, n_sims, ir3mo_sims, slope_curve_vals)

equity_rs_mod <- fit_equity_rs_model(equity_mean_mod_resids)
equity_sim <- equity_single_sim(equity_mean_mod, equity_rs_mod, n_years*12, cpi_sim, ir3mo_sim)
equity_sims <- equity_multiple_sims(equity_mean_mod, equity_rs_mod, n_years*12, n_sims, cpi_sims, ir3mo_sims)


# Track Cash Flow ---------------------------------------------------------

# 1 dollar applied to a single cpi simulation

dollar_value <- cumprod(exp(cpi_sim))

equity_gross <- exp(equity_sim) 
plot(equity_gross, type = "l")
mean(equity_gross)

starting_value <- 100

portfolio_value <- rep(NA, length(dollar_value))

for (i in 1:length(portfolio_value)) {
  if (i ==1) {
    portfolio_value[i] <- (starting_value - dollar_value[i])*equity_gross[i] 
  }
  else {
    portfolio_value[i] <- (portfolio_value[i-1] - dollar_value[i])*equity_gross[i]
  }
  
}

plot(portfolio_value, type = "l")


# Multiple Simulations ------------------------------------------------------



equity_gross <- exp(equity_sims)
rowMeans(equity_gross)
matrix_cpi_sims <- do.call(rbind, cpi_sims)
price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
price_level[,n_years*12]

# Set starting portfolio value
starting_value <- 100
withdrawal_amt <- starting_value*(.04/12)

T <- ncol(equity_gross)
n_sims <- nrow(equity_gross)

portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)

# Period 1
portfolio_value[, 1] <- (starting_value - price_level[, 1]*withdrawal_amt) * equity_gross[, 1]

# Remaining periods
for (t in 2:T) {
  portfolio_value[, t] <- (portfolio_value[, t-1] - price_level[, t]*withdrawal_amt) * equity_gross[, t]
  portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
}

#portfolio_value <- pmax(portfolio_value, 0)

port_avg <- colMeans(portfolio_value)

port_ci <- apply(portfolio_value, 2, quantile, probs = c(.05, .95))

plot(port_avg, type = "l", ylim = c(0, 4000), main = "Portfolio Value", xlab = "Month")
lines(port_ci[2,], col = "red", lty = 2)
lines(port_ci[1,], col = "red", lty = 2)


# Calculate failure rates -------------------------------------------------

# Mortality

death_ages <- sample(
  mortality_tbl$month,
  size = n_sims,
  replace = TRUE,
  prob = mortality_tbl$death_pdf
)

hist(death_ages, breaks = 71)

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
mean(portfolio_df$success_no_death)

portfolio_df |> count(success)
portfolio_df |> count(success_no_death)


# Annuity Prices ----------------------------------------------------------

price <- price_annuity(start_age = 60, yield_curve_sim, mortality_tbl, 1, .2)
prices <- price_annuities(start_age = 60, yield_curve_sims, mortality_tbl, 1, .1, 1000)


# Portfolio Value w/ annuity and equities ---------------------------------

# 1 dollar applied to a single cpi simulation

dollar_value <- cumprod(exp(cpi_sim))

equity_gross <- exp(equity_sim) 
plot(equity_gross, type = "l")
mean(equity_gross)

starting_value <- 100

# 60% stock 40% annuities

annuity_amt <- starting_value*.4
stock_market_amt <- starting_value - annuity_amt

monthly_annuity_payout <- annuity_amt / price

withdrawal_amt <- starting_value*(.04/12) - monthly_annuity_payout


portfolio_value <- rep(NA, length(dollar_value))

for (i in 1:length(portfolio_value)) {
  if (i ==1) {
    portfolio_value[i] <- (stock_market_amt - dollar_value[i]*withdrawal_amt)*equity_gross[i] 
  }
  else {
    portfolio_value[i] <- (portfolio_value[i-1] - dollar_value[i]*withdrawal_amt)*equity_gross[i]
  }
  
}

plot(portfolio_value, type = "l")

# Multiple sims

equity_gross <- exp(equity_sims)
rowMeans(equity_gross)
matrix_cpi_sims <- do.call(rbind, cpi_sims)
price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
price_level[,n_years*12]

# Set starting portfolio value
starting_value <- 100

# 60% stock 40% annuities
annuity_amt <- starting_value*.5
stock_market_amt <- starting_value - annuity_amt

monthly_annuity_payout <- rep(annuity_amt, n_sims) / prices

withdrawal_amt <- rep(starting_value, n_sims)*rep((.08/12), n_sims) - monthly_annuity_payout


T <- ncol(equity_gross)
n_sims <- nrow(equity_gross)

portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)

# Period 1
portfolio_value[, 1] <- (stock_market_amt - price_level[, 1]*withdrawal_amt) * equity_gross[, 1]

# Remaining periods
for (t in 2:T) {
  portfolio_value[, t] <- (portfolio_value[, t-1] - price_level[, t]*withdrawal_amt) * equity_gross[, t]
  portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
}

port_avg <- colMeans(portfolio_value)

port_ci <- apply(portfolio_value, 2, quantile, probs = c(.05, .95))

plot(port_avg, type = "l", ylim = c(0, 4000), main = "Portfolio Value", xlab = "Month")
lines(port_ci[2,], col = "red", lty = 2)
lines(port_ci[1,], col = "red", lty = 2)

# Mortality

death_ages <- sample(
  mortality_tbl$month,
  size = n_sims,
  replace = TRUE,
  prob = mortality_tbl$death_pdf
)

hist(death_ages, breaks = 71)

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
mean(portfolio_df$success_no_death)

portfolio_df |> count(success)
portfolio_df |> count(success_no_death)

portfolio_sim <- function(annuity_prop, withdrawal_rate, annuity_prices, equity_sims, cpi_sims, death_ages) {
  
  # Multiple sims
  
  equity_gross <- exp(equity_sims)
  rowMeans(equity_gross)
  matrix_cpi_sims <- do.call(rbind, cpi_sims)
  price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
  price_level[,n_years*12]
  
  # Set starting portfolio value
  starting_value <- 100
  
  # 60% stock 40% annuities
  annuity_amt <- starting_value*annuity_prop
  stock_market_amt <- starting_value - annuity_amt
  
  monthly_annuity_payout <- rep(annuity_amt, n_sims) / prices
  
  withdrawal_amt <- rep(starting_value, n_sims)*rep((withdrawal_rate/12), n_sims) - monthly_annuity_payout
  
  
  T <- ncol(equity_gross)
  n_sims <- nrow(equity_gross)
  
  portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)
  
  # Period 1
  portfolio_value[, 1] <- (stock_market_amt - price_level[, 1]*withdrawal_amt) * equity_gross[, 1]
  
  # Remaining periods
  for (t in 2:T) {
    portfolio_value[, t] <- (portfolio_value[, t-1] - price_level[, t]*withdrawal_amt) * equity_gross[, t]
    portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
  }
  
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
  
  success_rate <- mean(portfolio_df$success)
  success_rate_no_death <- mean(portfolio_df$success_no_death)
  
  return(list(success = success_rate, success_no_death = success_rate_no_death))
}


death_ages <- sample(
  mortality_tbl$month,
  size = n_sims,
  replace = TRUE,
  prob = mortality_tbl$death_pdf
)

portfolio_sim(annuity_prop = .4, withdrawal_rate = .08, annuity_prices, equity_sims, cpi_sims, death_ages)

df <- expand_grid(
  withdrawal_rate = seq(.01, .2, by = .001),
  annuity_prop = seq(0, 1, by = .1)  # Adjust range and step as needed
) %>%
  mutate(success_rates = pmap(list(annuity_prop, withdrawal_rate),
                                 ~portfolio_sim(annuity_prop = ..1, 
                                                withdrawal_rate = ..2, 
                                                annuity_prices, 
                                                equity_sims, 
                                                cpi_sims,
                                                death_ages))) %>%
  mutate(
    success_rate     = map_dbl(success_rates, 1),  # first element
    success_no_death = map_dbl(success_rates, 2)   # second element
  )

# heat map of success rates

ggplot(data = df) +
  geom_tile(aes(x = annuity_prop, y = withdrawal_rate, fill = success_rate))


ggplot(data = df) +
  geom_tile(aes(x = annuity_prop, y = withdrawal_rate, fill = success_no_death))

ggplot(data = df) +
  geom_line(aes(x = withdrawal_rate, y = success_rate, color = factor(annuity_prop)))

ggplot(data = df) +
  geom_line(aes(x = withdrawal_rate, y = success_no_death, color = factor(annuity_prop)))

