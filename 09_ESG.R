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

n_years <- 50
n_sims <- 10000

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
eci_sim <- eci_single_sim(eci_mod, n_years, cpi_sim = cpi_sim, final_cpi_value = final_cpi_val)
eci_sims <- eci_multiple_sims(eci_mod, n_years, n_sims, cpi_sims, final_cpi_val)

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

# wage value

wage_value <- rep(cumprod(exp(eci_sim)), each = 3)

# 20 year old without any savings, starting salary of 60,000
# expenses are 4000 per month
# extra is invested in stock market

monthly_expenses <- 4000
monthly_revenue <- 60000/12

starting_value <- monthly_revenue

portfolio_value <- rep(NA, length(dollar_value))

for (i in 1:length(portfolio_value)) {
  # starting month
  if (i ==1) {
    portfolio_value[i] <- (monthly_revenue*wage_value[i] - monthly_expenses*dollar_value[i])*equity_gross[i]
  }
  # employment
  else if (i < 40*12) {
    portfolio_value[i] <- (portfolio_value[i-1] 
                           + monthly_revenue*wage_value[i]
                           - monthly_expenses*dollar_value[i])*equity_gross[i]
  }
  # retirement
  else {
    portfolio_value[i] <- (portfolio_value[i-1]
                           - monthly_expenses*dollar_value[i])*equity_gross[i]
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

death_ages <- get_death_ages(60, mortality_tbl, n_sims)

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

price <- price_annuity(start_age = 60, yield_curve_sim, mortality_tbl, 480, 1, .2)

prices_test <- map_dbl(seq(1:960), ~price_annuity(start_age = 60, yield_curve_sim, mortality_tbl, .x, 1, .2))



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
end_cpi <- price_level[,n_years*12]


matrix_med_sims <- do.call(rbind, med_sims)
price_level <- t(apply(matrix_med_sims, 1, function(x) cumprod(exp(x))))
end_med_inflation <- price_level[,n_years*12]

# overlay density plots of end_cpi and end_med_inflation
plot(density(end_cpi), main = "Inflation Comparison after 40 years", xlab = "Index Value", ylim = c(0,1.7))
lines(density(end_med_inflation), col = "red")
legend("topright", legend = c("CPI", "Medical Inflation"), col = c("black", "red"), lty = 1)


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


portfolio_sim_vec <- function(annuity_prop, withdrawal_rate, annuity_prices, equity_sims, cpi_sims, med_sims, death_ages) {
  
  n_sims <- nrow(equity_sims)
  T <- ncol(equity_sims)
  
  # Convert log returns to gross returns
  equity_gross <- exp(equity_sims)
  
  # Price level from CPI simulations
  matrix_cpi_sims <- do.call(rbind, cpi_sims)
  price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
  matrix_med_sims <- do.call(rbind, med_sims)
  med_price_level <- t(apply(matrix_med_sims, 1, function(x) cumprod(exp(x))))
  
  
  # Starting portfolio
  starting_value <- 100
  annuity_amt <- starting_value * annuity_prop
  stock_market_amt <- starting_value - annuity_amt
  monthly_annuity_payout <- rep(annuity_amt, n_sims) / annuity_prices
  withdrawal_amt <- rep(starting_value, n_sims) * (withdrawal_rate / 12)
  
  # Initialize portfolio matrix
  portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)
  
  # Period 1
  portfolio_value[, 1] <- (stock_market_amt - ((price_level[, 1] * withdrawal_amt * .8) + 
                                                 (med_price_level[, 1] * withdrawal_amt * .2)) + 
                             monthly_annuity_payout) * equity_gross[, 1]
  
  # Don't allow value to go less than 0
  portfolio_value[, 1] <- pmax(portfolio_value[, 1], 0)
  
  # Remaining periods
  for (t in 2:T) {
    portfolio_value[, t] <- (portfolio_value[, t-1] - ((price_level[, t] * withdrawal_amt * .82) + 
                                                         (med_price_level[, t] * withdrawal_amt * .18))
                             + monthly_annuity_payout) * equity_gross[, t]
    
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
    value_no_death_ci = value_no_death_ci
  ))
}


prices <- price_annuities(start_age = 60, yield_curve_sims, mortality_tbl, 1, 1, .1, n_sims)

death_ages <- get_death_ages(60, mortality_tbl, n_sims)

test <- portfolio_sim_vec(annuity_prop = .5, withdrawal_rate = .05, prices, equity_sims, cpi_sims, med_sims, death_ages)

df <- expand_grid(
  withdrawal_rate = seq(.01, .2, by = .001),
  annuity_prop = seq(0, 1, by = .1)  # Adjust range and step as needed
)

library(progressr)

with_progress({
  
  p <- progressor(along = 1:nrow(df))
  
   df <- df %>%
    mutate(success_rates = pmap(list(annuity_prop, withdrawal_rate),
                                ~{
                                  p()
                                  portfolio_sim_vec(annuity_prop = ..1, 
                                               withdrawal_rate = ..2, 
                                               prices, 
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

# heat map of success rates

ggplot(data = final_df) +
  geom_tile(aes(x = withdrawal_rate, y = annuity_prop, fill = success_rate))


ggplot(data = final_df) +
  geom_tile(aes(x = withdrawal_rate, y = annuity_prop, fill = success_no_death))



## Line Plot that we are using ##
ggplot(data = final_df) +
  geom_line(
    aes(
      x = withdrawal_rate,
      y = success_rate,
      color = factor(annuity_prop)
    ),
    linewidth = 0.3
  ) +
  labs(
    x = "Withdrawal Rate",
    y = "Success Rate",
    color = "Annuity Proportion",
    title = "Portfolio Success Rates"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 12) +
  scale_color_discrete(
    labels = function(x) scales::percent(as.numeric(as.character(x)))
  )
  # scale_color_viridis_d(
  #   labels = function(x) scales::percent(as.numeric(as.character(x))),
  #   option = "viridis"
  # )
  # scale_color_brewer(
  #   labels = function(x) scales::percent(as.numeric(as.character(x))),
  #   palette = "Set1"
  # )
  
ggplot(data = final_df %>% filter(between(withdrawal_rate, 0.03,.049))) +
  geom_line(
    aes(
      x = withdrawal_rate,
      y = success_rate,
      color = factor(annuity_prop)
    ),
    linewidth = 0.3
  ) +
  labs(
    x = "Withdrawal Rate",
    y = "Success Rate",
    color = "Annuity Proportion",
    title = "Portfolio Success Rates for Withdrawal Rates Between 3% and 5%"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 12) +
  scale_color_discrete(
    labels = function(x) scales::percent(as.numeric(as.character(x)))
  )



ggplot(data = final_df %>% filter(annuity_prop %in% c(0,1))) + 
  geom_line(aes(x = withdrawal_rate, y = success_rate, color = factor(annuity_prop)))
  






ggplot(data = final_df) +
  geom_line(aes(x = withdrawal_rate, y = success_no_death, color = factor(annuity_prop)))

ggplot(data = final_df) +
  geom_line(aes(x = withdrawal_rate, y = value_no_death_p50, color = factor(annuity_prop))) +
  labs(title = "Median End Portfolio Value")
  # geom_line(aes(x = withdrawal_rate, y = value_no_death_p05, color = factor(annuity_prop)), linetype = "dashed") +
  # geom_line(aes(x = withdrawal_rate, y = value_no_death_p95, color = factor(annuity_prop)), linetype = "dashed")

ggplot(data = final_df %>%
         filter(between(withdrawal_rate, 0.03,.049))) +
  geom_line(aes(x = withdrawal_rate, y = value_at_death_p50, color = factor(annuity_prop))) +
  labs(title = "Median Portfolio Value at Death")
  #geom_line(aes(x = withdrawal_rate, y = value_at_death_p05, color = factor(annuity_prop)), linetype = "dashed") +
  #geom_line(aes(x = withdrawal_rate, y = value_at_death_p95, color = factor(annuity_prop)), linetype = "dashed")




# Adding in Wages ---------------------------------------------------------

portfolio_sim_vec_wages <- function(annuity_prop, monthly_revenue, monthly_expenses, annuity_prices,
                                    equity_sims, cpi_sims, med_sims, eci_sims, death_ages, years_working) {
  
  n_sims <- nrow(equity_sims)
  T <- ncol(equity_sims)
  
  # Convert log returns to gross returns
  equity_gross <- exp(equity_sims)
  
  # Price level from CPI simulations
  matrix_cpi_sims <- do.call(rbind, cpi_sims)
  price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
  matrix_med_sims <- do.call(rbind, med_sims)
  med_price_level <- t(apply(matrix_med_sims, 1, function(x) cumprod(exp(x))))
  
  # Wage level from ECI simulations
  matrix_eci_sims <- do.call(rbind, eci_sims)
  wage_value <- t(apply(matrix_eci_sims, 1, function(x) cumprod(exp(x))))
  wage_value <- t(apply(wage_value, 1, function(x) {
    n_years <- length(x)
    monthly <- numeric(n_years * 12)
    
    for (i in 1:(n_years - 1)) {
      # endpoints for this year
      y0 <- x[i]
      y1 <- x[i + 1]
      
      # 12 monthly points (including first month = y0)
      monthly_segment <- seq(y0, y1, length.out = 13)[-1]
      
      # fill into output
      monthly[((i - 1) * 12 + 1):(i * 12)] <- monthly_segment
    }
    
    # last year gets flat because no next point
    monthly[(n_years - 1) * 12 + 1] <- x[n_years]
    monthly[((n_years - 1) * 12 + 1):(n_years * 12)] <- x[n_years]
    
    monthly
  }))
  
  # Initialize portfolio matrix
  portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)
  
  # Period 1
  portfolio_value[, 1] <- (monthly_revenue*wage_value[, 1] - monthly_expenses*price_level[, 1])*equity_gross[, 1]
  
  # initialize vector to keep track of savings rates
  savings_rates <- matrix(NA, nrow = n_sims, ncol = years_working*12)
  savings_rates[, 1] <- (monthly_revenue*wage_value[, 1] - monthly_expenses*price_level[, 1])/(monthly_revenue*wage_value[, 1])
  
  # Employment
  for (t in 2:(years_working*12)) {
    portfolio_value[, t] <- (portfolio_value[, t-1] 
                             + monthly_revenue*wage_value[, t]
                             - monthly_expenses*price_level[, t])*equity_gross[, t]
    portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
    
    savings_rates[, t] <- (monthly_revenue*wage_value[, t] - monthly_expenses*price_level[, t])/(monthly_revenue*wage_value[, t])
  }
  
  
  nest_egg <- portfolio_value[, years_working*12]
  annuity_amt <- nest_egg * annuity_prop
  portfolio_value[, years_working*12] <- nest_egg - annuity_amt
  monthly_annuity_payout <- annuity_amt/ annuity_prices
  
  
  
  withdrawal_rates <- ifelse(nest_egg == 0, NA,
           (price_level[, years_working*12] * monthly_expenses * 12) / nest_egg)
  
  
  summary(nest_egg)
  print(paste0("Average Monthly expenses at retirement: ", mean(price_level[, years_working*12] * monthly_expenses)))
  print(paste0("Median Withdrawal rate at retirement: ", quantile(withdrawal_rates, .5, na.rm = TRUE)))
  
  
  # Remaining periods
  for (t in ((years_working*12) + 1):T) {
    portfolio_value[, t] <- (portfolio_value[, t-1] - ((price_level[, t] * monthly_expenses * .82) + 
                                                         (med_price_level[, t] * monthly_expenses * .18))
                             + monthly_annuity_payout) * equity_gross[, t]
  }
  
  # Never allow negative values
  portfolio_value <- pmax(portfolio_value, 0)
  
  # First month portfolio hits 0
  failure_month_vec <- apply(portfolio_value, 1, function(x) {
    m <- which(x == 0)[1]
    if (is.na(m)) NA else m
  })
  
  # Death month (relative to starting age of 20
  death_month_vec <- ifelse(death_ages > 20*12, death_ages - 20*12, 0)
  
  # Clamp death months to max simulation length
  death_month_vec <- pmin(death_month_vec, ncol(portfolio_value))
  death_month_vec[death_month_vec == 0] <- 1  # pre-60 deaths → use first month
  
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
    savings_rates = savings_rates,
    success = success_rate,
    success_no_death = success_rate_no_death,
    expected_value_at_death = expected_value_at_death,
    expected_value_no_death = expected_value_no_death,
    value_at_death_ci = value_at_death_ci,
    value_no_death_ci = value_no_death_ci
  ))
}

# purchase annuity in 40 years (at age 60)
prices <- price_annuities(start_age = 60, yield_curve_sims, mortality_tbl, 480, 1, 0, 1000)

death_ages <- sample(
  mortality_tbl$month,
  size = n_sims,
  replace = TRUE,
  prob = mortality_tbl$death_pdf
)

test <- portfolio_sim_vec_wages(annuity_prop = 0, monthly_revenue = 2000, monthly_expenses = 2000,
                                prices, equity_sims, cpi_sims, med_sims, eci_sims, death_ages, years_working = 40)

portfolio_sim_vec_wages_fixed_savings_rate <- function(annuity_prop, monthly_revenue, savings_rate, annuity_prices,
                                    equity_sims, cpi_sims, med_sims, eci_sims, death_ages, years_working, wr = FALSE) {
  
  n_sims <- nrow(equity_sims)
  T <- ncol(equity_sims)
  
  # Convert log returns to gross returns
  equity_gross <- exp(equity_sims)
  
  # Price level from CPI simulations
  matrix_cpi_sims <- do.call(rbind, cpi_sims)
  price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
  matrix_med_sims <- do.call(rbind, med_sims)
  med_price_level <- t(apply(matrix_med_sims, 1, function(x) cumprod(exp(x))))
  
  # Wage level from ECI simulations
  matrix_eci_sims <- do.call(rbind, eci_sims)
  wage_value <- t(apply(matrix_eci_sims, 1, function(x) cumprod(exp(x))))
  wage_value <- t(apply(wage_value, 1, function(x) {
    n_years <- length(x)
    monthly <- numeric(n_years * 12)
    
    for (i in 1:(n_years - 1)) {
      # endpoints for this year
      y0 <- x[i]
      y1 <- x[i + 1]
      
      # 12 monthly points (including first month = y0)
      monthly_segment <- seq(y0, y1, length.out = 13)[-1]
      
      # fill into output
      monthly[((i - 1) * 12 + 1):(i * 12)] <- monthly_segment
    }
    
    # last year gets flat because no next point
    monthly[(n_years - 1) * 12 + 1] <- x[n_years]
    monthly[((n_years - 1) * 12 + 1):(n_years * 12)] <- x[n_years]
    
    monthly
  }))
  
  # Initialize portfolio matrix
  portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)
  
  # Period 1
  portfolio_value[, 1] <- (monthly_revenue*wage_value[, 1]*savings_rate)*equity_gross[, 1]
  
  
  # Employment
  for (t in 2:(years_working*12)) {
    portfolio_value[, t] <- (portfolio_value[, t-1] 
                             + monthly_revenue*wage_value[, t]*savings_rate)*equity_gross[, t]
    portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
  }
  
  
  nest_egg <- portfolio_value[, years_working*12]
  annuity_amt <- nest_egg * annuity_prop
  monthly_annuity_payout <- annuity_amt/ annuity_prices
  portfolio_value[, years_working*12] <- nest_egg - annuity_amt + monthly_annuity_payout
  
  if(wr) {
    monthly_retirement_spending <- monthly_revenue*wage_value[, years_working*12]*(1-savings_rate)
    withdrawal_rates <- ifelse(nest_egg == 0, NA,
                               (price_level[, years_working*12] * monthly_retirement_spending * 12) / nest_egg)
    mean_withdrawal_rate <- mean(withdrawal_rates, na.rm = TRUE)
    monthly_retirement_spending <- portfolio_value[, years_working*12] * (mean_withdrawal_rate / 12)
    median_withdrawal_rate <- NA
  } else {
    monthly_retirement_spending <- monthly_revenue*wage_value[, years_working*12]*(1-savings_rate)
    
    # calculate withdrawal rate for reference
    withdrawal_rates <- ifelse(nest_egg == 0, NA,
                               (price_level[, years_working*12] * monthly_retirement_spending * 12) / nest_egg)
    
    mean_withdrawal_rate <- mean(withdrawal_rates, na.rm = TRUE)
    median_withdrawal_rate <- quantile(withdrawal_rates, .5, na.rm = TRUE)
  }
  
  
  
  summary(nest_egg)
  #print(paste0("Average Monthly expenses at retirement: ", mean(monthly_retirement_spending)))
  #print(paste0("Mean Withdrawal rate at retirement: ", quantile(withdrawal_rates, .5, na.rm = TRUE)))
  
  
  # Remaining periods
  for (t in ((years_working*12) + 1):T) {
    portfolio_value[, t] <- (portfolio_value[, t-1] - ((price_level[, t] * monthly_retirement_spending * .82) + 
                                                         (med_price_level[, t] * monthly_retirement_spending * .18))
                             + monthly_annuity_payout) * equity_gross[, t]
    # Never allow negative values
    portfolio_value[,t] <- pmax(portfolio_value[,t], 0)
  }
  
  # First month portfolio hits 0
  failure_month_vec <- apply(portfolio_value, 1, function(x) {
    m <- which(x == 0)[1]
    if (is.na(m)) NA else m
  })
  
  # Death month (relative to starting age of 20)
  death_month_vec <- death_ages - 20*12
  
  # Clamp death months to max simulation length
  death_month_vec <- pmin(death_month_vec, ncol(portfolio_value))
  
  # Value at death (vectorized)
  value_at_death <- portfolio_value[cbind(1:n_sims, death_month_vec)]
  
  # Success indicators
  success <- (death_month_vec <= failure_month_vec) | is.na(failure_month_vec)
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
    mean_withdrawal_rate = mean_withdrawal_rate,
    median_withdrawal_rate = median_withdrawal_rate,
    success = success_rate,
    success_no_death = success_rate_no_death,
    expected_value_at_death = expected_value_at_death,
    expected_value_no_death = expected_value_no_death,
    value_at_death_ci = value_at_death_ci,
    value_no_death_ci = value_no_death_ci
  ))
}

prices <- price_annuities(start_age = 60, yield_curve_sims, mortality_tbl, 40*12, 1, 0, 1000)

test <- portfolio_sim_vec_wages_fixed_savings_rate(annuity_prop = 1, monthly_revenue = 1, savings_rate = .01,
                                prices, equity_sims, cpi_sims, med_sims, eci_sims, death_ages, years_working = 40, wr = FALSE)

library(progressr)

df2 <- expand_grid(
  savings_rate = seq(.01, .5, by = .01),
  annuity_prop = seq(0, 1, by = .1)  # Adjust range and step as needed
)

with_progress({
  
  p <- progressor(along = 1:nrow(df2))

  df2 <- df2 %>%
    mutate(success_rates = pmap(list(annuity_prop, savings_rate),
                                ~{
                                  p()
                                  portfolio_sim_vec_wages_fixed_savings_rate(annuity_prop = ..1, 
                                                    monthly_revenue = 1, 
                                                    savings_rate = ..2,
                                                    prices, 
                                                    equity_sims, 
                                                    cpi_sims,
                                                    med_sims,
                                                    eci_sims,
                                                    death_ages,
                                                    years_working = 40,
                                                    wr = FALSE)
                                }))
})

final_df2 <- df2 %>%
  mutate(
    success_rate        = map_dbl(success_rates, "success"),
    success_no_death    = map_dbl(success_rates, "success_no_death"),
    mean_withdrawal_rate = map_dbl(success_rates, "mean_withdrawal_rate"),
    median_withdrawal_rate = map_dbl(success_rates, "median_withdrawal_rate"),
    mean_value_at_death = map_dbl(success_rates, "expected_value_at_death"),
    mean_value_no_death = map_dbl(success_rates, "expected_value_no_death"),
    value_at_death_p05  = map_dbl(success_rates, ~ .x$value_at_death_ci[["5%"]]),
    value_at_death_p50  = map_dbl(success_rates, ~ .x$value_at_death_ci[["50%"]]),
    value_at_death_p95  = map_dbl(success_rates, ~ .x$value_at_death_ci[["95%"]]),
    value_no_death_p05  = map_dbl(success_rates, ~ .x$value_no_death_ci[["5%"]]),
    value_no_death_p50  = map_dbl(success_rates, ~ .x$value_no_death_ci[["50%"]]),
    value_no_death_p95  = map_dbl(success_rates, ~ .x$value_no_death_ci[["95%"]])
  )

ggplot(data = final_df2) +
  geom_line(aes(x = savings_rate, y = success_rate, color = factor(annuity_prop)))

ggplot(data = final_df2) +
  geom_line(aes(x = mean_withdrawal_rate, y = success_rate, color = factor(annuity_prop)))

ggplot(data = final_df2) +
  geom_line(aes(x = median_withdrawal_rate, y = success_rate, color = factor(annuity_prop)))


ggplot(data = final_df2) +
  geom_line(aes(x = savings_rate, y = mean_value_at_death, color = factor(annuity_prop))) +
  labs(title = "Mean End Portfolio Value")
# geom_line(aes(x = withdrawal_rate, y = value_no_death_p05, color = factor(annuity_prop)), linetype = "dashed") +
# geom_line(aes(x = withdrawal_rate, y = value_no_death_p95, color = factor(annuity_prop)), linetype = "dashed")

ggplot(data = final_df2) +
  geom_line(aes(x = savings_rate, y = value_at_death_p50, color = factor(annuity_prop))) +
  labs(title = "Median Portfolio Value at Death") +
  geom_line(aes(x = savings_rate, y = value_at_death_p05, color = factor(annuity_prop)), linetype = "dashed") +
  geom_line(aes(x = savings_rate, y = value_at_death_p95, color = factor(annuity_prop)), linetype = "dashed")

## Fixed withdrawal rates

with_progress({
  
  p <- progressor(along = 1:nrow(df2))
  
  df3 <- df2 %>%
    mutate(success_rates = pmap(list(annuity_prop, savings_rate),
                                ~{
                                  p()
                                  portfolio_sim_vec_wages_fixed_savings_rate(annuity_prop = ..1, 
                                                                             monthly_revenue = 1, 
                                                                             savings_rate = ..2,
                                                                             prices, 
                                                                             equity_sims, 
                                                                             cpi_sims,
                                                                             med_sims,
                                                                             eci_sims,
                                                                             death_ages,
                                                                             years_working = 40,
                                                                             wr = TRUE)
                                }))
})

final_df3 <- df3 %>%
  mutate(
    success_rate        = map_dbl(success_rates, "success"),
    success_no_death    = map_dbl(success_rates, "success_no_death"),
    mean_withdrawal_rate = map_dbl(success_rates, "mean_withdrawal_rate"),
    median_withdrawal_rate = map_dbl(success_rates, "median_withdrawal_rate"),
    mean_value_at_death = map_dbl(success_rates, "expected_value_at_death"),
    mean_value_no_death = map_dbl(success_rates, "expected_value_no_death"),
    value_at_death_p05  = map_dbl(success_rates, ~ .x$value_at_death_ci[["5%"]]),
    value_at_death_p50  = map_dbl(success_rates, ~ .x$value_at_death_ci[["50%"]]),
    value_at_death_p95  = map_dbl(success_rates, ~ .x$value_at_death_ci[["95%"]]),
    value_no_death_p05  = map_dbl(success_rates, ~ .x$value_no_death_ci[["5%"]]),
    value_no_death_p50  = map_dbl(success_rates, ~ .x$value_no_death_ci[["50%"]]),
    value_no_death_p95  = map_dbl(success_rates, ~ .x$value_no_death_ci[["95%"]])
  )

ggplot(data = final_df3 %>% filter(mean_withdrawal_rate < .2)) +
  geom_line(aes(x = mean_withdrawal_rate, y = success_rate, color = factor(annuity_prop)))


# Plots to compare withdrawal rates and success values

library(patchwork)
p1 <- ggplot(data = final_df2 %>%
               filter(mean_withdrawal_rate < .2)) +
  geom_line(aes(x = mean_withdrawal_rate, y = success_rate, color = factor(annuity_prop)))

p2 <- ggplot(data = final_df) +
  geom_line(aes(x = withdrawal_rate, y = success_rate, color = factor(annuity_prop)))


p1 + p2

ggplot() +
  geom_line(data = final_df,
            aes(x = withdrawal_rate, y = success_rate, color = factor(annuity_prop))) + 
  geom_line(data = final_df2 %>% filter(mean_withdrawal_rate < .2),
            aes(x = mean_withdrawal_rate, y = success_rate, color = factor(annuity_prop)),
            linetype = 2)
  



# Adding Wages ------------------------------------------------------------

# 1 dollar applied to a single cpi simulation

dollar_value <- cumprod(exp(cpi_sim))

equity_gross <- exp(equity_sim) 
plot(equity_gross, type = "l")
mean(equity_gross)

# wage value

wage_value <- rep(cumprod(exp(eci_sim)), each = 3)
plot(wage_value, type = "l", xlim = c(0,200), ylim = c(0,5))
lines(dollar_value)

# 20 year old without any savings, starting salary of 60,000
# expenses are 4000 per month
# extra is invested in stock market

monthly_expenses <- 2000
monthly_revenue <- 2400

# savings rate
(monthly_revenue - monthly_expenses) / monthly_revenue

portfolio_value <- rep(NA, length(dollar_value))

for (i in 1:length(portfolio_value)) {
  # starting month
  if (i ==1) {
    portfolio_value[i] <- (monthly_revenue*wage_value[i] - monthly_expenses*dollar_value[i])*equity_gross[i]
    portfolio_value[i] <- ifelse(portfolio_value[i] < 0, 0, portfolio_value[i])
  }
  # employment
  else if (i < 40*12) {
    portfolio_value[i] <- (portfolio_value[i-1] 
                           + monthly_revenue*wage_value[i]
                           - monthly_expenses*dollar_value[i])*equity_gross[i]
    portfolio_value[i] <- ifelse(portfolio_value[i] < 0, 0, portfolio_value[i])
  }
  # retirement
  else {
    portfolio_value[i] <- (portfolio_value[i-1]
                           - monthly_expenses*dollar_value[i])*equity_gross[i]
    portfolio_value[i] <- ifelse(portfolio_value[i] < 0, 0, portfolio_value[i])
  }
  
}

plot(portfolio_value, type = "l")


# multiple sims

equity_gross <- exp(equity_sims)
rowMeans(equity_gross)
matrix_cpi_sims <- do.call(rbind, cpi_sims)
price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
price_level[,40*12]

matrix_eci_sims <- do.call(rbind, eci_sims)
wage_value <- t(apply(matrix_eci_sims, 1, function(x) cumprod(exp(x))))
wage_value <- t(apply(wage_value, 1, function(x) {
  n_years <- length(x)
  monthly <- numeric(n_years * 12)
  
  for (i in 1:(n_years - 1)) {
    # endpoints for this year
    y0 <- x[i]
    y1 <- x[i + 1]
    
    # 12 monthly points (including first month = y0)
    monthly_segment <- seq(y0, y1, length.out = 12)
    
    # fill into output
    monthly[((i - 1) * 12 + 1):(i * 12)] <- monthly_segment
  }
  
  # last year gets flat because no next point
  monthly[(n_years - 1) * 12 + 1] <- x[n_years]
  monthly[(n_years - 1) * 12 + 2:(12)] <- x[n_years]
  
  monthly
}))


wage_value[,40*12]
vals <- wage_value[,40*12]/price_level[,40*12]
hist(vals)
mean(vals)

monthly_expenses <- 2000
monthly_revenue <- 2100
savings_rate <- (monthly_revenue-monthly_expenses)/monthly_revenue

starting_value <- monthly_revenue

portfolio_value <- rep(NA, length(dollar_value))

T <- ncol(equity_gross)
n_sims <- nrow(equity_gross)

portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)

# Period 1
portfolio_value[, 1] <- (monthly_revenue*wage_value[, 1] - monthly_expenses*price_level[, 1])*equity_gross[, 1]



# Employment
for (t in 2:(40*12)) {
  portfolio_value[, t] <- (portfolio_value[, t-1] 
                         + monthly_revenue*wage_value[, t]
                         - monthly_expenses*price_level[, t])*equity_gross[, t]
  portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
}

# retirement
for (t in ((40*12)+1):T) {
  portfolio_value[, t] <- (portfolio_value[, t-1] 
                           - monthly_expenses*price_level[, t])*equity_gross[, t]
  portfolio_value[, t] <- pmax(portfolio_value[, t], 0)
}

#portfolio_value <- pmax(portfolio_value, 0)


port_avg <- colMeans(portfolio_value)

port_ci <- apply(portfolio_value, 2, quantile, probs = c(.05, .5, .95))

plot(port_avg, type = "l", ylim = c(0, 10000000), main = "Portfolio Value", xlab = "Month")
lines(port_ci[3,], col = "red", lty = 2)
lines(port_ci[2,], col = "blue", lty = 2)
lines(port_ci[1,], col = "red", lty = 2)
abline(v = 481, lty = 4)


# make y axis dollars in today's dollars
vals <- portfolio_value/price_level

port_avg <- colMeans(vals)

port_ci <- apply(vals, 2, quantile, probs = c(.05, .5, .95))

plot(port_avg, type = "l", ylim = c(0, 10000000), main = "Portfolio Value", xlab = "Month")
lines(port_ci[3,], col = "red", lty = 2)
lines(port_ci[2,], col = "blue", lty = 2)
lines(port_ci[1,], col = "red", lty = 2)
abline(v = 481, lty = 4)
