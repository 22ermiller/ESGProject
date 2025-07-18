library(tidyverse)
library(tidyquant)
library(forecast)
library(modeltime)
library(patchwork)


# Get S&P 500 price data (adjusted closing prices)
sp500_raw <- tq_get("^SP500TR", from = "1990-01-01", to = Sys.Date())

sp500 <- sp500_raw |> 
  # only get first day of each month
  mutate(year = year(date),
         month = month(date),
         day = day(date)) |>
  group_by(year, month) |>
  arrange(day) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(date = floor_date(date, unit = "month")) |>
  mutate(lagged_sp500 = lag(close, n = 1)) |> 
  mutate(dif_sp500 = close - lagged_sp500,
         log_dif_sp500 = log(close) - log(lagged_sp500)) |> 
  filter(!is.na(log_dif_sp500))

ggplot(data = sp500) +
  geom_line(aes(x = date, y = close))

ggplot(data = sp500) +
  geom_line(aes(x = date, y = log_dif_sp500))


# Explore Relationships ---------------------------------------------------

# Read in CPI data
cpi_df <- read_csv("data/cpi.csv") |> 
  filter(date >= "1990-01-01") |> 
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
  filter(!is.na(log_dif_cpi))

# Read in interest rate data
interest_df <- read_csv("data/interest_rate.csv") |> 
  rename(date = observation_date,
         rate = DGS3MO) |> 
  mutate(rate = rate/100) %>% 
  filter(!is.na(rate)) |> 
  mutate(year = year(date),
         month = month(date),
         day = day(date)) |> 
  group_by(year, month) |> 
  arrange(day) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  select(-c(year,month,day)) %>% 
  mutate(date = floor_date(date, unit = "month")) |>
  filter(date >= min(cpi_df$date) - months(1) & date <= max(cpi_df$date)) |> 
  mutate(lagged_rate = lag(rate, n = 1)) |> 
  mutate(dif_rate = rate - lagged_rate,
         log_dif_rate = log(rate) - log(lagged_rate)) |> 
  filter(!is.na(log_dif_rate))

full_df <- sp500 |> 
  left_join(cpi_df, by = "date") %>% 
  left_join(interest_df, by = "date") %>% 
  filter(!is.na(log_dif_cpi) & !is.na(rate)) %>% 
  mutate(sp500_rminterest = log_dif_sp500 - rate/12,
         sp500_rminterest_rmmean = sp500_rminterest - mean(sp500_rminterest, na.rm = TRUE))

# CPI vs S&P 500

cor(full_df$log_dif_cpi, full_df$log_dif_sp500, use = "pairwise.complete.obs")

ggplot(data = full_df) +
  geom_point(aes(x = log_dif_sp500, y = log_dif_cpi))

p1 <- ggplot(data = full_df) + 
  geom_line(aes(x = date, y = log_dif_sp500)) + 
  labs(title = "Market Returns")

p2 <- ggplot(data = cpi_df) + 
  geom_line(aes(x = date, y = log_dif_cpi)) + 
  labs(title = "Inflation")

p1 + p2

# Interest Rate vs S&P 500

cor(full_df$rate, full_df$log_dif_sp500, use = "pairwise.complete.obs")

ggplot(data = full_df) +
  geom_point(aes(x = log_dif_sp500, y = rate))

p3 <- ggplot(data = full_df) + 
  geom_line(aes(x = date, y = log_dif_sp500)) + 
  labs(title = "Market Returns")

p4 <- ggplot(data = full_df) + 
  geom_line(aes(x = date, y = rate)) + 
  labs(title = "Inflation")

p3 + p4

# ARIMA Model -------------------------------------------------------------

returns_ts <- ts(full_df$log_dif_sp500, frequency = 12, start = c(1990, 1))

ar101 <- Arima(returns_ts, order = c(1, 0, 1))
summary(ar101)

# fit ARIMA model using auto.arima

auto_ar <- auto.arima(returns_ts, method = "ML")
summary(auto_ar)

# Analyze residuals

ar_residuals <- resid(ar101)
auto.ar_residuals <- resid(auto_ar)

par(mfrow = c(1, 1))
plot(ar_residuals)
plot(auto.ar_residuals)

ggAcf(ar_residuals, lag.max = 200)
ggAcf(auto.ar_residuals, lag.max = 200)


# ARIMA Model w/ predictors -----------------------------------------------

# Create matrix of predictors

X <- as.matrix(cbind(
  full_df$log_dif_cpi,
  full_df$rate
))

full_ar101 <- Arima(returns_ts, order = c(1, 0, 1), xreg= X)
summary(full_ar101)

cpi_only_ar101 <- Arima(returns_ts, order = c(1, 0, 1), xreg= X[,1])
summary(cpi_only_ar101)


# GARCH Model -------------------------------------------------------------

library(rugarch)

# Model without predictors
simple_garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                                distribution.model = "norm")

simple_garch_fit <- ugarchfit(spec = simple_garch_spec, data = returns_ts)

# Model with cpi & interest rate
full_garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)
                                           , external.regressors = X
                         ),
                         distribution.model = "norm")

full_garch_fit <- ugarchfit(spec = full_garch_spec, data = returns_ts)

# Model with cpi only
X_cpi <- X[,1, drop = FALSE]
cpi_garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(0, 0)
                                                , external.regressors = X_cpi, include.mean = FALSE
                              ),
                              distribution.model = "norm")

cpi_garch_fit <- ugarchfit(spec = cpi_garch_spec, data = returns_ts)

infocriteria(simple_garch_fit)
infocriteria(full_garch_fit)
infocriteria(cpi_garch_fit)


# Mean Model --------------------------------------------------------------

# Mean Models for all 3 sp500s
mean_mod <- lm(log_dif_sp500 ~ log_dif_cpi + rate, data = full_df)
mean_mod2 <- lm(sp500_rminterest ~ log_dif_cpi + rate, data = full_df)
mean_mod3 <- lm(sp500_rminterest_rmmean ~ log_dif_cpi + rate, data = full_df)

summary(mean_mod)
summary(mean_mod2)
summary(mean_mod3)

mean_mod_resids <- resid(mean_mod)
mean_mod2_resids <- resid(mean_mod2)
mean_mod3_resids <- resid(mean_mod3)

# Mean models with CPI instead of log_dif_cpi
cpi_mean_mod <- lm(log_dif_sp500 ~ cpi + rate, data = full_df)
cpi_mean_mod2 <- lm(sp500_rminterest ~ cpi + rate, data = full_df)
cpi_mean_mod3 <- lm(sp500_rminterest_rmmean ~ cpi + rate, data = full_df)

summary(cpi_mean_mod)
summary(cpi_mean_mod2)
summary(cpi_mean_mod3)

cpi_mean_mod_resids <- resid(cpi_mean_mod)
cpi_mean_mod2_resids <- resid(cpi_mean_mod2)
cpi_mean_mod3_resids <- resid(cpi_mean_mod3)




# Regime-Switching GARCH --------------------------------------------------

library(MSGARCH)

spec <- CreateSpec(variance.spec = list(model = "sGARCH"),
                         distribution.spec = list("norm"),
                         switch.spec = list(K = 2))

rs_garch_fit <- FitML(spec, data = full_df$log_dif_sp500)
summary(rs_garch_fit)

predict(rs_garch_fit, nahead= 10)
# Plot residuals
plot(resids)

# Fit all the models

mean_mod1_rs <- FitML(spec, data = mean_mod_resids)
mean_mod2_rs <- FitML(spec, data = mean_mod2_resids)
mean_mod3_rs <- FitML(spec, data = mean_mod3_resids)

cpi_mean_mod1_rs <- FitML(spec, data = cpi_mean_mod_resids)
cpi_mean_mod2_rs <- FitML(spec, data = cpi_mean_mod2_resids)
cpi_mean_mod3_rs <- FitML(spec, data = cpi_mean_mod3_resids)

summary(mean_mod1_rs)
summary(mean_mod2_rs)
summary(mean_mod3_rs)
summary(cpi_mean_mod1_rs)
summary(cpi_mean_mod2_rs)
summary(cpi_mean_mod3_rs)


# Simulations -------------------------------------------------------------

# Read in previous models

cpi_mod <- readRDS("models/cpi_mod.rds")
interest_mod <- readRDS("models/interest_garch.rds")

# CPI Simulation Function

cpi_simulate <- function(model, n = 600, start_date = max(full_df$date) + months(1), final_val = tail(full_df$cpi, 1)) {
  sim <- simulate(model, nsim = n)
  sim_df <- data.frame(date = seq(from = start_date, by = "month", length.out = n),
                       cpi_sim = sim)
  final_df <- sim_df %>% 
    mutate(real_cpi = final_val * exp(cumsum(cpi_sim))) # transform to real cpi values
  return(final_df)
}

# Interest Rate Simulation Function

interest_simulate <- function(model, cpi_sim, n = 600, start_date = max(full_df$date) + months(1)) {
  r_bar <- .005
  ir_mean <- 0.02574889
  mexsim_list <- list(matrix(cpi_sim))
  sim <- ugarchsim(model, n.sim = n, startMethod = "sample", mexsimdata = mexsim_list)@simulation$seriesSim +
    ir_mean # add back the mean of the interest rate
  sim_df <- data.frame(date = seq(from = start_date, by = "month", length.out = n),
                       rate_sim = sim)
  final_df <- sim_df %>% mutate(
    real_rate = ifelse(rate_sim > r_bar,
                       rate_sim,
                       exp((rate_sim - r_bar + r_bar * log(r_bar)) / r_bar)
    )
  )
  return(final_df)
}

# RS GARCH S&P500 Simulation

sp500_rs_simulate <- function(mean_model, rs_model, cpi_sim, interest_sim, n = 600,
                           start_date = max(full_df$date) + months(1), final_value = tail(full_df$close, 1),
                           og_mean = mean(full_df$sp500_rminterest) ,response_type = "log_sp500", cpi_type = "log_dif_cpi") {
  newdata <- data.frame(cpi_sim) |> setNames(cpi_type)
  newdata$rate <- interest_sim
  mean_vals <- predict(mean_model, newdata = newdata)
  sim <- simulate(rs_model, nsim = n)
  sim_df <- data.frame(date = seq(from = start_date, by = "month", length.out = n),
                       resid_sim = sim$draw[1,],
                       state = sim$state[1,])
  final_df <- sim_df %>% 
    mutate(temp = mean_vals + resid_sim,
           realsp500 = case_when(response_type == "log_sp500" ~ final_value * exp(cumsum(temp)),
                                 response_type == "sp500_rminterest" ~ final_value * exp(cumsum(temp + log(1 + interest_sim))),
                                 response_type == "sp500_rminterest_rmmean" ~
                                   final_value * exp(cumsum(temp + log(1 + interest_sim) + og_mean)))
           ) # transform to real sp500 values
  return(final_df)
}

sp500_garch_sim <- function(model, cpi_sim, interest_sim, n = 600,
                           start_date = max(full_df$date) + months(1), final_value = tail(full_df$close, 1)) {
  X <- as.matrix(cbind(cpi_sim, interest_sim))
  sim <- ugarchsim(model, n.sim = n, mexsimdata = list(X))@simulation$seriesSim
  sim_df <- data.frame(date = seq(from = start_date, by = "month", length.out = n),
                       sp500_sim = sim)
  final_df <- sim_df %>% 
    mutate(realsp500 = final_value * exp(cumsum(sp500_sim))) # transform to real sp500 values
  return(final_df)
}

# Run the simulations
cpi_sim <- cpi_simulate(cpi_mod, n = 600)
interest_sim <- interest_simulate(interest_mod, cpi_sim$cpi_sim, n = 600)
plot(interest_sim$real_rate, type = "l")
plot(cpi_sim$real_cpi, type = "l")
plot(cpi_sim$cpi_sim, type = "l")

# 3 different S&P 500 simulations w/ log_cpi
sp500_sim <- sp500_rs_simulate(mean_mod, mean_mod1_rs, cpi_sim$cpi_sim, interest_sim$real_rate, n = 600)
sp500_rminterest_sim <- sp500_rs_simulate(mean_mod2, mean_mod2_rs, cpi_sim$cpi_sim, interest_sim$real_rate, n = 600,
                                                        response_type = "sp500_rminterest")
sp500_rminterest_rmmean_sim <- sp500_rs_simulate(mean_mod3, mean_mod3_rs, cpi_sim$cpi_sim, interest_sim$real_rate, n = 600,
                                                        response_type = "sp500_rminterest_rmmean")

# 3 different s&P 500 simulations w/ cpi
cpi_sp500_sim <- sp500_rs_simulate(cpi_mean_mod, cpi_mean_mod1_rs, cpi_sim$real_cpi, interest_sim$real_rate, n = 600, cpi_type = "cpi")
cpi_sp500_rminterest_sim <- sp500_rs_simulate(cpi_mean_mod2, cpi_mean_mod2_rs, cpi_sim$real_cpi, interest_sim$real_rate, n = 600,
                                          response_type = "sp500_rminterest",cpi_type = "cpi")
cpi_sp500_rminterest_rmmean_sim <- sp500_rs_simulate(cpi_mean_mod3, cpi_mean_mod3_rs, cpi_sim$real_cpi, interest_sim$real_rate, n = 600,
                                                 response_type = "sp500_rminterest_rmmean",cpi_type = "cpi")

garch_sim <- sp500_garch_sim(full_garch_fit, cpi_sim$cpi_sim, interest_sim$real_rate, n = 600)

# Plot the simulations
plot(cpi_sim$cpi_sim)
plot(interest_sim$real_rate, type = "l")

# log_dif_cpi as predictor
plot(sp500_sim$temp, type = "l")
plot(sp500_rminterest_sim$temp, type = "l")
plot(sp500_rminterest_rmmean_sim$temp, type = "l")

# cpi as predictor
plot(cpi_sp500_sim$temp, type = "l")
plot(cpi_sp500_rminterest_sim$temp, type = "l")
plot(cpi_sp500_rminterest_rmmean_sim$temp, type = "l")

plot(garch_sim$sp500_sim, type = "l")


# Multiple Simulations ----------------------------------------------------

n_sims <- 1000
sim_length <- 600

sp500_sim <- matrix(NA, nrow = n_sims, ncol = sim_length)
sp500_rminterest_sim <- matrix(NA, nrow = n_sims, ncol = sim_length)
sp500_rminterest_rmmean_sim <- matrix(NA, nrow = n_sims, ncol = sim_length)
cpi_sp500_sim <- matrix(NA, nrow = n_sims, ncol = sim_length)
cpi_sp500_rminterest_sim <- matrix(NA, nrow = n_sims, ncol = sim_length)
cpi_sp500_rminterest_rmmean_sim <- matrix(NA, nrow = n_sims, ncol = sim_length)
garch_sim <- matrix(NA, nrow = n_sims, ncol = sim_length)

for(i in 1:n_sims) {
  
  cpi_sim <- cpi_simulate(cpi_mod, n = sim_length)
  interest_sim <- interest_simulate(interest_mod, cpi_sim$cpi_sim, n = sim_length)

  sp500_sim[i,] <- sp500_rs_simulate(mean_mod, mean_mod1_rs, cpi_sim$cpi_sim, interest_sim$real_rate, n = sim_length)$temp
  sp500_rminterest_sim[i,] <- sp500_rs_simulate(mean_mod2, mean_mod2_rs, cpi_sim$cpi_sim, interest_sim$real_rate, n = 600,
                                                response_type = "sp500_rminterest")$temp
  sp500_rminterest_rmmean_sim[i,] <- sp500_rs_simulate(mean_mod3, mean_mod3_rs, cpi_sim$cpi_sim, interest_sim$real_rate, n = 600,
                                                       response_type = "sp500_rminterest_rmmean")$temp
  
  # cpi_sp500_sim[i,] <- sp500_rs_simulate(cpi_mean_mod, cpi_mean_mod1_rs, cpi_sim$real_cpi, interest_sim$real_rate, n = 600, cpi_type = "cpi")$temp
  # cpi_sp500_rminterest_sim[i,] <- sp500_rs_simulate(cpi_mean_mod2, cpi_mean_mod2_rs, cpi_sim$real_cpi, interest_sim$real_rate, n = 600,
  #                                                   response_type = "sp500_rminterest",cpi_type = "cpi")$temp
  # cpi_sp500_rminterest_rmmean_sim[i,] <- sp500_rs_simulate(cpi_mean_mod3, cpi_mean_mod3_rs, cpi_sim$real_cpi, interest_sim$real_rate, n = 600,
  #                                                          response_type = "sp500_rminterest_rmmean",cpi_type = "cpi")$temp
  # 
  # 
  garch_sim[i,] <- sp500_garch_sim(full_garch_fit, cpi_sim$cpi_sim, interest_sim$real_rate, n = 600)$sp500_sim
}

sp500_avg <- colMeans(sp500_sim)
sp500_median <- apply(sp500_sim, 2, median)
sp500_rminterest_avg <- colMeans(sp500_rminterest_sim)
sp500_rminterest_rmmean_avg <- colMeans(sp500_rminterest_rmmean_sim)
cpi_sp500_avg <- colMeans(cpi_sp500_sim)
cpi_sp500_rminterest_avg <- colMeans(cpi_sp500_rminterest_sim)
cpi_sp500_rminterest_rmmean_avg <- colMeans(cpi_sp500_rminterest_rmmean_sim)
garch_avg <- colMeans(garch_sim)
garch_median <- apply(garch_sim, 2, median)
# 
sp500_ci <- apply(sp500_sim, 2, quantile, probs = c(.025, .975))
sp500_rminterest_ci <- apply(sp500_rminterest_sim, 2, quantile, probs = c(.025, .975))
sp500_rminterest_rmmean_ci <- apply(sp500_rminterest_rmmean_sim, 2, quantile, probs = c(.025, .975))
cpi_sp500_ci <- apply(cpi_sp500_sim, 2, quantile, probs = c(.025, .975))
cpi_sp500_rminterest_ci <- apply(cpi_sp500_rminterest_sim, 2, quantile, probs = c(.025, .975))
cpi_sp500_rminterest_rmmean_ci <- apply(cpi_sp500_rminterest_rmmean_sim, 2, quantile, probs = c(.025, .975))
garch_ci <- apply(garch_sim, 2, quantile, probs = c(.025, .975))

plot_sims <- function(avg, ci, title = "Simulation") {
  plot(avg, type = "l",
       ylim = c(-.2,.2), 
       main = title)
  lines(ci[2,], col = "red", lty = 2)
  lines(ci[1,], col = "red", lty = 2)
}

plot_sims(sp500_avg, sp500_ci)
plot_sims(sp500_rminterest_avg, sp500_rminterest_ci)
plot_sims(sp500_rminterest_rmmean_avg, sp500_rminterest_rmmean_ci)
plot_sims(cpi_sp500_avg, cpi_sp500_ci)
plot_sims(cpi_sp500_rminterest_avg, cpi_sp500_rminterest_ci)
plot_sims(cpi_sp500_rminterest_rmmean_avg, cpi_sp500_rminterest_rmmean_ci)
plot_sims(garch_avg, garch_ci, title = "GARCH Simulation")

plot(sp500_avg, type = "l", ylim = c(-.01,.012))
lines(sp500_rminterest_avg, type = "l")
lines(sp500_rminterest_rmmean_avg, type = "l")


# plot sp500_sim and garch_sim on same plot
plot(sp500_median, col = "red", type = "l",main = "S&P 500 Simulations")
lines(garch_median, col = "blue")
lines(sp500_ci[2,], col = "red", lty = 2)
lines(sp500_ci[1,], col = "red", lty = 2)
lines(garch_ci[2,], col = "blue", lty = 2)
lines(garch_ci[1,], col = "blue", lty = 2)




