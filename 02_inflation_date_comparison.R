# Libraries
library(tidyverse)
library(tidymodels)
library(forecast)
library(modeltime)
library(timetk)


# CPI ---------------------------------------------------------------------

cpi_raw <- read_csv("data/cpi.csv")

cpi_df <- read_csv("data/cpi.csv") |> 
  filter(date >= "2010-01-01") |> # only dates from 2010 onwards
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
  filter(!is.na(log_dif_cpi)) # remove first observation without lag


cpi_df2 <- read_csv("data/cpi.csv") |> 
  filter(date >= "1990-01-01") |> # only dates from 2010 onwards
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
  filter(!is.na(log_dif_cpi)) # remove first observation without lag

# Ensure the data is ordered and converted into time series
log_dif_cpi_ts <- ts(cpi_df$log_dif_cpi, start = c(min(year(cpi_df$date)), month(min(cpi_df$date))), frequency = 12)
log_dif_cpi_ts2 <- ts(cpi_df2$log_dif_cpi, start = c(min(year(cpi_df$date)), month(min(cpi_df$date))), frequency = 12)


# Fit ARIMA(1,0,1) model
cpi_arima_model <- Arima(log_dif_cpi_ts, order = c(1, 0, 1))
cpi_arima_model2 <- Arima(log_dif_cpi_ts2, order = c(1, 0, 1))
summary(cpi_arima_model)
summary(cpi_arima_model2)

# Get draws of the model 

n_sims <- 1000
sim_length <- 12*30
ar101_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
ar101_sims2 <- matrix(NA, nrow = n_sims, ncol = sim_length)

for(i in 1:n_sims){
  ar101_sims[i,] <- simulate(cpi_arima_model, nsim = sim_length)
  ar101_sims2[i,] <- simulate(cpi_arima_model2, nsim = sim_length)
}

# get average simulation
ar101_avg <- colMeans(ar101_sims)
# get 95% intervals
ar101_ci <- apply(ar101_sims, 2, quantile, probs = c(.025, .975))


# get average simulation
ar101_avg2 <- colMeans(ar101_sims2)
# get 95% intervals
ar101_ci2 <- apply(ar101_sims2, 2, quantile, probs = c(.025, .975))

# plot simulation
plot(ar101_avg, type = "l", ylim = c(-.009, .015), col = "red")
lines(ar101_ci[2,], col = "red", lty = 2)
lines(ar101_ci[1,], col = "red", lty = 2)
lines(ar101_avg2, col = "blue")
lines(ar101_ci2[2,], col = "blue", lty = 2)
lines(ar101_ci2[1,], col = "blue", lty = 2)
legend("topright", legend = c("2010", "1990"), col = c("red", "blue"), lty = c(1, 1))


# ECI ---------------------------------------------------------------------

quarterly_cpi <- cpi_df |> 
  filter(month(date) %in% c(3, 6, 9, 12)) |>
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi),
         lagged_cpi = lag(log_dif_cpi, n = 1)) |> 
  filter(!is.na(log_dif_cpi) & !is.na(lagged_cpi)) # remove first observation without lag

eci_df <- read_csv("data/eci.csv") |> 
  filter(date >= min(quarterly_cpi$date)-months(3)) |> # only need data that cpi has
  mutate(lagged_eci = lag(eci, n = 1)) |> 
  mutate(log_dif_eci = log(eci) - log(lagged_eci)) |> 
  filter(!is.na(log_dif_eci)) # remove first observation without lag

quarterly_cpi2 <- cpi_df2 |> 
  filter(month(date) %in% c(3, 6, 9, 12)) |>
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi),
         lagged_cpi = lag(log_dif_cpi, n = 1)) |> 
  filter(!is.na(log_dif_cpi) & !is.na(lagged_cpi)) # remove first observation without lag

eci_df2 <- read_csv("data/eci.csv") |> 
  filter(date >= min(quarterly_cpi2$date)-months(3)) |> # only need data that cpi has
  mutate(lagged_eci = lag(eci, n = 1)) |> 
  mutate(log_dif_eci = log(eci) - log(lagged_eci)) |> 
  filter(!is.na(log_dif_eci)) # remove first observation without lag

# Correlation between ECI and CPI
cor(eci_df$log_dif_eci, quarterly_cpi$log_dif_cpi)
# find correlation between ECI and CPI lagged by 1
cor(eci_df$log_dif_eci, quarterly_cpi$lagged_cpi, use = "complete.obs")

# Fit Model


log_dif_eci_ts <- ts(eci_df$log_dif_eci, start = c(min(year(eci_df$date)), quarter(min(eci_df$date))), frequency = 4)
lagged_cpi_ts <- ts(quarterly_cpi$lagged_cpi, start = c(min(year(eci_df$date)), quarter(min(eci_df$date))), frequency = 4)
log_dif_eci_ts2 <- ts(eci_df2$log_dif_eci, start = c(min(year(eci_df2$date)), quarter(min(eci_df2$date))), frequency = 4)
lagged_cpi_ts2 <- ts(quarterly_cpi2$lagged_cpi, start = c(min(year(eci_df2$date)), quarter(min(eci_df2$date))), frequency = 4)

arima_model_wlagcpi <- Arima(log_dif_eci_ts, order = c(1, 1, 1), xreg = lagged_cpi_ts)
arima_model_wlagcpi2 <- Arima(log_dif_eci_ts2, order = c(1, 1, 1), xreg = lagged_cpi_ts2)
summary(arima_model_wlagcpi)
summary(arima_model_wlagcpi2)

# Get draws of the model 

n_sims <- 1000
sim_length <- 4*30
full_model_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
full_model_sims2 <- matrix(NA, nrow = n_sims, ncol = sim_length)

for(i in 1:n_sims){
  log_dif_cpi_sim <- simulate(cpi_arima_model, nsim = sim_length*3)
  log_dif_cpi_sim2 <- simulate(cpi_arima_model2, nsim = sim_length*3)
  
  # make simulations quarterly
  quarterly_cpi_sim_df <- data.frame(
    date = zoo::as.Date(time(log_dif_cpi_sim)),
    value = log_dif_cpi_sim) |> 
    mutate(year = year(date),
           quarter = quarter(date)) |> 
    group_by(year, quarter) |> 
    summarize(date = max(date),
              log_dif_cpi = sum(value))
  
  
  # make simulations quarterly
  quarterly_cpi_sim_df2 <- data.frame(
    date = zoo::as.Date(time(log_dif_cpi_sim2)),
    value = log_dif_cpi_sim2) |> 
    mutate(year = year(date),
           quarter = quarter(date)) |> 
    group_by(year, quarter) |> 
    summarize(date = max(date),
              log_dif_cpi = sum(value))
  
  lagged_cpi_sim <- c(tail(quarterly_cpi$log_dif_cpi, n = 1), head(quarterly_cpi_sim_df$log_dif_cpi, n = -1))
  full_model_sims[i,] <- simulate(arima_model_wlagcpi, nsim = sim_length, xreg = lagged_cpi_sim)
  lagged_cpi_sim2 <- c(tail(quarterly_cpi2$log_dif_cpi, n = 1), head(quarterly_cpi_sim_df2$log_dif_cpi, n = -1))
  #lagged_cpi_sim2 <- quarterly_cpi_sim_df2$log_dif_cpi
  full_model_sims2[i,] <- simulate(arima_model_wlagcpi2, nsim = sim_length, xreg = lagged_cpi_sim2)
}

full_model_avg <- colMeans(full_model_sims)
full_model_ci <- apply(full_model_sims, 2, quantile, probs = c(.025, .975))
full_model_avg2 <- colMeans(full_model_sims2)
full_model_ci2 <- apply(full_model_sims2, 2, quantile, probs = c(.025, .975))

# plot simulation
plot(full_model_avg, type = "l", col = "red", ylim = c(-.008, .02))
lines(full_model_ci[2,], col = "red", lty = 2)
lines(full_model_ci[1,], col = "red", lty = 2)
lines(full_model_avg2, col = "blue")
lines(full_model_ci2[2,], col = "blue", lty = 2)
lines(full_model_ci2[1,], col = "blue", lty = 2)
legend("topright", legend = c("2010", "1990"), col = c("red", "blue"), lty = c(1, 1))

## Explore relationship between draws of CPI and ECI Model

# Get draws of the model

cpi_sim <- simulate(cpi_arima_model, nsim = 20*12)

quarterly_cpi_sim_df <- data.frame(
  date = zoo::as.Date(time(cpi_sim)),
  value = cpi_sim) |> 
  mutate(year = year(date),
         quarter = quarter(date)) |> 
  group_by(year, quarter) |> 
  summarize(date = max(date),
            log_dif_cpi = sum(value))

lagged_cpi_sim <- c(tail(quarterly_cpi$log_dif_cpi, n = 1), head(quarterly_cpi_sim_df$log_dif_cpi, n = -1))
eci_sim <- simulate(arima_model_wlagcpi, nsim = 20*4, xreg = lagged_cpi_sim)

cor(quarterly_cpi_sim_df$log_dif_cpi, eci_sim)
cor(lagged_cpi_sim, eci_sim, use = "complete.obs")
plot(quarterly_cpi_sim_df$log_dif_cpi, eci_sim)
plot(lagged_cpi_sim, eci_sim)


# Medical Inflation -------------------------------------------------------

med_df <- read_csv("data/med_inflation.csv") |> 
  filter(date >= "2010-01-01") |> 
  mutate(lagged_med = lag(med_inflation, n = 1)) |> 
  mutate(log_dif_med = log(med_inflation) - log(lagged_med)) |> 
  filter(!is.na(log_dif_med)) # remove first observation without lag

# Ensure the data is ordered and converted into time series
log_dif_med_ts <- ts(med_df$log_dif_med, start = c(min(year(med_df$date))), frequency = 12)

# Fit ARIMA(1,0,1) model
med_arima_model <- Arima(log_dif_med_ts, order = c(1, 0, 1))

# Get draws of the model

n_sims <- 1000
sim_length <- 12*30
ar101_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)

for(i in 1:n_sims){
  ar101_sims[i,] <- simulate(med_arima_model, nsim = sim_length)
}

# get average simulation
ar101_avg <- colMeans(ar101_sims)
# get 95% intervals
ar101_ci <- apply(ar101_sims, 2, quantile, probs = c(.025, .975))

# plot simulation
plot(ar101_avg, type = "l", ylim = c(-.009, .015), col = "red")
lines(ar101_ci[2,], col = "red", lty = 2)
lines(ar101_ci[1,], col = "red", lty = 2)

# Explore residuals from each model

# Get residuals
med_residuals <- med_arima_model$residuals
cpi_residuals <- cpi_arima_model$residuals

cor(med_residuals, cpi_residuals)
plot(med_residuals, cpi_residuals)



