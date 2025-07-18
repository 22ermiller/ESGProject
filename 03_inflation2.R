# Libraries
library(tidyverse)
library(tidymodels)
library(forecast)
library(modeltime)
library(timetk)
# Read in Data


# CPI ---------------------------------------------------------------------
cpi_raw <- read_csv("data/cpi.csv")

cpi_df <- read_csv("data/cpi.csv") |> 
  filter(date >= "2010-01-01") |> 
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
  filter(!is.na(log_dif_cpi)) # remove first observation without lag

# Ensure the data is ordered and converted into time series
log_dif_cpi_ts <- ts(cpi_df$log_dif_cpi, start = c(min(year(cpi_df$date)), month(min(cpi_df$date))), frequency = 12)

# Fit ARIMA(1,0,1) model
cpi_arima_model <- Arima(log_dif_cpi_ts, order = c(1, 0, 1))

# add diferencing
dif_arima_model <- Arima(log_dif_cpi_ts, order = c(0, 1, 2))

# optimal model from auto_arima
seasonal_arima <- Arima(log_dif_cpi_ts, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 2), period = 12))

# Summary of the models
summary(cpi_arima_model)
#summary(dif_arima_model)
summary(seasonal_arima)

# AIC for each model
AIC(cpi_arima_model)
AIC(dif_arima_model)
AIC(seasonal_arima)

BIC(cpi_arima_model)
BIC(seasonal_arima)
BIC(dif_arima_model)
# Get draws of the model 

n_sims <- 1000
sim_length <- 12*30
ar101_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
seasonal_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)

for(i in 1:n_sims){
  ar101_sims[i,] <- simulate(cpi_arima_model, nsim = sim_length)
  seasonal_sims[i,] <- simulate(seasonal_arima, nsim = sim_length)
}

# get average simulation
ar101_avg <- colMeans(ar101_sims)
seasonal_avg <- colMeans(seasonal_sims)
# get 95% intervals
ar101_ci <- apply(ar101_sims, 2, quantile, probs = c(.025, .975))
seasonal_ci <- apply(seasonal_sims, 2, quantile, probs = c(.025, .975))

# plot simulation
plot(ar101_avg, type = "l", ylim = c(-.009, .015), col = "red")
lines(ar101_ci[2,], col = "red", lty = 2)
lines(ar101_ci[1,], col = "red", lty = 2)
lines(seasonal_avg, col = "blue")
lines(seasonal_ci[2,], col = "blue", lty = 2)
lines(seasonal_ci[1,], col = "blue", lty = 2)
legend("bottomleft", legend = c("ARIMA(1,0,1)", "Seasonal ARIMA(1,1,1)(1,0,2)"), col = c("red", "blue"), lty = 1)

arima_sim <- simulate(cpi_arima_model, nsim = 20*12)
seasonal_arima_sim <- simulate(seasonal_arima, nsim = 20*12)

#plot the simulation
autoplot(arima_sim) + ggtitle("ARIMA(1,0,1) Simulation")  + autolayer(log_dif_cpi_ts, series = "Actual") 

df <- data.frame(
  year = 1:length(ar101_avg),  # Assuming ar101_avg has a time component
  avg = ar101_avg,
  upper = ar101_ci[2,],
  lower = ar101_ci[1,]
)

# Plot
p2 <- ggplot(df, aes(x = year)) +
  geom_line(aes(y = avg, color = "Average")) +  # Main line with legend label
  geom_line(aes(y = upper, color = "Confidence Band"), linetype = "dashed") +  # Upper CI line, no legend for linetype
  geom_line(aes(y = lower, color = "Confidence Band"), linetype = "dashed") +  # Lower CI line, no legend for linetype
  scale_y_continuous(limits = c(-0.009, 0.015)) +  # Set y-axis limits
  labs(x = "Year", y = "Value", title = "Simulation with 95% Confidence Intervals") +
  scale_color_manual(values = c("Average" = "black", "Confidence Band" = "red")) +  # Customize colors
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top")  # Move the legend to the top

p2



p1 <- ggplot() + 
  geom_line(aes(x = time(arima_sim), y = arima_sim, color = "Simulated")) + 
  geom_line(aes(x = time(log_dif_cpi_ts), y = log_dif_cpi_ts, color = "Actual")) + 
  ggtitle("Single Simulation") + 
  xlab("Year") + 
  ylab("Log Difference in CPI") + 
  scale_color_manual(values = c("Simulated" = "red", "Actual" = "black")) + 
  theme_minimal() +
  labs(color = "") +  # Label the legend
  theme(legend.position = "top")  # Move the legend to the top

p3 <- p1 + p2

ggsave("cpi_sim.png", p3)


autoplot(arima_sim, series = "Future") +
  ggtitle("ARIMA(1,0,1) Simulation") + 
  autolayer(log_dif_cpi_ts, series = "Actual") +
  ylab("Log Difference in CPI") +  # Change the y-axis label
  scale_color_manual(values = c("Simulated" = "blue", "Actual" = "black"), 
                     labels = c("Simulated", "Actual")) +  # Customize the legend
  theme_minimal()



autoplot(seasonal_arima_sim) + ggtitle("Seasonal ARIMA(1,1,1)(1,0,2) Simulation") + autolayer(log_dif_cpi_ts, series = "Actual")

seasonal_arima_draws <- seasonal_arima |> forecast(h = 12)

# Why is the differencing component better?

plot(log_dif_cpi_ts)
differenced_log_dif_cpi <- cpi_df |> 
  mutate(lagged_log_dif_cpi = lag(log_dif_cpi, n = 1)) |>
  filter(!is.na(lagged_log_dif_cpi)) |>
  mutate(differenced_log_dif_cpi = log_dif_cpi - lagged_log_dif_cpi) |>
  pull(differenced_log_dif_cpi)
differenced_log_dif_cpi_ts <- ts(differenced_log_dif_cpi, start = c(min(year(cpi_df$date))), frequency = 12)

plot(log_dif_cpi_ts)
lines(differenced_log_dif_cpi_ts, col = alpha("red", .5))

## What is this modelling?
## (log(cpi_t)-log(cpi_t-1)) - (log(cpi_t-1)-log(cpi_t-2)) = log_cpi_t - 2*log_cpi_t-1 + log_cpi_t-2

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

## Explore relationship between ECI and CPI

# Correlation between ECI and CPI
cor(eci_df$log_dif_eci, quarterly_cpi$log_dif_cpi)
# find correlation between ECI and CPI lagged by 1
cor(eci_df$log_dif_eci, quarterly_cpi$lagged_cpi, use = "complete.obs")

plot(eci_df$log_dif_eci, type = "l")
plot(quarterly_cpi$log_dif_cpi, type = "l")
plot(eci_df$log_dif_eci, quarterly_cpi$log_dif_cpi)
plot(eci_df$log_dif_eci, quarterly_cpi$lagged_cpi)



# Ensure the data is ordered and converted into time series
log_dif_eci_ts <- ts(eci_df$log_dif_eci, start = c(min(year(eci_df$date)), quarter(min(eci_df$date))), frequency = 4)
log_dif_cpi_ts <- ts(quarterly_cpi$log_dif_cpi, start = c(min(year(eci_df$date))), frequency = 4)
lagged_cpi_ts <- ts(quarterly_cpi$lagged_cpi, start = c(min(year(eci_df$date))), frequency = 4)
xreg_matrix <- cbind(log_dif_cpi_ts, lagged_cpi_ts)
xreg_matrix <- cbind(quarterly_cpi$log_dif_cpi,quarterly_cpi$lagged_cpi)

test.lm <- lm(eci_df$log_dif_eci~xreg_matrix)
summary(test.lm)

resid_ts <- ts(test.lm$residuals, start = c(min(year(eci_df$date))), frequency = 4)

resid_arima <- Arima(resid_ts, order = c(1, 1, 1))
summary(resid_arima)

# Fit ARIMA(1,0,1) model
arima_model <- Arima(log_dif_eci_ts, order = c(1, 0, 1))
# add in cpi as covariate
full_arima_model <- Arima(log_dif_eci_ts, order = c(1, 1, 1), xreg=xreg_matrix)
full_seasonal_arima_model <- Arima(log_dif_eci_ts, order = c(0,0,0), 
                                   seasonal = list(order=c(0,1,1),period=4), 
                                   xreg=xreg_matrix)
arima_model_wcpi <- Arima(log_dif_eci_ts, order = c(1, 0, 1), xreg = xreg_matrix[,1])
arima_model_wlagcpi <- Arima(log_dif_eci_ts, order = c(1, 1, 1), xreg = xreg_matrix[,2])


# Summary of the model
summary(full_arima_model)
summary(arima_model)
summary(arima_model_wcpi)
summary(arima_model_wlagcpi)
summary(full_seasonal_arima_model)

# AIC for each model
AIC(full_arima_model)
AIC(arima_model)
AIC(arima_model_wcpi)
AIC(arima_model_wlagcpi)
AIC(full_seasonal_arima_model)

BIC(full_arima_model)
BIC(arima_model)
BIC(arima_model_wcpi)
BIC(arima_model_wlagcpi)
BIC(full_seasonal_arima_model)

# Get draws of the model 

n_sims <- 1000
sim_length <- 4*30
ar101_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
full_model_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)

for(i in 1:n_sims){
  log_dif_cpi_sim <- simulate(cpi_arima_model, nsim = sim_length*3)
  # filter to only keep quarterly monthly values
  log_dif_cpi_sim <- log_dif_cpi_sim[seq(1, length(log_dif_cpi_sim), by = 3)]
  
  lagged_cpi_sim <- c(tail(quarterly_cpi$log_dif_cpi, n = 1), head(as.vector(log_dif_cpi_sim), n = -1))
  xreg_matrix_sim <- cbind(log_dif_cpi_sim, lagged_cpi_sim)
  ar101_sims[i,] <- simulate(arima_model, nsim = sim_length)
  full_model_sims[i,] <- simulate(arima_model_wlagcpi, nsim = sim_length, xreg = xreg_matrix_sim[,2])
}

# remove first column of each simulation
ar101_sims <- ar101_sims[,-1]
full_model_sims <- full_model_sims[,-1]

# get average simulation
ar101_avg <- colMeans(ar101_sims)
full_model_avg <- colMeans(full_model_sims)
# get 95% intervals
ar101_ci <- apply(ar101_sims, 2, quantile, probs = c(.025, .975))
full_model_ci <- apply(full_model_sims, 2, quantile, probs = c(.025, .975))

# plot simulation
plot(ar101_avg, type = "l", col = "red", ylim = c(-.008, .02))
lines(ar101_ci[2,], col = "red", lty = 2)
lines(ar101_ci[1,], col = "red", lty = 2)
lines(full_model_avg, col = "blue")
lines(full_model_ci[2,], col = "blue", lty = 2)
lines(full_model_ci[1,], col = "blue", lty = 2)
abline(h = .01, col = "black")
legend("bottomleft", legend = c("ARIMA(1,0,1)", "ARIMA(1,1,1) w/ lag cpi"), col = c("red", "blue"), lty = 1)

arima_sim <- simulate(arima_model, nsim = 4*12)
full_model_sim <- simulate(full_arima_model, nsim = 4*12, xreg = xreg_matrix_sim[,1])

#plot the simulation
autoplot(arima_sim) + ggtitle("ARIMA(1,0,1) Simulation")  + autolayer(log_dif_cpi_ts, series = "Actual") 
autoplot(full_model_sim) + ggtitle("Seasonal ARIMA(1,1,1)(1,0,2) Simulation") + autolayer(log_dif_cpi_ts, series = "Actual")



# Exploration of Model Residuals ------------------------------------------

# Get residuals
cpi_residuals <- cpi_arima_model$residuals[seq(3, length(cpi_arima_model$residuals)-3, by = 3)]
eci_residuals <- arima_model$residuals

cor(cpi_residuals, eci_residuals)
plot(cpi_residuals, eci_residuals)
cor(lag(cpi_residuals), eci_residuals, use = "complete.obs")
plot(lag(cpi_residuals), eci_residuals)
cor(lag(cpi_residuals, 2), eci_residuals, use = "complete.obs")
plot(lag(cpi_residuals, 2), eci_residuals)


## Explore relationship between draws

# Get draws of the model

cpi_sim <- simulate(cpi_arima_model, nsim = 20*12)
last_cpi_val <- tail(cpi_df$log_dif_cpi, n = 1)

quarterly_cpi_sim_df <- data.frame(
  date = zoo::as.Date(time(cpi_sim)),
  value = cpi_sim) |> 
  mutate(year = year(date),
         quarter = quarter(date)) |> 
  group_by(year, quarter) |> 
  summarize(date = max(date),
            log_dif_cpi = sum(value))

lagged_cpi_sim <- c(tail(quarterly_cpi$log_dif_cpi, n = 1), head(quarterly_cpi_sim_df$log_dif_cpi, n = -1))
xreg_vector <- lagged_cpi_sim
eci_sim <- simulate(arima_model_wlagcpi, nsim = 20*4, xreg = xreg_vector)

cor(quarterly_cpi_sim_df$log_dif_cpi, eci_sim)
cor(lagged_cpi_sim, eci_sim, use = "complete.obs")
plot(quarterly_cpi_sim_df$log_dif_cpi, eci_sim)



# Med ---------------------------------------------------------------------

med_df <- read_csv("data/med_inflation.csv") |> 
  filter(date >= "2010-01-01") |> 
  mutate(lagged_med = lag(med_inflation, n = 1)) |> 
  mutate(log_dif_med = log(med_inflation) - log(lagged_med)) |> 
  filter(!is.na(log_dif_med)) # remove first observation without lag

# Ensure the data is ordered and converted into time series
log_dif_med_ts <- ts(med_df$log_dif_med, start = c(min(year(med_df$date))), frequency = 12)
log_dif_cpi_ts <- ts(cpi_df$log_dif_cpi, start = c(min(year(cpi_df$date))), frequency = 12)

## Explore relationship between Med and CPI
cor(med_df$log_dif_med, cpi_df$log_dif_cpi)
cor(med_df$log_dif_med, lag(cpi_df$log_dif_cpi), use = "complete.obs")

## Explore relationship between Med and ECI

quarterly_med <- med_df |> 
  filter(month(date) %in% c(3, 6, 9, 12)) |>
  mutate(lagged_med = lag(med_inflation, n = 1)) |> 
  mutate(log_dif_med = log(med_inflation) - log(lagged_med),
         lagged_med = lag(log_dif_med, n = 1)) |> 
  filter(!is.na(log_dif_med) & !is.na(lagged_med)) # remove first observation without lag

## Explore relationship between Med and ECI
cor(quarterly_med$log_dif_med, eci_df$log_dif_eci)
cor(quarterly_med$lagged_med, eci_df$log_dif_eci)
cor(quarterly_med$log_dif_med, lag(eci_df$log_dif_eci, n = 1), use = "complete.obs")

plot(quarterly_med$log_dif_med, eci_df$log_dif_eci)
plot(quarterly_med$lagged_med, eci_df$log_dif_eci)
plot(quarterly_med$log_dif_med, lag(eci_df$log_dif_eci, n = 1))
plot(quarterly_med$lagged_med, lag(eci_df$log_dif_eci, n = 1)) # check to make sure lagging is correct






# plot of Med and CPI
plot(log_dif_med_ts, log_dif_cpi_ts)

# Fit ARIMA(1,0,1) model
arima_model <- Arima(log_dif_med_ts, order = c(1, 0, 1))

# add in cpi as covariate
full_arima<- Arima(log_dif_med_ts, order = c(1, 0, 1), xreg = log_dif_cpi_ts)

# optimal model from auto_arima
seasonal_arima <- Arima(log_dif_med_ts, order = c(1, 0, 1), seasonal = list(order = c(0, 0, 2), period = 12))

# add in cpi as covariate
full_seasonal_arima <- Arima(log_dif_med_ts, order = c(1, 0, 1), seasonal = list(order = c(0, 0, 2), period = 12),
                             xreg = log_dif_cpi_ts)

# Summary of the models
summary(arima_model)
summary(full_arima)
summary(seasonal_arima)
summary(full_seasonal_arima)

# AIC for each model
AIC(arima_model)
AIC(full_arima)
AIC(seasonal_arima)
AIC(full_seasonal_arima)

BIC(arima_model)
BIC(full_arima)
BIC(seasonal_arima)
BIC(full_seasonal_arima)

# Get draws of the model

n_sims <- 1000
sim_length <- 12*30
ar101_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
seasonal_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)

for(i in 1:n_sims){
  ar101_sims[i,] <- simulate(arima_model, nsim = sim_length)
  seasonal_sims[i,] <- simulate(seasonal_arima, nsim = sim_length)
}

# get average simulation
ar101_avg <- colMeans(ar101_sims)
seasonal_avg <- colMeans(seasonal_sims)
# get 95% intervals
ar101_ci <- apply(ar101_sims, 2, quantile, probs = c(.025, .975))
seasonal_ci <- apply(seasonal_sims, 2, quantile, probs = c(.025, .975))

# plot simulation
plot(ar101_avg, type = "l", ylim = c(-.009, .015), col = "red")
lines(ar101_ci[2,], col = "red", lty = 2)
lines(ar101_ci[1,], col = "red", lty = 2)
lines(seasonal_avg, col = "blue")
lines(seasonal_ci[2,], col = "blue", lty = 2)
lines(seasonal_ci[1,], col = "blue", lty = 2)
legend("bottomleft", legend = c("ARIMA(1,0,1)", "Seasonal ARIMA(1,0,1)(0,0,2)"), col = c("red", "blue"), lty = 1)

# Explore residuals from each model

# Get residuals
med_residuals <- arima_model$residuals
cpi_residuals <- cpi_arima_model$residuals

cor(med_residuals, cpi_residuals)
plot(med_residuals, cpi_residuals)



