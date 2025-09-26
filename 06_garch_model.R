## Interest Rate Model ##

## Libraries
library(tidyverse)
library(gganimate)
library(patchwork)
library(forecast)
library(modeltime)
library(timetk)
library(rugarch)
library(fGarch)

cpi_df_raw <- read_csv("data/cpi.csv") 

cpi_df <- cpi_df_raw |> 
  filter(date >= "1990-01-01") |> # only dates from 2010 onwards
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
  filter(!is.na(log_dif_cpi))

interest_df <- read_csv("data/ir3mo.csv") |> 
  mutate(lagged_rate = lag(rate, n = 1),
         rate_rmmean = rate - mean(rate, na.rm = TRUE)) |> 
  mutate(dif_rate = rate - lagged_rate,
         log_dif_rate = log(rate) - log(lagged_rate))


# Implement Transformation ------------------------------------------------

r_bar <- .005

model_df <- interest_df |> dplyr::select(date, dif_rate, rate) |> 
  left_join(cpi_df, by = c("date" = "date")) |>
  mutate(rate = rate / 100) |> 
  mutate(rate = ifelse(rate == 0, .0001, rate)) |> # can't have 0 for transformation
  mutate(transformed_rate = ifelse(rate > r_bar,
                                   rate,
                                   r_bar - r_bar*log(r_bar) + r_bar * log(rate))) |> 
  mutate(transformed_dif = transformed_rate - lag(transformed_rate, n = 1,),
         transformed_rmmean = transformed_rate - mean(transformed_rate, na.rm = TRUE)) |> 
  filter(!is.na(transformed_dif)) 


# EDA ---------------------------------------------------------------------

p1 <- ggplot(data = interest_df) +
  geom_line(aes(x = date, y = .01*rate)) + 
  labs(title = "Interest Rate")

p2 <- ggplot(data = model_df) +
  geom_line(aes(x = date, y = transformed_rate)) + 
  labs(title = "Transformed Interest Rate")

p1 + p2

ggplot(data = model_df) +
  geom_line(aes(x = date, y = transformed_rmmean)) + 
  labs(title = "Transformed Interest Rate")

# Explore Correlation between transformed Rate and CPI 
cor(model_df$transformed_rate, model_df$log_dif_cpi)

par(mfrow = c(1, 1))
plot(model_df$transformed_rate, model_df$log_dif_cpi)
abline(h = .003)

# Modelling ---------------------------------------------------------------

# Check for hetereoskedasticity by fitting AR(1,0,1) and plotting ACF

# set up data
cpi <- as.matrix(model_df$log_dif_cpi)
transformed_rate_ts <- tk_ts(model_df$transformed_rate,start = c(min(year(model_df$date)), month(min(model_df$date))),frequency = 12)

# fit AR(1,0,1) model


transformed_ar1 <- Arima(transformed_rate_ts, order = c(1, 0, 1), include.constant = TRUE)
summary(transformed_ar1)

transformed_ar <- Arima(transformed_rate_ts, order = c(1, 0, 1), xreg = cpi)
summary(transformed_ar)

# save transformed_ar mod

saveRDS(transformed_ar, file = "models/temp_interest_mod.rds")

# fit ARIMA model using auto.arima

transformed_auto_ar1 <- auto.arima(transformed_rate_ts, method = "ML")
summary(transformed_auto_ar1)

transformed_auto_ar <- auto.arima(transformed_rate_ts, xreg = cpi, method = "ML")
summary(transformed_auto_ar)


# Evaluate residuals from AR models

ar_residuals <- resid(transformed_ar)
auto.ar_residuals <- resid(transformed_auto_ar)

p4 <- plot(ar_residuals)
plot(ar_residuals)
plot(auto.ar_residuals)

ggAcf(ar_residuals, lag.max = 200)
ggAcf(auto.ar_residuals, lag.max = 200)


# GARCH Model -------------------------------------------------------------

# garch model without cpi



garch_ts <- tk_ts(model_df$transformed_rmmean, start = c(min(year(model_df$date)), month(min(model_df$date))), frequency = 12)
head(garch_ts, n = 1) # starting value of time series

# filter ts to be after 1995
garch_ts2 <- subset(garch_ts, start = 60)
garch_ts2 <- garch_ts-mean(garch_ts)
head(garch_ts2, n = 1) # starting value of time series

plot(garch_ts2, type = "l")

dif_ts <- diff(garch_ts, lag = 1)

par(mfrow = c(1, 1))
plot(dif_ts, type = "l")
abline(h = 0)

# Model without cpi
simple_garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(1, 1), include.mean = FALSE
                                ),
                                distribution.model = "norm")

simple_garch_fit <- ugarchfit(spec = simple_garch_spec, data = garch_ts)

# Model with cpi
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(1, 1)
                                           , external.regressors = cpi, include.mean = TRUE
                         ),
                         distribution.model = "norm")

garch_fit <- ugarchfit(spec = garch_spec, data = garch_ts)

saveRDS(garch_fit, file = "models/interest_garch.rds")

garch_fit2 <- ugarchfit(spec = garch_spec, data = garch_ts2)
dif_garch_fit <- ugarchfit(spec = garch_spec, data = dif_ts)

garch_fit
garch_fit2
dif_garch_fit

# fGarch

fgarch_fit <- garchFit(formula = transformed_rate ~ arma(1, 1) + garch(1,1), 
                data = model_df, 
                cond.dist = "norm")

# create spec object with fitted values from garchFit to perform simulation
fgarch_spec <- garchSpec(model = list(mu = 0.0003207451,
                                      ar = 0.9954789885,
                                      ma = 0.1775936027,
                                      omega = 0.0000003977,
                                      alpha = 0.4892283328,
                                      beta = 0.5819735021))


dif_fgarch_fit <- garchFit(formula = transformed_dif ~ arma(1, 1) + garch(1,1), 
                data = model_df, 
                cond.dist = "norm")

summary(fgarch_fit)
summary(dif_fgarch_fit)

library(rugarch)

mu <- coef(garch_fit)[1]

# Evaluate residuals

model_output_df <- model_df |> 
  mutate(fitted_rate = fitted(garch_fit),
         residuals = residuals(garch_fit),
         sigma = sigma(garch_fit),
         standardized_residuals = residuals / sigma)

p3 <- ggplot(data = model_output_df) +
  geom_line(aes(x = date, y = standardized_residuals)) +
  labs(title = "GARCH Residuals")

p3

hist(model_output_df$standardized_residuals, breaks = 50)
hist(ar_residuals, breaks = 50)

acf(model_output_df$standardized_residuals, lag.max = 200)
acf(ar_residuals, lag.max = 200)

p4 <- ggplot(data = model_df) +
  geom_line(aes(x = date, y = ar_residuals)) +
  labs(title = "AR(1,0,1) Residuals")

p3 + p4



# Simulations -------------------------------------------------------------

back_transform <- function(x) {
  r_bar <- .005
  if(x > r_bar){
    return(x)
  } else {
    return(exp((x - r_bar + r_bar*log(r_bar))/r_bar))
  }
}

cpi_mod <- readRDS("models/cpi_mod.rds")

single_interest_simulation <- function(sim_length = 20*12) {
  
  # AR101 simulation
  ar_sim <- apply(as.matrix(simulate(transformed_ar1, nsim = sim_length)), 1, back_transform)
  
  # interest rate w/out cpi simulation
  simple_garch_sim <- apply(as.matrix(ugarchsim(simple_garch_fit, n.sim = sim_length, startMethod = "sample")@simulation$seriesSim),
                            1, back_transform)
  
  # cpi simulation
  cpi_sim <- matrix(simulate(cpi_mod, nsim = sim_length), ncol = 1)
  mexsim_list <- list(cpi_sim)
  
  sim_results <- ugarchsim(garch_fit, n.sim = sim_length, startMethod = "sample", mexsimdata = mexsim_list)@simulation$seriesSim +
    mean(model_df$transformed_rate)
  
  garch_sim <- apply(as.matrix(sim_results),
                     1, back_transform)
  
  dif_garch_sim <- ugarchsim(dif_garch_fit, n.sim = sim_length, startMethod = "sample", mexsimdata = mexsim_list)@simulation$seriesSim
  # back transform the differences
  undifferenced_sim <- rep(tail(garch_ts, n = 1), length(dif_garch_sim)) + 
    cumsum(dif_garch_sim)
  
  final_dif_garch_sim <- apply(as.matrix(undifferenced_sim), 1, back_transform)
  
  # fGarch simulation
  #fgarch_sim <- apply(as.matrix(garchSim(fgarch_spec, n = sim_length)), 1, back_transform)
  
  # add tail to the differences
  full_ts <- c(garch_ts, final_dif_garch_sim)
  #plot(full_ts, type = "l")
  

  sim_cor <- cor(cpi_sim, garch_sim)
  
  # plot simulation
  par(mfrow = c(1, 1))
  #plot(cpi_sim, type = "l", main = "CPI Simulation", ylab = "Rate")
  plot(garch_sim, type = "l", main = "ruGarch Simulation", ylab = "Rate")
  #plot(dif_garch_sim, type = "l", main = "Differenced ruGarch Simulation", ylab = "Rate")
  abline(h =0)
  #plot(final_dif_garch_sim, type = "l", main = "Back Transformed Differences",
       #sub = paste("Correlation:", round(sim_cor, 3)), ylab = "Rate")
}

# empirical correlation is 0.09953081

single_interest_simulation()

# multiple simulations

multiple_interest_simulation <- function(n_sims = 1000, sim_length = 80*12) {
  
  # AR101 simulation
  ar_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
  cpi_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
  garch_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
  
  for(i in 1:n_sims){
    ar_sims[i,] <- apply(as.matrix(simulate(transformed_ar1, nsim = sim_length)), 1, back_transform)
    cpi_sims[i,] <- simulate(cpi_mod, nsim = sim_length)
    mexsim_list <- list(matrix(cpi_sims[i,], ncol = 1))
    sim_results <- ugarchsim(garch_fit, n.sim = sim_length, startMethod = "sample", mexsimdata = mexsim_list)@simulation$seriesSim + 
      mean(model_df$transformed_rate)
    garch_sims[i,] <- apply(as.matrix(sim_results),
                            1, back_transform)
    
  }
  # 
  # ar_avg <- colMeans(ar_sims)
  cpi_avg <- colMeans(cpi_sims)
  garch_avg <- colMeans(garch_sims)
  # 
  # ar_ci <- apply(ar_sims, 2, quantile, probs = c(.025, .975))
  cpi_ci <- apply(cpi_sims, 2, quantile, probs = c(.025, .975))
  garch_ci <- apply(garch_sims, 2, quantile, probs = c(.025, .975))
  
  # plot simulation
  par(mfrow = c(1, 1))
  # plot(ar_avg, type = "l", ylim = c(lower_bound, upper_bound), main = "AR Simulation")
  # lines(ar_ci[2,], col = "red", lty = 2)
  # lines(ar_ci[1,], col = "red", lty = 2)
  # 
  # plot(dif_garch_avg, type = "l", ylim = c(lower_bound, upper_bound), main = "Dif GARCH Simulation")
  # lines(dif_garch_ci[2,], col = "red", lty = 2)
  # lines(dif_garch_ci[1,], col = "red", lty = 2)
  # 
  plot(garch_avg, type = "l", ylim = c(0,.3), main = "GARCH Simulation w/ CPI")
  lines(garch_ci[2,], col = "red", lty = 2)
  lines(garch_ci[1,], col = "red", lty = 2)
  abline(h = mean(model_df$transformed_rate), col = "blue", lty = 2)
  
  # plot(ar_avg, type = "l",col = "red", ylim = c(lower_bound, upper_bound), main = "All Simulations")
  # lines(ar_ci[2,], col = "red", lty = 2)
  # lines(ar_ci[1,], col = "red", lty = 2)
  # lines(dif_garch_avg, type = "l", col = "blue")
  # lines(dif_garch_ci[2,], col = "blue", lty = 2)
  # lines(dif_garch_ci[1,], col = "blue", lty = 2)
  # lines(garch_avg, type = "l", col = "green")
  # lines(garch_ci[2,], col = "green", lty = 2)
  # lines(garch_ci[1,], col = "green", lty = 2)
  # #abline(h = mu)
  # legend("topleft", legend = c("AR", "GARCH", "GARCH w/ CPI"), col = c("red", "blue", "green"), lty = 1, cex = 0.5)
  # 
}

multiple_interest_simulation()

# Evaluate Draws From All Models ------------------------------------------


eci_mod <- readRDS("models/eci_mod.rds")
med_mod <- readRDS("models/med_mod.rds")

quarterly_cpi <- cpi_df |> 
  filter(month(date) %in% c(3, 6, 9, 12)) |>
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi),
         lagged_cpi = lag(log_dif_cpi, n = 1)) |> 
  filter(!is.na(log_dif_cpi) & !is.na(lagged_cpi)) # remove first observation without lag



single_inflation_simulation <- function(sim_length = 20*12) {
  
  # single cpi simulation
  cpi_sim <- simulate(cpi_mod, nsim = sim_length)
  
  quarterly_cpi_sim_df <- data.frame(
    date = zoo::as.Date(time(cpi_sim)),
    value = cpi_sim) |> 
    mutate(year = year(date),
           quarter = quarter(date)) |> 
    group_by(year, quarter) |> 
    summarize(date = max(date),
              log_dif_cpi = sum(value))
  
  lagged_cpi_sim <- c(tail(quarterly_cpi$log_dif_cpi, n = 1), head(quarterly_cpi_sim_df$log_dif_cpi, n = -1))
  eci_sim <- simulate(eci_mod, nsim = 20*4, xreg = lagged_cpi_sim)
  
  simulation_cor <- cor(lagged_cpi_sim, eci_sim)
  # plot both simulations side by side
  par(mfrow = c(1, 2))
  plot(lagged_cpi_sim, type = 'l', main = "CPI Simulation")
  plot(eci_sim, type = 'l', main = "ECI Simulation")
  
  # Add a main title for both plots
  mtext(paste("Correlation:", round(simulation_cor, 3)), outer = TRUE, cex = 1.2, line = -1)
}

# observed correlation between CPI and ECI is .577
single_inflation_simulation()



