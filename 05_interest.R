library(tidyverse)
library(gganimate)
library(patchwork)

cpi_df_raw <- read_csv("data/cpi.csv") 

cpi_df <- cpi_df_raw |> 
  filter(date >= "1990-01-01") |> # only dates from 2010 onwards
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
  filter(!is.na(log_dif_cpi))

interest_df <- read_csv("data/interest_rate.csv") |> 
  rename(date = observation_date,
         rate = DGS3MO) |> 
  filter(!is.na(rate)) |> 
  mutate(year = year(date),
         month = month(date),
         day = day(date)) |> 
  group_by(year, month) |> 
  arrange(day) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  mutate(date = floor_date(date, unit = "month")) |>
  filter(date >= min(cpi_df$date) - months(1) & date <= max(cpi_df$date)) |> 
  mutate(lagged_rate = lag(rate, n = 1)) |> 
  mutate(dif_rate = rate - lagged_rate,
    log_dif_rate = log(rate) - log(lagged_rate)) |> 
  filter(!is.na(log_dif_rate))
  


# EDA ---------------------------------------------------------------------


ggplot(data = interest_df) +
  geom_line(aes(x = date, y = rate))

ggplot(data = interest_df) +
  geom_line(aes(x = date, y = dif_rate))

cor(interest_df$rate, cpi_df$log_dif_cpi)
cor(interest_df$rate, lag(cpi_df$log_dif_cpi), use = "complete.obs")

cors <- rep(NA, 50)

for (i in 1:50) {
  cors[i] <- cor(interest_df$rate, lag(cpi_df$log_dif_cpi, n = i), use = "complete.obs")
}

plot(cors, type = "l")

par(mfrow = c(2,2))
plot(interest_df$rate, cpi_df$log_dif_cpi, 
     main = "No Lag", xlab = "Interest Rate", ylab = "CPI")
plot(interest_df$rate, lag(cpi_df$log_dif_cpi, n = 5), 
     main = "Lag 5", xlab = "Interest Rate", ylab = "CPI")
plot(interest_df$rate, lag(cpi_df$log_dif_cpi, n = 10), 
     main = "Lag 10", xlab = "Interest Rate", ylab = "CPI")
plot(interest_df$rate, lag(cpi_df$log_dif_cpi, n = 19), 
     main = "Lag 19", xlab = "Interest Rate", ylab = "CPI")


# Different Inflation Rates ---------------------------------------------------

cors <- rep(NA, 300)

for (i in 1:300) {
  
  test_df <- cpi_df_raw |> 
    mutate(lag1 = lag(cpi, n = i)) |> 
    mutate(log_dif_cpi = log(cpi) - log(lag1))
  
  full_df <- interest_df |>
    left_join(test_df, by = c("date" = "date")) |>
    filter(!is.na(log_dif_cpi))
  
  cors[i] <- cor(full_df$rate, full_df$log_dif_cpi)
  
  ggplot(data = full_df) +
    geom_point(aes(x = rate, y = log_dif_cpi))
  
}

rolling_year_cpi <- cpi_df_raw |> 
  mutate(month_lag = lag(cpi, n = 1),
         year_lag = lag(cpi, n = 12),
         quarter_lag = lag(cpi, n = 4),
         six_month_lag = lag(cpi, n = 6),
         x_month_lag = lag(cpi, n = 80),
         y_month_lag = lag(cpi, n = 130)) |> 
  mutate(log_dif_cpi = log(cpi) - log(month_lag),
        log_yeardif_cpi = log(cpi) - log(year_lag),
         log_quarterdif_cpi = log(cpi) - log(quarter_lag),
         log_sixmonthdif_cpi = log(cpi) - log(six_month_lag),
         log_xmonthdif_cpi = log(cpi) - log(x_month_lag),
         log_ymonthdif_cpi = log(cpi) - log(y_month_lag))

full_df <- interest_df |> 
  left_join(rolling_year_cpi, by = c("date" = "date"))

cor(full_df$rate, full_df$log_yeardif_cpi)

ggplot(data = full_df) +
  geom_point(aes(x = rate, y = log_yeardif_cpi)) +
  labs(title = "Interest Rate vs. Rolling Year Inflation")

cor(full_df$rate, full_df$log_quarterdif_cpi)

ggplot(data = full_df) +
  geom_point(aes(x = rate, y = log_quarterdif_cpi)) +
  labs(title = "Interest Rate vs. Rolling Quarter Inflation")

cor(full_df$rate, full_df$log_sixmonthdif_cpi)

ggplot(data = full_df) +
  geom_point(aes(x = rate, y = log_sixmonthdif_cpi)) +
  labs(title = "Interest Rate vs. Rolling Six Month Inflation")

cor(full_df$rate, full_df$log_xmonthdif_cpi)

ggplot(data = full_df) +
  geom_point(aes(x = rate, y = log_xmonthdif_cpi))

cor(full_df$rate, full_df$log_ymonthdif_cpi)

ggplot(data = full_df) +
  geom_point(aes(x = rate, y = log_ymonthdif_cpi))



# Animation ---------------------------------------------------------------

library(gganimate)

# Initialize storage for data frames
plot_data <- list()

for (i in 1:300) {
  
  test_df <- cpi_df_raw |> 
    mutate(lag1 = lag(cpi, n = i)) |> 
    mutate(log_dif_cpi = log(cpi) - log(lag1))
  
  full_df <- interest_df |>
    left_join(test_df, by = c("date" = "date")) |>
    filter(!is.na(log_dif_cpi))
  
  # Store the data with a new column for frame tracking
  full_df$lag_value <- i
  plot_data[[i]] <- full_df
}

# Combine all frames into a single data frame
animation_df <- bind_rows(plot_data)

# Create animated scatter plot
p <- ggplot(animation_df, aes(x = rate, y = log_dif_cpi)) +
  geom_point(alpha = 0.7) +
  labs(title = "Correlation of Short-Term Interest Rates and Inflation",
       subtitle = "Lag: {frame_time}",
       x = "Interest Rate",
       y = "Log Difference in CPI") +
  transition_time(lag_value) +  # Creates the animation over different lag values
  ease_aes('linear')  # Smooth transition

gifski(png_files = list.files(pattern = "gganim_plot.*.png"), 
       gif_file = "animation.gif", 
       width = 800, height = 600, delay = 0.05)

# Save animation
animate(p, duration = 10, fps = 20, width = 800, height = 600)
anim_save("inflation_interest_animation.gif")

# Create animated scatter plot
p <- ggplot(animation_df, aes(x = date, y = log_dif_cpi)) +
  geom_line() +
  labs(title = "Inflation",
       subtitle = "Lag: {frame_time}",
       x = "Date",
       y = "Log Difference in CPI") +
  transition_time(lag_value) +  # Creates the animation over different lag values
  ease_aes('linear')  # Smooth transition

gifski(png_files = list.files(pattern = "gganim_plot.*.png"), 
       gif_file = "animation2.gif", 
       width = 800, height = 600, delay = 0.05)

# Save animation
animate(p, duration = 10, fps = 20, width = 800, height = 600)
anim_save("inflation_interest_animation.gif")

## Change in interest vs. Log Dif CPI

full_df <- interest_df |> 
  left_join(cpi_df, by = c("date" = "date"))

p1 <- ggplot(data = full_df) +
  geom_line(aes(x = date, y = dif_rate))

p2 <- ggplot(data = full_df) +
  geom_line(aes(x = date, y = log_dif_cpi))

p1 + p2

ggplot(data = full_df) +
  geom_point(aes(x = dif_rate, y = log_dif_cpi))



cor(full_df$dif_rate, full_df$log_dif_cpi)

temp_df <- full_df |> filter(rate != 0 & lagged_rate != 0)
cor(temp_df$log_dif_rate, temp_df$log_dif_cpi)

ggplot(data = temp_df) +
  geom_point(aes(x = log_dif_rate, y = log_dif_cpi))


# Modelling ---------------------------------------------------------------

library(forecast)
library(modeltime)
library(tidymodels)
library(timetk)

full_df %>%
  pull(dif_rate) %>%
  forecast::ggAcf(., lag.max = 200) +
  ggtitle("Interest Rate")

arima_mod <- Arima(full_df$dif_rate, order = c(1, 0, 0),# seasonal = c(12,0,0)
                   )
summary(arima_mod)

arima_mod2 <- Arima(full_df$rate, order = c(1, 0, 1))
summary(arima_mod2)

arima_mod3 <- Arima(full_df$rate, order = c(3, 0, 1))
summary(arima_mod3)

arima_mod4 <- Arima(full_df$rate, order = c(3, 0, 2))
summary(arima_mod4)

arima_mod <- Arima(full_df$dif_rate, order = c(1, 0, 0), xreg = full_df$log_dif_cpi)
summary(arima_mod)

arima_mod2 <- Arima(full_df$dif_rate, order = c(1, 0, 1))
summary(arima_mod2)

arima_mod3 <- Arima(full_df$rate, order = c(3, 0, 1), xreg = full_df$log_dif_cpi)
summary(arima_mod3)

arima_mod4 <- Arima(full_df$rate, order = c(3, 0, 2), xreg = full_df$log_dif_cpi)
summary(arima_mod4)


# Auto Arima --------------------------------------------------------------

ir_ts <- tk_ts(full_df$dif_rate)
log_dif_cpi_ts <- tk_ts(full_df$log_dif_cpi)

ir_auto_arima_mod <- auto.arima(ir_ts, xreg = log_dif_cpi_ts, method = "ML")
summary(ir_auto_arima_mod)

ir_arima <- Arima(ir_ts, order = c(1, 0, 1), xreg = log_dif_cpi_ts, method = "ML")
summary(ir_arima)

# Simulation --------------------------------------------------------------

# inflation model

# Fit ARIMA(1,0,1) model
cpi_arima_model <- Arima(log_dif_cpi_ts, order = c(1, 0, 1))

# Get draws of the model 

n_sims <- 1000
sim_length <- 12*30
log_dif_cpi_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
full_model_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
red_model_sims <- matrix(NA, nrow = n_sims, ncol = sim_length)

for(i in 1:n_sims){
  log_dif_cpi_sim <- simulate(cpi_arima_model, nsim = sim_length)
  
  log_dif_cpi_sims[i,] <- log_dif_cpi_sim
  full_model_sims[i,] <- simulate(ir_arima, nsim = sim_length, xreg = log_dif_cpi_sim)
  red_model_sims[i,] <- simulate(ir_auto_arima_mod, nsim = sim_length, xreg = log_dif_cpi_sim)
}

full_model_avg <- colMeans(full_model_sims)
full_model_ci <- apply(full_model_sims, 2, quantile, probs = c(.025, .975))

red_model_avg <- colMeans(red_model_sims)
red_model_ci <- apply(red_model_sims, 2, quantile, probs = c(.025, .975))

# plot simulation
plot(full_model_avg, type = "l", col = "red", ylim = c(-.5,.5))
lines(full_model_ci[2,], col = "red", lty = 2)
lines(full_model_ci[1,], col = "red", lty = 2)
lines(red_model_avg, col = "blue")
lines(red_model_ci[2,], col = "blue", lty = 2)
lines(red_model_ci[1,], col = "blue", lty = 2)
legend("topright", legend = c("Arima()", "auto.arima()"), col = c("red", "blue"), lty = 1)

# plot single simulation
plot(full_model_sims[1,], type = "l", col = "red")


# auto.arima() Single Simulation --------------------------------------------

# plot full simulation
cpi_sim <- simulate(cpi_arima_model, nsim = sim_length)
single_sim <- simulate(ir_auto_arima_mod, nsim = sim_length, xreg = cpi_sim)

ggplot() + 
  geom_line(aes(x = time(single_sim), y = single_sim, color = "Simulated")) + 
  geom_line(aes(x = time(ir_ts), y = ir_ts, color = "Actual")) + 
  ggtitle("Single Simulation") + 
  xlab("Year") + 
  ylab("Interest Rate") + 
  scale_color_manual(values = c("Simulated" = "red", "Actual" = "black")) + 
  theme_minimal() +
  labs(color = "") +  # Label the legend
  theme(legend.position = "top") 

test_df <- data.frame(date = seq.Date(from = max(full_df$date), by = "month", length.out = sim_length),
                       dif_rate = single_sim) |> 
  mutate(rate = cumsum(dif_rate) + full_df$rate[nrow(full_df)])

ggplot(data = test_df) +
  geom_line(aes(x = date, y = rate)) +
  geom_line(data = full_df, aes(x = date, y = rate), color = "red")


# Arima() Single Simulation -----------------------------------------------

# plot full simulation
cpi_sim <- simulate(cpi_arima_model, nsim = sim_length)
single_sim <- simulate(ir_arima, nsim = sim_length, xreg = cpi_sim)

ggplot() + 
  geom_line(aes(x = time(single_sim), y = single_sim, color = "Simulated")) + 
  geom_line(aes(x = time(ir_ts), y = ir_ts, color = "Actual")) + 
  ggtitle("Single Simulation") + 
  xlab("Year") + 
  ylab("Interest Rate") + 
  scale_color_manual(values = c("Simulated" = "red", "Actual" = "black")) + 
  theme_minimal() +
  labs(color = "") +  # Label the legend
  theme(legend.position = "top") 

test_df <- data.frame(date = seq.Date(from = max(full_df$date), by = "month", length.out = sim_length),
                      dif_rate = single_sim) |> 
  mutate(rate = cumsum(dif_rate) + full_df$rate[nrow(full_df)])

ggplot(data = test_df) +
  geom_line(aes(x = date, y = rate)) +
  geom_line(data = full_df, aes(x = date, y = rate), color = "red")


# get correlations between rows of full_model_sims and log_dif_cpi_sims

correlations <- purrr::map2_dbl(asplit(full_model_sims, 1), asplit(log_dif_cpi_sims, 1), cor)
mean(correlations)
hist(correlations)


# Piece-wise transform GARCH model ----------------------------------------------

## Info on GARCH models:
## https://datawookie.dev/blog/2024/04/what-is-a-garch-model/

library(rugarch)

r_bar <- .005


model2_df <- interest_df |> select(date, dif_rate, rate) |> 
  left_join(cpi_df, by = c("date" = "date")) |>
  mutate(rate = rate / 100) |> 
  mutate(rate = ifelse(rate == 0, .0001, rate)) |> # can't have 0 for transformation
  mutate(transformed_rate = ifelse(rate > r_bar,
                                   rate,
                                   r_bar - r_bar*log(r_bar) + r_bar * log(rate)))

ggplot(data = interest_df) +
  geom_line(aes(x = date, y = rate))

ggplot(data = model2_df) +
  geom_line(aes(x = date, y = transformed_rate))

# Check for hetereoskedasticity by fitting AR(1,0,1) and plotting ACF

regressors <- as.matrix(model2_df$log_dif_cpi)

transformed_rate_ts <- tk_ts(model2_df$transformed_rate,start = c(min(year(model2_df$date)), month(min(model2_df$date))),frequency = 12)

transformed_ar <- Arima(transformed_rate_ts, order = c(1, 0, 1), xreg = regressors)

transformed_auto_ar <- auto.arima(transformed_rate_ts, xreg = regressors, method = "ML")

summary(transformed_auto_ar)

residuals <- resid(transformed_ar)

plot(residuals)

ggAcf(residuals, lag.max = 200)

garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(1, 1)
                                           , external.regressors = regressors
                                           ),
                         distribution.model = "norm")

garch_fit <- ugarchfit(spec = garch_spec, data = model2_df$transformed_rate)

garch_fit

model2_df <- model2_df |> 
  mutate(fitted_rate = fitted(garch_fit),
         residuals = residuals(garch_fit),
         sigma = sigma(garch_fit),
         standardized_residuals = residuals / sigma)

ggplot(data = model2_df) +
  geom_line(aes(x = date, y = standardized_residuals))

ggplot(data = model2_df) +
  geom_line(aes(x = date, y = transformed_rate)) +
  geom_line(aes(x = date, y = fitted_rate), color = "red")



ggplot(data = model2_df) +
  geom_line(aes(x = date, y = sigma))
