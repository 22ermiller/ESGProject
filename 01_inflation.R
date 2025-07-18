

# Libraries
library(tidyverse)
library(tidymodels)
library(forecast)
library(modeltime)
library(timetk)

# Read in Data

cpi_df <- read_csv("data/cpi.csv") |> 
  filter(date >= "2010-01-01") |> 
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
  filter(!is.na(log_dif_cpi)) # remove first observation without lag

med_inflation_df <- read_csv("data/med_inflation.csv") |> 
  filter(date >= min(cpi_df$date)) |> # only need data that cpi has
  mutate(lagged_med = lag(med_inflation, n = 1)) |> 
  mutate(log_dif_med = log(med_inflation) - log(lagged_med)) |> 
    filter(!is.na(log_dif_med)) # remove first observation without lag

# get difference of the logs of each value


ggplot() +
  #geom_line(aes(x = eci$date, y = eci$log_dif_eci, color = "ECI")) +
  geom_line(aes(x = cpi_df$date, y = cpi_df$log_dif_cpi, color = "CPI")) +
  geom_line(aes(x = med_inflation_df$date, y = med_inflation_df$log_dif_med, color = "Medical Inflation")) +
  labs(
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  scale_color_manual(values = c("CPI" = "black", "Medical Inflation" = "red"))

ggplot() +
  geom_line(aes(x = eci_df$date, y = eci_df$log_dif_eci, color = "ECI")) +
  labs(
    title = "Log Difference of ECI",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  scale_color_manual(values = c("ECI" = "blue"))


full_inflation <- cpi_df |>
  full_join(eci_df, by = "date") |>
  full_join(med_inflation_df, by = "date")


GGally::ggpairs(full_inflation |> select(log_dif_eci, log_dif_cpi, log_dif_med))

## Explore lags
lag_df <- full_inflation |> 
  mutate(lagged = lag(eci, n = 1))

cor(lag_df$lagged, lag_df$cpi,  use = "complete.obs")


# ARIMA Model for CPI -------------------------------------------------------

cpi_df |> plot_time_series(date, log_dif_cpi)

cpi_df %>%
  pull(log_dif_cpi) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("CPI")

cpi_recipe <- recipe(log_dif_cpi~date, data = cpi_df) #%>%
  #step_date(date, features = c("year", "month"))

ar_model <- arima_reg() |> 
  set_engine("auto_arima")

cpi_cv_split <- time_series_split(cpi_df, assess = '5 years', cumulative  = TRUE)

cpi_ar_wf <- workflow() %>%
  add_recipe(cpi_recipe) %>%
  add_model(ar_model) %>%
  fit(data = training(cpi_cv_split))

cpi_cv_results <- modeltime_calibrate(cpi_ar_wf,
                                  new_data = testing(cpi_cv_split))

# Visualize CV results
p1 <- cpi_cv_results %>%
  modeltime_forecast(
    new_data = testing(cpi_cv_split),
    actual_data = cpi_df) %>%
  plot_modeltime_forecast(.interactive = FALSE)

## Evaluate the accuracy
cpi_cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast

cpi_fullfit <- cpi_cv_results %>%
  modeltime_refit(data = cpi_df)

names(cpi_fullfit)
cpi_fullfit$.model
eci_fullfit$.model

cpi_preds <- cpi_fullfit %>%
  modeltime_forecast(h = '30 years') %>%
  rename(date = .index, log_dif_cpi = .value) %>%
  select(date, log_dif_cpi) %>%
  full_join(cpi_df, by = 'date')

p2 <- cpi_fullfit %>%
  modeltime_forecast(h = '30 years', actual_data = cpi_df) %>%
  plot_modeltime_forecast(.interactive = FALSE)

plotly::subplot(p1, p2, nrows = 2)

# Get residuals
cpi_residuals <- cpi_fullfit %>%
  modeltime_residuals(new_data = cpi_df) %>%
  rename(date = .index, residual = .residuals) %>%
  select(date, residual)

# plot residuals
ggplot(data = cpi_residuals) +
  geom_line(aes(x = date, y = residual)) +
  labs(
    title = "CPI Residuals",
    x = "Date",
    y = "Value",
    color = "Legend"
  )

cpi_residuals %>%
  pull(residual) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Residuals")

# ARIMA Model for ECI ----------------------------------------------------


quarterly_cpi <- cpi_df |> 
  filter(month(date) %in% c(3, 6, 9, 12)) |>
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi),
         lagged_cpi = lag(log_dif_cpi, n = 1)) |> 
  filter(!is.na(log_dif_cpi) & !is.na(lagged_cpi))

eci_df <- read_csv("data/eci.csv") |> 
  filter(date >= min(quarterly_cpi$date)) |> # only need data that cpi has
  mutate(lagged_eci = lag(eci, n = 1)) |> 
  mutate(log_dif_eci = log(eci) - log(lagged_eci)) |> 
  filter(!is.na(log_dif_eci)) |> # remove first observation without lag
  left_join(quarterly_cpi |> select(date, log_dif_cpi, lagged_cpi), by = "date")

eci_df |> plot_time_series(date, log_dif_eci)


eci_df %>%
  pull(log_dif_eci) %>%
  forecast::ggAcf(., lag.max = 5*365) +
  ggtitle("ECI")

eci_recipe <- recipe(log_dif_eci~date+log_dif_cpi + lagged_cpi, data = eci_df)

ar_model <- arima_reg() |> 
  set_engine("auto_arima", ic = "bic")

eci_cv_split <- time_series_split(eci_df, assess = '2 years', cumulative  = TRUE)

eci_ar_wf <- workflow() %>%
  add_recipe(eci_recipe) %>%
  add_model(ar_model) %>%
  fit(data = training(eci_cv_split))

eci_cv_results <- modeltime_calibrate(eci_ar_wf,
                                  new_data = testing(eci_cv_split))

# Visualize CV results
p3 <- eci_cv_results %>%
  modeltime_forecast(
    new_data = testing(eci_cv_split),
    actual_data = eci_df) %>%
  plot_modeltime_forecast(.interactive = TRUE)

## Evaluate the accuracy
eci_cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast

eci_fullfit <- eci_cv_results %>%
  modeltime_refit(data = eci_df)

eci_preds <- eci_fullfit %>%
  modeltime_forecast(h = '5 years') %>%
  rename(date = .index, log_dif_eci = .value) %>%
  select(date, log_dif_eci) %>%
  bind_rows(eci_df)

# plot model output
ggplot(data = eci_preds) +
  geom_line(aes(x = date, y = log_dif_eci)) +
  labs(
    title = "ECI Forecast",
    x = "Date",
    y = "Value",
    color = "Legend"
  )

p4 <- eci_fullfit %>%
  modeltime_forecast(h = '30 years', actual_data = eci_df) %>%
  plot_modeltime_forecast(.interactive = FALSE)

plotly::subplot(p3, p4, nrows = 2)

# Get residuals
eci_residuals <- eci_fullfit %>%
  modeltime_residuals(new_data = eci_df) %>%
  rename(date = .index, residual = .residuals) %>%
  select(date, residual)

# plot residuals
ggplot(data = eci_residuals) +
  geom_line(aes(x = date, y = residual)) +
  labs(
    title = "ECI Residuals",
    x = "Date",
    y = "Value",
    color = "Legend"
  )

eci_residuals %>%
  pull(residual) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Residuals")


# ARIMA Model for Medical Inflation ------------------------------------------------------

med_inflation_df %>%
  pull(log_dif_med) %>%
  forecast::ggAcf(., lag.max = 3*365) +
  ggtitle("Medical Inflation")

med_recipe <- recipe(log_dif_med~date, data = med_inflation_df)

ar_model <- arima_reg() |> 
  set_engine("auto_arima")

med_cv_split <- time_series_split(med_inflation_df, assess = '20 years', cumulative  = TRUE)

med_ar_wf <- workflow() %>%
  add_recipe(med_recipe) %>%
  add_model(ar_model) %>%
  fit(data = training(med_cv_split))

med_cv_results <- modeltime_calibrate(med_ar_wf,
                                  new_data = testing(med_cv_split))

# Visualize CV results
p5 <- med_cv_results %>%
  modeltime_forecast(
    new_data = testing(med_cv_split),
    actual_data = med_inflation_df) %>%
  plot_modeltime_forecast(.interactive = TRUE)

## Evaluate the accuracy
med_cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast

med_fullfit <- med_cv_results %>%
  modeltime_refit(data = med_inflation_df)

# get residuals from model
med_fullfit$.model

med_preds <- med_fullfit %>%
  modeltime_forecast(h = '30 years') %>%
  rename(date = .index, log_dif_med = .value) %>%
  select(date, log_dif_med) %>%
  bind_rows(med_inflation_df)

p6 <- med_fullfit %>%
  modeltime_forecast(h = '30 years', actual_data = med_inflation_df) %>%
  plot_modeltime_forecast(.interactive = FALSE)

plotly::subplot(p5, p6, nrows = 2)

# Get residuals
med_residuals <- med_fullfit %>%
  modeltime_residuals(new_data = med_inflation_df) %>%
  rename(date = .index, residual = .residuals) %>%
  select(date, residual)

# plot residuals
ggplot(data = med_residuals) +
  geom_line(aes(x = date, y = residual)) +
  labs(
    title = "Med Residuals",
    x = "Date",
    y = "Value",
    color = "Legend"
  )

med_residuals %>%
  pull(residual) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Residuals")


# Explore Correlations between residuals ----------------------------------

residuals_df <- cpi_residuals |> 
  rename(cpi_residual = residual) |>
  full_join(eci_residuals |> rename(eci_residual = residual), by = "date") |> 
  full_join(med_residuals |> rename(med_residual = residual), by = "date")

GGally::ggpairs(residuals_df |> select(-date))


