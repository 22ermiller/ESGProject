library(tidyverse)
library(vars)
library(forecast)
library(modeltime)
library(rugarch)
library(tsDyn)

# Read in Data

r_bar <- .005

full_ir <- read_csv("data/full_ir.csv") |> 
  mutate(across(three_month:thirty_year, ~./100)) |> 
  mutate(across(three_month:thirty_year, ~ifelse(. == 0, .0001, .))) |> # can't have 0 for transformation
  mutate(across(three_month:thirty_year, ~ifelse(. > r_bar, ., r_bar - r_bar*log(r_bar) + r_bar * log(.)))) |> # transformation
  # calculate slope and curvature
  mutate(slope = thirty_year - three_month,
         curve = three_month + thirty_year - (2*ten_year))


# VAR Model ---------------------------------------------------------------

X <- cbind(slope = full_ir$slope, curve = full_ir$curve)

X_ts <- ts(X, start = c(min(year(full_ir$date)), month(min(full_ir$date))), frequency = 12)

var_model <- lineVar(X_ts, lag = 1, include = "const", exogen = full_ir$three_month)

summary(var_model)
saveRDS(var_model, "models/yield_curve_var_mod.rds")

# Forecasting -------------------------------------------------------------

# Read in short rate and cpi models

ir3mo_mod <- readRDS("models/interest_garch.rds")
cpi_mod <- readRDS("models/cpi_mod.rds")

sim_length = 400

cpi_sim <- matrix(simulate(cpi_mod, nsim = sim_length), ncol = 1)

ir3mo_sim <- ugarchsim(ir3mo_mod, n.sim = sim_length, startMethod = "sample", mexsimdata = list(cpi_sim))@simulation$seriesSim +
  mean(full_ir$three_month)


# Do I need to incorporate the covariance matrix in the simulation
sample_cov <- cov(var_model$residuals)
var_sim <- VAR.sim(var_model$coefficients,
                   n = 400, 
                   starting = matrix(c(tail(full_ir$slope, 1), tail(full_ir$curve, 1)), nrow = 1),
                   varcov = sample_cov,
                   exogen = ir3mo_sim)


plot(var_sim[,1], type = "l")
plot(var_sim[,2], type = "l")

# Multiple simulations



n_sim = 100
sim_length = 480

slope_sim <- matrix(NA, nrow = n_sim, ncol = sim_length)
curve_sim <- matrix(NA, nrow = n_sim, ncol = sim_length)

for (i in 1:n_sim) {
  var_sim <- VAR.sim(var_model$coefficients,
                     n = sim_length,
                     starting = matrix(c(tail(full_ir$slope, 1), tail(full_ir$curve, 1)), nrow = 1),
                     varcov = sample_cov,
                     exogen = ir3mo_sims[i,]
                     )
  
  slope_sim[i, ] <- var_sim[,1]
  curve_sim[i, ] <- var_sim[,2]
}

slope_avg <- colMeans(slope_sim)
curve_avg <- colMeans(curve_sim)

slope_ci <- apply(slope_sim, 2, quantile, probs = c(.025, .975))
curve_ci <- apply(curve_sim, 2, quantile, probs = c(.025, .975))

plot(slope_avg, type = "l",ylim = c(-.05, .1), main = "Slope Sims")
lines(slope_ci[2,], col = "red", lty = 2)
lines(slope_ci[1,], col = "red", lty = 2)
abline(h = mean(full_ir$slope), col = "blue", lty = 2)

plot(curve_avg, type = "l",ylim = c(-.05, .04), main = "Curve Sims")
lines(curve_ci[2,], col = "red", lty = 2)
lines(curve_ci[1,], col = "red", lty = 2)
abline(h = mean(full_ir$curve), col = "blue", lty = 2)



# Yield Curve Weightings --------------------------------------------------

one_year.lm <- lm(one_year ~ three_month + slope + curve, data = full_ir)
two_year.lm <- lm(two_year ~ three_month + slope + curve, data = full_ir)
three_year.lm <- lm(three_year ~ three_month + slope + curve, data = full_ir)
five_year.lm <- lm(five_year ~ three_month + slope + curve, data = full_ir)
seven_year.lm <- lm(seven_year ~ three_month + slope + curve, data = full_ir)
twenty_year.lm <- lm(twenty_year ~ three_month + slope + curve, data = full_ir)

# summary of all models
models <- list(one_year.lm, two_year.lm, three_year.lm, five_year.lm, seven_year.lm, twenty_year.lm)
model_summaries <- lapply(models, summary)

model_summaries

saveRDS(models, "models/yield_curve_lm_mods.rds")

# extract coefficients from all models
coef_matrix <- sapply(models, function(model) coef(model))

# In sample prediction

plot_insample_curve <- function(models) {
  # randomly sample 1 row from var_sim
  
  sample_row <- sample(1:nrow(full_ir), 1)
  sample_slope <- full_ir$slope[sample_row]
  sample_curve <- full_ir$curve[sample_row]
  sample_level <- full_ir$three_month[sample_row]
  
  # predict on all models
  new_df <- data.frame(three_month = sample_level, slope = sample_slope, curve = sample_curve)
  one_year_pred <- predict(models[[1]], newdata = new_df)
  two_year_pred <- predict(models[[2]], newdata = new_df)
  three_year_pred <- predict(models[[3]], newdata = new_df)
  five_year_pred <- predict(models[[4]], newdata = new_df)
  seven_year_pred <- predict(models[[5]], newdata = new_df)
  twenty_year_pred <- predict(models[[6]], newdata = new_df)
  thirty_year_pred <- sample_level + sample_slope
  ten_year_pred <- sample_level + (sample_slope - sample_curve)/2
  
  # put in df with 1 column as time, and other column as rate
  pred_df <- data.frame(
    time = c(3/12, 1, 2, 3, 5, 7, 10, 20, 30),
    rate = c(sample_level, one_year_pred, two_year_pred, three_year_pred,
             five_year_pred, seven_year_pred, ten_year_pred, twenty_year_pred, thirty_year_pred)
  )
  
  actual_df <- full_ir[sample_row,] |> 
    pivot_longer(cols = three_month:thirty_year) |> 
    dplyr::select(rate = value, date) |> 
    mutate(time = c(3/12, 1, 2, 3, 5, 7, 10, 20, 30))
  
  #plot(yield_curve_df$time, yield_curve_df$rate)
  ggplot() +
    geom_point(data = pred_df, aes(x = time, y = rate), color = "blue") +
    geom_point(data = actual_df, aes(x = time, y = rate), color = "red") +
    
    labs(subtitle = paste0("Date: ", actual_df$date[1]))
  
}

plot_insample_curve(models)


# plot a "yield curve"

plot_yield_curve <- function(var_sim, models) {
  # randomly sample 1 row from var_sim
  sample_row <- sample(1:nrow(var_sim), 1)
  sample_slope <- var_sim[sample_row, 1]
  sample_curve <- var_sim[sample_row, 2]
  
  # get random 3mo "level"
  sample_level <- ir3mo_sim[sample_row]
  
  # predict on all models
  new_df <- data.frame(three_month = sample_level, slope = sample_slope, curve = sample_curve)
  one_year_pred <- predict(models[[1]], newdata = new_df)
  two_year_pred <- predict(models[[2]], newdata = new_df)
  three_year_pred <- predict(models[[3]], newdata = new_df)
  five_year_pred <- predict(models[[4]], newdata = new_df)
  seven_year_pred <- predict(models[[5]], newdata = new_df)
  twenty_year_pred <- predict(models[[6]], newdata = new_df)
  thirty_year_pred <- sample_level + sample_slope
  ten_year_pred <- sample_level + (sample_slope - sample_curve)/2
  
  # put in df with 1 column as time, and other column as rate
  yield_curve_df <- data.frame(
    time = c(3/12, 1, 2, 3, 5, 7, 10, 20, 30),
    rate = c(sample_level, one_year_pred, two_year_pred, three_year_pred,
             five_year_pred, seven_year_pred, ten_year_pred, twenty_year_pred, thirty_year_pred)
  )
  
  # back_transform
  yield_curve_df <- yield_curve_df %>%
    mutate(rate = ifelse(rate > r_bar, rate,
                         r_bar - r_bar * log(r_bar) + r_bar * log(rate)))
  
  #plot(yield_curve_df$time, yield_curve_df$rate)
  ggplot(data = yield_curve_df) +
    geom_point(aes(x = time, y = rate)) +
    #geom_smooth(aes(x = time, y = rate), se = FALSE) +
    labs(subtitle = paste0("Slope: ", sample_slope, ". Curve: ", sample_curve))
  
}

plot_yield_curve(var_sim, models)

# Gut check models

plot(var_sim[,1], var_sim[,2], xlab = "Slope", ylab = "Curvature")


# How has the yield curve historically changed? ---------------------------


library(gganimate)

yield_long <- full_ir %>%
  dplyr::select(date, three_month, one_year, two_year, three_year, five_year, seven_year, ten_year, twenty_year, thirty_year) %>%
  pivot_longer(
    cols = -date,
    names_to = "maturity",
    values_to = "yield"
  ) %>%
  mutate(
    maturity_years = case_when(
      maturity == "three_month" ~ 0.25,
      maturity == "one_year" ~ 1,
      maturity == "two_year" ~ 2,
      maturity == "three_year" ~ 3,
      maturity == "five_year" ~ 5,
      maturity == "seven_year" ~ 7,
      maturity == "ten_year" ~ 10,
      maturity == "twenty_year" ~ 20,
      maturity == "thirty_year" ~ 30
    )
  )

p <- ggplot(yield_long, aes(x = maturity_years, y = yield)) +
  #geom_smooth(color = "blue", se = FALSE) +
  geom_point() +
  scale_x_continuous(breaks = c(0.25, 1, 2, 3, 5, 7, 10, 20, 30)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = 'US Yield Curve on: {current_frame}',
    x = 'Time',
    y = 'Yield',
  ) +
  theme_minimal(base_size = 14) +
  transition_manual(
    date,
  ) +
  ease_aes('linear')

animate(p, width = 800, height = 500,nframes = length(unique(yield_long$date)), renderer = ffmpeg_renderer("yield_curve.mp4"))


