## All simulation functions for the ESG ##


# CPI ---------------------------------------------------------------------

cpi_single_sim <- function(model, sim_length) {
  simulate(model, nsim = sim_length)
}

cpi_multiple_sims <- function(model, sim_length, n_sims) {
  sims <- vector("list", n_sims)
  
  for (i in 1:n_sims) {
    sims[[i]] <- cpi_single_sim(model, sim_length)
  }
  
  return(sims)
}

# ECI ---------------------------------------------------------------------

# get last quarterly cpi value helper function

get_last_quarterly_cpi_val <- function(cpi_df) {
  quarterly_cpi <- cpi_df |> 
    filter(month(date) %in% c(3, 6, 9, 12)) |>
    mutate(lagged_cpi = lag(cpi, n = 1)) |> 
    mutate(log_dif_cpi = log(cpi) - log(lagged_cpi),
           lagged_cpi = lag(log_dif_cpi, n = 1)) |> 
    filter(!is.na(log_dif_cpi) & !is.na(lagged_cpi)) # remove first observation without lag
  
  last_val <- tail(quarterly_cpi$log_dif_cpi, n = 1)
  return(last_val)
}

eci_single_sim <- function(model, sim_length, cpi_sim, final_cpi_value) {
  
  # make cpi simulation quarterly
  quarterly_cpi_sim_df <- data.frame(
    date = zoo::as.Date(time(cpi_sim)),
    value = cpi_sim) |> 
    mutate(year = year(date),
           quarter = quarter(date)) |> 
    group_by(year, quarter) |> 
    summarize(date = max(date),
              log_dif_cpi = sum(value))
  
  lagged_cpi_sim <- c(final_cpi_value, head(quarterly_cpi_sim_df$log_dif_cpi, n = -1))
  sim <- simulate(model, nsim = sim_length, xreg = lagged_cpi_sim)
  
  return(sim)
}

eci_multiple_sims <- function(model, sim_length, nsims, cpi_sims, final_cpi_value) {
  sims <- vector("list", n_sims)
  
  for(i in 1:n_sims){
    sims[[i]] <- eci_single_sim(model, sim_length, cpi_sims[[i]], final_cpi_value)
  }
  
  return(sims)
}

# Medical Inflation -------------------------------------------------------

med_single_sim <- function(model, sim_length) {
  simulate(model, nsim = sim_length)
}

med_multiple_sims <- function(model, sim_length, n_sims) {
  sims <- vector("list", n_sims)
  
  for (i in 1:n_sims) {
    sims[[i]] <- med_single_sim(model, sim_length)
  }
  
  return(sims)
}


# Short Term Interest Rates -----------------------------------------------

get_average_3mo_rate <- function(interest_df) {
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
  
  return(mean(model_df$transformed_rate))
}

back_transform <- function(x) {
  r_bar <- .005
  if(x > r_bar){
    return(x)
  } else {
    return(exp((x - r_bar + r_bar*log(r_bar))/r_bar))
  }
}

ir3mo_single_sim <- function(model, sim_length, cpi_sim, mean_rate) {
  
  # cpi simulation
  cpi <- matrix(cpi_sim, ncol = 1)
  mexsim_list <- list(cpi)
  
  sim <- ugarchsim(model, n.sim = sim_length, startMethod = "sample", mexsimdata = mexsim_list)@simulation$seriesSim +
    mean_rate
  
  final_sim <- apply(as.matrix(sim),
                     1, back_transform)
  
  return(final_sim)
}

ir3mo_multiple_sims <- function(model, sim_length, nsims, cpi_sims, mean_rate) {
  sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
  
  for (i in 1:n_sims) {
    
    sims[i,] <- ir3mo_single_sim(model, sim_length, cpi_sims[[i]], mean_rate)
  }
  
  return(sims)
}


# Yield Curve -------------------------------------------------------------

get_final_slope_curve_vals <- function(ir_df) {
  list(slope = tail(ir_df$slope, 1),
       curve = tail(ir_df$curve, 1))
}

yield_single_sim <- function(var_mod, lm_mods, sim_length, ir3mo_sim, final_vals) {
  
  sample_cov <- cov(var_mod$residuals)
  var_sim <- VAR.sim(var_mod$coefficients,
                     n = sim_length, 
                     starting = matrix(c(final_vals$slope, final_vals$curve), nrow = 1),
                     varcov = sample_cov)
  three_month <- ir3mo_sim
  slope <- var_sim[,1]
  curve <- var_sim[,2]
  
  new_df <- data.frame(three_month = three_month,
                       slope = slope,
                       curve = curve)
  
  preds <- sapply(lm_mods, function(model) predict(model, newdata = new_df))
  
  # Add 10yr and 30yr yield (derived from slope and curve)
  ten_year <- three_month + (slope - curve)/2
  thirty_year <- three_month + slope
  
  # Create full yield curve paths (time series format)
  yield_curve <- data.frame(
    three_month = three_month,
    one_year = preds[, 1],
    two_year = preds[, 2],
    three_year = preds[, 3],
    five_year = preds[, 4],
    seven_year = preds[, 5],
    ten_year = ten_year,
    twenty_year = preds[, 6],
    thirty_year = thirty_year
  )
  
  return(yield_curve)
}


# Equity Returns -----------------------------------------------------------

# Must refit MSGARCH model, fitted model can't be saved out of memory due to stored C++ pointers that can't be loaded across R sessions
fit_equity_rs_model <- function(data) {
  spec <- CreateSpec(variance.spec = list(model = "sGARCH"),
                     distribution.spec = list("norm"),
                     switch.spec = list(K = 2))
  MSGARCH::FitML(spec, data = data)
}

equity_single_sim <- function(mean_mod, rs_mod, sim_length, cpi_sim, ir3mo_sim) {
  
  X <- data.frame(log_dif_cpi = cpi_sim,
                  rate = ir3mo_sim)
  mean_vals <- predict(mean_mod, X)
  rs_sim <- simulate(rs_mod, nsim = sim_length)
  sim_df <- data.frame(resid_sim = rs_sim$draw[1,],
                       state = rs_sim$state[1,]) 
  
  final_sim <- sim_df$resid_sim + mean_vals
  return(final_sim)
}

equity_multiple_sims <- function(mean_mod, rs_mod, sim_length, nsims, cpi_sims, ir3mo_sims) {
  sims <- matrix(NA, nrow = n_sims, ncol = sim_length)
  
  for (i in 1:n_sims) {
    
    sims[i,] <- equity_single_sim(mean_mod, rs_mod, sim_length, cpi_sims[[i]], ir3mo_sims[i,])
  }
  
  return(sims)
}

