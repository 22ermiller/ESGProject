library(tidyverse)

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

mean_3mo_ir <- get_average_3mo_rate(ir3mo_df)
ir3mo_sim <- ir3mo_single_sim(ir3mo_mod, n_years*12, cpi_sim, mean_3mo_ir)
ir3mo_sims <- ir3mo_multiple_sims(ir3mo_mod, n_years*12, n_sims, cpi_sims, mean_3mo_ir)

slope_curve_vals <- get_final_slope_curve_vals(full_ir_df)
yield_curve_sim <- yield_single_sim(yield_var_mod, yield_lms, n_years*12, ir3mo_sim, slope_curve_vals)
yield_curve_sims <- yield_multiple_sims(yield_var_mod, yield_lms, n_years*12, n_sims, ir3mo_sims, slope_curve_vals)


# Annuity Pricing Functions --------------------------------------------------------

# Extrapolate interest rates for each month

get_yield_df <- function(yield_curve) {
  yield_curve_df <- yield_curve %>%
    pivot_longer(three_month:thirty_year, names_to = "time", values_to = "rate") %>%
    mutate(time = case_when(time == "three_month" ~ 3/12,
                            time == "one_year" ~ 1,
                            time == "two_year" ~ 2,
                            time == "three_year" ~ 3,
                            time == "five_year" ~ 5,
                            time == "seven_year" ~ 7,
                            time == "ten_year" ~ 10,
                            time == "twenty_year" ~ 20,
                            time == "thirty_year" ~ 30
    ))
  return(yield_curve_df)
}


interpolate_rate <- function(curve_df, target_month) {
  
  target_time <- target_month/12
  
  # Ensure sorted by maturity
  curve_df <- curve_df %>% arrange(time)
  
  # If exactly matches a maturity, return directly
  if (target_time %in% curve_df$time) {
    return(curve_df$rate[curve_df$time == target_time])
  } else if (target_time < min(curve_df$time)) { # if target time is below minimum time, return minimum time rate
    return(curve_df$rate[curve_df$time == min(curve_df$time)])
  } else if (target_time > max(curve_df$time)) { # if target time is above maximum time, return maximum time rate
    return(curve_df$rate[curve_df$time == max(curve_df$time)])
  }
  
  lower <- max(curve_df$time[curve_df$time < target_time])
  upper <- min(curve_df$time[curve_df$time > target_time])
  
  rate_lower <- curve_df$rate[curve_df$time == lower]
  rate_upper <- curve_df$rate[curve_df$time == upper]
  
  slope <- (rate_upper - rate_lower) / (upper - lower)
  interpolated <- rate_lower + slope * (target_time - lower)
  
  return(interpolated)
}

# create a sequence of months from 1-40 years
months_seq <- seq(1, n_years * 12)  # Convert months to years


# put month sequence in rate interpolation function
interpolated_rates <- map_dbl(months_seq, ~interpolate_rate(yield_curve_df, .x))
plot(interpolated_rates)

pricing_df <- data.frame(month = months_seq, rate = interpolated_rates) %>%
  mutate(discount = (1 + rate)^(-month / 12),
         survival_prob = mortality_tbl$S[1:(n_years*12)],
         epv = discount*survival_prob)

# calculate price
total_epv <- sum(pricing_df$epv)
load <- .1
price <- total_epv*(1+load)


# Function to price annuities with variable length, payouts, and start times


price_annuity <- function(start_age, yield_curve, mortality_tbl, payout, load) {
  
  # get yield_curve_df
  yield_curve_df <- get_yield_df(yield_curve[1,])
  
  # set mortality for starting age
  if(start_age*12 >= min(mortality_tbl$month)-1) {
    mortality_tbl_clipped <- mortality_tbl %>%
      filter(month >= start_age*12)
  } else {
    stop("start_age must be at least 50")
  }
  
  # define length (from mortality table probabilities)
  length <- nrow(mortality_tbl_clipped)
  
  # create a sequence of months from 1-length
  months_seq <- seq(1, length)
  
  # put month sequence in rate interpolation function
  interpolated_rates <- map_dbl(months_seq, ~interpolate_rate(yield_curve_df, .x))
  

  
  pricing_df <- data.frame(month = months_seq, rate = interpolated_rates) %>%
    mutate(discount = (1 + rate)^(-month / 12),
           survival_prob = mortality_tbl_clipped$S,
           epv = discount*survival_prob)
  
  # calculate price
  total_epv <- sum(pricing_df$epv)*payout
  load <- .1
  price <- total_epv*(1+load)
  
  return(price)
}

price_annuities <- function(start_age, yield_curves, mortality_tbl, payout, load, n_sims) {
  prices <- vector("numeric", n_sims)
  
  for (i in 1:n_sims) {
    prices[i] <- price_annuity(start_age, yield_curves[[i]], mortality_tbl, payout, load)
  }
  
  return(prices)
}

price_annuity(start_age = 80, yield_curve_sim, mortality_tbl, 1, .1)

prices <- price_annuities(start_age = 65, yield_curve_sims, mortality_tbl, 1, .1, 50)







