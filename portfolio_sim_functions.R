# Functions to simulate cash-flows in a portfolio using ESG draws and annuity prices


portfolio_sim <- function(annuity_prop, withdrawal_rate, annuity_prices, equity_sims, cpi_sims, med_sims, death_ages) {
  
  n_sims <- nrow(equity_sims)
  T <- ncol(equity_sims)
  
  # Convert log returns to gross returns
  equity_gross <- exp(equity_sims)
  
  # Price level from CPI simulations
  price_level <- t(apply(cpi_sims, 1, function(x) cumprod(exp(x))))
  med_price_level <- t(apply(med_sims, 1, function(x) cumprod(exp(x))))
  
  
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
