library(tidyverse)

mortality_tbl_raw <- tibble(
  age = 50:120,
  q_x = c(
    0.00488, 0.00513, 0.00541, 0.00571, 0.00604, 0.00640, 0.00681, 0.00724, 0.00767, 0.00808,
    0.00845, 0.00879, 0.00914, 0.00956, 0.01010, 0.01083, 0.01174, 0.01284, 0.01413, 0.01559,
    0.01724, 0.01909, 0.02116, 0.02347, 0.02606, 0.02897, 0.03225, 0.03595, 0.04017, 0.04495,
    0.05035, 0.05646, 0.06332, 0.07102, 0.07966, 0.08935, 0.10021, 0.11239, 0.12592, 0.14079,
    0.15694, 0.17391, 0.19142, 0.20927, 0.22735, 0.24563, 0.26410, 0.28278, 0.30168, 0.32076,
    0.33996, 0.35910, 0.37794, 0.39633, 0.41415, 0.43131, 0.44771, 0.46329, 0.47800, 0.49181,
    0.50000, 0.50000, 0.50000, 0.50000, 0.50000, 0.50000, 0.50000, 0.50000, 0.50000, 0.50000, 1
  )
)

mortality_tbl <-  mortality_tbl_raw |> 
  mutate(
    # q_x is P(D_t|S_t-1)
    p_x = 1 - q_x,                              # P(S_t|S_t-1) = 1-q_x
    S = cumprod(p_x),                           # P(S_t) = P(S_t|S_t-1)*P(S_t-1)
    death_pdf = (lag(S) * q_x)/12,  # P(D_t) = P(D_t|S_t-1)*P(S_t-1)
    death_pdf = replace(death_pdf, 1, mortality_tbl_raw$q_x[1]/12)
  ) |> 
  uncount(weights = 12) |> 
  mutate(
    age_years = rep(mortality_tbl_raw$age, each = 12),  # Repeat each age 12 times
    month_within_year = rep(0:11, times = nrow(mortality_tbl_raw)), # 0-11 for each year
    month = age_years * 12 + month_within_year,  # Total months from birth
    death_cdf = cumsum(death_pdf),
    S_monthly = 1-death_cdf
  ) |> 
  select(month, death_pdf, S = S_monthly)

sum(mortality_tbl$death_pdf, na.rm = TRUE)

write_csv(mortality_tbl, "data/mortality.csv")

