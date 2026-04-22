## This Script shows how to use functions to get portfolio simulation

source("ESG_draws.R")
source("portfolio_sim_functions.R")
source("functions.R") # to use annuity pricing functions

# for annuity pricing
mortality_tbl <- read_csv("data/mortality.csv") |> filter(!is.na(death_pdf))

n_years <- 50
n_sims <- 500

esg_sims <- ESG_draws(n_years, n_sims)

cpi_sims <- esg_sims$cpi
med_sims <- esg_sims$med_cpi
yield_curve_sims <- esg_sims$yield_curve
equity_sims <- esg_sims$equity_returns

# how many months into future is annuity purchased
purchase_date <- 1
# how many dollars per month does annuity payout
payout <- 1
# loading factor for annuity
loading <- .1

annuity_prices <- price_annuities(60, yield_curve_sims, mortality_tbl, purchase_date, payout, loading, n_sims)

annuity_prop <- .4
wr <- .04

# sample death ages
death_ages <- get_death_ages(60, mortality_tbl, n_sims)

sim_results <- portfolio_sim(annuity_prop, wr, annuity_prices, equity_sims, cpi_sims, med_sims, death_ages)
