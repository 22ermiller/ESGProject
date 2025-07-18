library(tidyverse)
library(janitor)

# Employment Cost Index --------------------------------------------------

# read in  data
eci_raw <- read_csv("raw_data/eci-continuous-dataset.csv") |>
  clean_names()


eci_final <- eci_raw |>
  filter(
    periodicity == "Current dollar index number" &
      industry == "All industries" &
      occupation == "All occupations" &
      estimate_type == "Total compensation" &
      ownership == "Civilian workers" # all workers
  ) |>
  mutate(date = as.Date(paste(year, period, "1", sep = "-"), format = "%Y-%B-%d")) |>
  select(date, eci = estimate)

write_csv(eci_final, "data/eci.csv")


# Medical Inflation ------------------------------------------------------

med_inflation_raw <- read_csv("raw_data/medical_inflation_raw.csv") |> 
  clean_names()

med_inflation_final <- med_inflation_raw |> 
  mutate(date = lubridate::ym(label)) |> 
  select(date, med_inflation = value)

write_csv(med_inflation_final, "data/med_inflation.csv")


# CPI Less Medical -------------------------------------------------------

# link to data: https://fred.stlouisfed.org/series/CUSR0000SA0L5

cpi_raw <- read_csv("raw_data/cpi_less_medical_raw.csv") |> 
  clean_names()

cpi_final <- cpi_raw |> rename(cpi = cusr0000sa0l5)

write_csv(cpi_final, "data/cpi.csv")
