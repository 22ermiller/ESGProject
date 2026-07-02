library(tidyverse)
library(janitor)
library(readxl)

# Employment Cost Index --------------------------------------------------

# link to data: https://www.bls.gov/eci/tables.htm

# read in  data
eci_raw <- read_xlsx(
  "raw_data/eci-continuous-dataset.xlsx",
  sheet = "Continuous dataset"
) |>
  clean_names()


eci_final <- eci_raw |>
  filter(
    periodicity == "Current dollar index number" &
      industry == "All industries" &
      occupation == "All occupations" &
      estimate_type == "Total compensation" &
      ownership == "Civilian workers" # all workers
  ) |>
  mutate(
    date = as.Date(paste(year, period, "1", sep = "-"), format = "%Y-%B-%d")
  ) |>
  select(date, eci = estimate)

write_csv(eci_final, "data/eci.csv")


# Medical Inflation ------------------------------------------------------

# link to data: https://data.bls.gov/timeseries/CUUR0000SAM?output_view=data
# for correctly formatted select earliest date range (1935), click "More Formatting Options", and then click "Column Format" then "Retreive Data"

med_inflation_raw <- read_csv("raw_data/medical_inflation_raw.csv") |>
  clean_names()

med_inflation_final <- med_inflation_raw |>
  filter(str_starts(period, "M")) %>% # keep monthly rows only
  mutate(
    month = as.integer(str_remove(period, "M")),
    date = make_date(year, month, 1) # first of the month
  ) |>
  select(date, med_inflation = value)

write_csv(med_inflation_final, "data/med_inflation.csv")


# CPI Less Medical -------------------------------------------------------

# link to data: https://fred.stlouisfed.org/series/CUSR0000SA0L5

cpi_raw <- read_csv("raw_data/cpi_less_medical_raw.csv") |>
  clean_names()

cpi_final <- cpi_raw |> rename(date = observation_date, cpi = cusr0000sa0l5)

write_csv(cpi_final, "data/cpi.csv")
