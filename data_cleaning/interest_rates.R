library(tidyverse)


# Format CPI so we know what dates to filter interest rates to ------------

cpi_df_raw <- read_csv("data/cpi.csv") 

cpi_df <- cpi_df_raw |> 
  filter(date >= "1990-01-01") |> # only dates from 2010 onwards
  mutate(lagged_cpi = lag(cpi, n = 1)) |> 
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |> 
  filter(!is.na(log_dif_cpi))


# Read in Interest Rates --------------------------------------------------

ir3mo_raw <- read_csv("raw_data/DGS3MO (1).csv")

ir10yr_raw <- read_csv("raw_data/DGS10.csv")

ir30yr_raw <- read_csv("raw_data/DGS30.csv")

ir20yr_raw <- read_csv("raw_data/DGS20.csv")

ir1yr_raw <- read_csv("raw_data/DGS1.csv")

ir2yr_raw <- read_csv("raw_data/DGS2(1).csv")

ir3yr_raw <- read_csv("raw_data/DGS3.csv")

ir5yr_raw <- read_csv("raw_data/DGS5.csv")

ir7yr_raw <- read_csv("raw_data/DGS7.csv")

# Format Data -------------------------------------------------------------

ir3mo <- ir3mo_raw |> 
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
  dplyr::select(date, rate)

ir10yr <- ir10yr_raw |> 
  rename(date = observation_date,
         rate = DGS10) |> 
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
  dplyr::select(date, rate)

ir30yr <- ir30yr_raw |> 
  rename(date = observation_date,
         rate = DGS30) |> 
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
  dplyr::select(date, rate)

ir1yr <- ir1yr_raw |> 
  rename(date = observation_date,
         rate = DGS1) |> 
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
  dplyr::select(date, rate)

ir2yr <- ir2yr_raw |>
  rename(date = observation_date,
         rate = DGS2) |> 
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
  dplyr::select(date, rate)

ir3yr <- ir3yr_raw |>
  rename(date = observation_date,
         rate = DGS3) |> 
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
  dplyr::select(date, rate)

ir5yr <- ir5yr_raw |>
  rename(date = observation_date,
         rate = DGS5) |> 
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
  dplyr::select(date, rate)

ir7yr <- ir7yr_raw |>
  rename(date = observation_date,
         rate = DGS7) |> 
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
  dplyr::select(date, rate)

ir20yr <- ir20yr_raw |>
  rename(date = observation_date,
         rate = DGS20) |> 
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
  dplyr::select(date, rate)

full_ir <- ir3mo |>
  rename(three_month = rate) |> 
  left_join(ir1yr |> rename(one_year = rate), by = "date") |>
  left_join(ir2yr |> rename(two_year = rate), by = "date") |>
  left_join(ir3yr |> rename(three_year = rate), by = "date") |>
  left_join(ir5yr |> rename(five_year = rate), by = "date") |>
  left_join(ir7yr |> rename(seven_year = rate), by = "date") |>
  left_join(ir10yr |> rename(ten_year = rate), by = "date") |> 
  left_join(ir20yr |> rename(twenty_year = rate), by = "date") |>
  left_join(ir30yr |> rename(thirty_year = rate), by = "date")



# Save Data ---------------------------------------------------------------

write_csv(ir3mo, "data/ir3mo.csv")
write_csv(ir10yr, "data/ir10yr.csv")
write_csv(ir30yr, "data/ir30yr.csv")
write_csv(ir1yr, "data/ir1yr.csv")
write_csv(ir2yr, "data/ir2yr.csv")
write_csv(ir3yr, "data/ir3yr.csv")
write_csv(ir5yr, "data/ir5yr.csv")
write_csv(ir7yr, "data/ir7yr.csv")
write_csv(ir20yr, "data/ir20yr.csv")
write_csv(full_ir, "data/full_ir.csv")

