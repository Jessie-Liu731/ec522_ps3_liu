# part 2 - getting comfortable with time-series data in r

#2.2 read the .csv file and inspect the data column
library(tidyverse)
library(fpp3)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

##2.2b specify column types explicitly rather than trusting the guess made by R
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

#2.3 reshape data to long format
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

#2.4 build your tsibble (data frame built for time series)

#2.4a build tsibble
retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

#2.4b inspect tsibble
head(retail_ts)

has_gaps(retail_ts)

#2.5 Canadian Gas Production

#2.5a see key and index variables in the canadian_gas tsibble
key_vars(canadian_gas)
index_var(canadian_gas)

#2.5b make a working copy of the data and inspect the new tsibble
can_gas <- canadian_gas |> select(Month, Volume)

has_gaps(can_gas)

#2.6 working with dates in a tsibble

#2.6a filtering by date
can_gas_recent <- can_gas |> 
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

#2.6b extracting parts of a data
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))
###December has the highest average production while June has the lowest average production

#2.6c date arithmetic
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

#2.6d changing the frequency of a series
#convert can_gas from monthly to quarterly data
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)
### since we need the quarterly volume and that equals to the sum of the monthly volume in each quarter.
### the quarterly series appears to be not seasonal now

#2.7 plotting with autoplot()

#2.7a plot U.S. retail sales
retail_ts |> autoplot(sales)

#2.7b layer ordinary ggplot2 code on top of the autoplot() call
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# The seasonal adjustment provides a line smoother than the lien for the NSA series. Although the upward trend is similar, the swings become much more smaller after adjusted.
# The NSA series has got an annual pattern where there's a spike and drop towards the end of each year. The SA series does not have this pattern.
# The two series still follow the same path over the long run yet the ups and downs become much smaller after seasonal adjustment.

#2.7c lplot the Canadian gas production series can_gas
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Monthly Natural Gas Production in Canada",
    x = NULL, y = "Billions of Cubic Meters", color = NULL
  ) +
  theme_minimal()

#2.7d choice of ggplot2 layer
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal() +
  facet_wrap (~series_id)
### the use of facet_wrap makes the comparison more visible without the line NSA series covering up the SA ones. 



