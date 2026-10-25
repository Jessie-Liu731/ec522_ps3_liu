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

#2.7c plot the Canadian gas production series can_gas
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


# part 3 - data transformations (logs and growth rates)

#3.1 log transformation
### The size of seasonal swings in can_gas change over time. The swings get larger as the series grow till 1990 and then the swings get smaller as the series shrink after 2000. 

#3.1b 
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))
can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Logged Monthly Natural Gas Production in Canada",
    x = NULL, y = "Billions of Cubic Meters (Logged)", color = NULL
  ) +
  theme_minimal()

###The trend totally changes in the logged series. It grows rapidly at the beginning and then it slows down and becomes more stable between 1975 and 1985 and after 2000. 
###The sizes of seasonal swings also change in different ways over time in the logged series. The swings are more stable at the first and last third of the series with the size in the first third is much more bigger. The size of swings in the middle third of the series becomes generally the largest over time.  
###I think the logged series is more useful for forecasting because it reflects some genuine changes and fluctuations in the data over time, in reference to the original volume at the time. This may make it easier as well as more informative to identify and model the underlying patterns.

#3.1c
retail_ts_nsa <- retail_ts |>
  filter(series_id == "RSAFS") |>
  mutate(log_sales = log(sales)) 

retail_ts_nsa |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

retail_ts_nsa |>
  autoplot(log_sales) +
  labs(
    title = "Logged U.S. Retail and Food Services Sales",
    x = NULL, y = "Millions of dollars (Logged)", color = NULL
  ) +
  theme_minimal()

###I think the case for logging this series is stronger than it was for can_gas. As the original level trend up and the logged trend becomes more linear in the logged series, it becomes easier to model and forecast. The size of seasonal swings also becomes more stable over time in the logged series. 

#3.2 growth rates
#3.2a compute growth rates for can_gas in two different ways
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Month-over-Month Percentage Growth Rate of Natural Gas Production in Canada",
    x = NULL, y = "Percentage", color = NULL
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))
can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Absolute Difference Between the Two Growth Rate Calculations",
    x = NULL, y = "Percentage Points", color = NULL
  ) +
  theme_minimal()

###The log difference approximation is doing a pretty good job of capturing the growth rate of the series. The absolute difference between the two methods is relatively small considering the original scale of growth rates, indicating that the log difference is a good approximation of the actual growth rate.

#3.3 seasonally adjusted vs. non seasonally adjusted growth rates

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

###The NSA series is noisier than the SA series. The SA series is much smoother and more stable over time, while the NSA series has more fluctuations and spikes with the size of swings larger as well. 
###This is because the SA series has been adjusted to remove the seasonal effects, while the NSA series still contains these effects. The SA series is more useful for analyzing the underlying trend and growth of retail sales, while the NSA series may be more useful for understanding the seasonal patterns and fluctuations in retail sales.





