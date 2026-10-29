# part 2 - getting comfortable with time-series data in r

#2.2 read the .csv file and inspect the data column
library(tidyverse)
library(fpp3)
library(forecast)

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

# The seasonal adjustment provides a line smoother than the line for the NSA series. Although the upward trend is similar, the swings become much more smaller after adjusted.
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
###I think the logged series is less useful here for this series because the size of the fluctuations does not really grow wit hthe level of the series but changes instead. That means that the series grows at changing percentage rate, making it more difficult to forecast in the logged terms. 

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
###This is because the SA series has been adjusted to remove the seasonal effects in the series, while the NSA series still contains these effects. The SA series is more useful for analyzing the underlying trend and growth of retail sales, while the NSA series may be more useful for understanding the seasonal patterns and fluctuations in retail sales.

#part 4 - seeing, decomposing, and modeling seasonality

#4.1 seasonal plots
#4.1a 
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

#4.1b 
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

#Compared to the NSA series, the SA series has a much more stable seasonal pattern over time. Compared to the lines tracing a siilar shape year after year in the NSA plot, the lines of different years get to trace different shapes in the SA series. 

#4.1c seasonal plot for can_gas
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Logged Monthly Natural Gas Production in Canada",
    y = "Billions of Cubic Meters (Logged)"
  ) +
  theme_minimal()

#The seasonal pattern shows that the production is highest in the winter months and lowest in the summer months. The seasonal pattern is more obvious in the earlier years of the series, with a larger difference between the winter and summer production levels. In the later years of the series, the seasonal pattern becomes less pronounced, with a smaller difference between the winter and summer production levels.
#The pattern matches my intuition about natural gas production in Canada as natural gas is usually used for generating electricity for both heating and cooling the buildings, with heating takes more than cooling. 

#4.2 a subseries plot
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

#4.3 seasonally adjusting with X-13ARIMA-SEATS

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

#December seems to have the highest seasonal factor at roughly 10%-20% above trend. 
#The size of the seasonal swings does appear to change over time. It is getting smaller.

#4.3b compare two SA series to see the percent difference between them
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

#My adjustment is very similar to the Census Bureau's adjustment, with the difference being mostly within 1%. Yet noticably, the difference is getting bigger over time. The two adjustment differ the most for 2020-2024 data at each beginning and end of the year.

#4.4 a simple model - linear-trend + seasonal dummies

#4.4a
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

#The estimated average monthly growth rate of gas production is 0.4%.
#June has the most significant seasonal effect. 

#4.4b
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

#The fitted line does well in around the year of 1965-1979 and 1989-2019. It visibly misses the other years.

#4.4c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

#The residuals do not look like white noise - the fitted values tend to be systematically larger in the middle of the series and smaller at the beginning and end of the series. This might indicate that things happening around different times could be incorporated into our model for a better forecasting performance.
#The Ljung-Box test, with a p-value of 0.000 that is well below 0.05, indicates that we can reject the null hypothesis that the residuals are white noise at the 5% level.


#part 5 - a first forecast evaluation

#5.1 an expanding-window TS-CV
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

#the date of the first forecast is January 1980 and the date of the last forecast is February 2005.
#For the seasonal random walk model, the MAE is 0.57 and the RMSE is 0.7. For the linear trend + seasonal dummies model, the MAE is 4.01 and the RMSE is 4.36.
#For forecast users with both absolute and quadratic loss functions, the seasonal random walk model forecasts more accurately. The two metrics agree with each other this time.

#5.2 is the difference significant?
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

#The DM statistic is -31.278, of which its absolute value is much larger than the critical value of 1.96 at the 5% significance level. The p-value is 0.000, which is less than 0.05. 
#Therefore, we can reject the null hypothesis that the two models have equal predictive accuracy at the 5% significance level. This indicates that the seasonal random walk model has significantly higher forecasting accuracy than the linear trend + seasonal dummies model for this dataset.

#5.3 a rolling window
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

#The accuracy of linear trend + seasonal dummies model change more when switching to a rolling window.
#since the trend and seasonal dummy parameters are estimated from the training sample, the expanding window evaluation will include the entire history of the series. Yet as we can see in the fitted values in Part 4.4b, a constant linear trend will not be able to capture the changing growth rate of the series. Also the size of the seasonal effects seem to change over time. 
#Therefore, the assumption of a stable data generating process seems to be violated here, so is the premise of using an expanding window to do TS-CV. In this scenario, a rolling window approach might be a better choice.





