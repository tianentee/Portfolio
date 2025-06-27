# --------------- Time Series Analysis ---------------
# Recommendation of an Investment of a Stock by Analyzing Historical Data 
# Load library  
library(fpp3)

# --------------- Read the data into R --------------- 
# Save it as tsibble object 
NGVC_data <- read.csv("NGVC.csv") 

# Data from 1 March 2023 to 30 September 2023
NGVC <- NGVC_data |>
  filter(Date >= "2023-03-01" & Date <= "2023-09-30") |>
  mutate(Day = row_number()) |> 
  mutate(Date = as_date(Date)) |>
  as_tsibble(index = Day) 

# Data from 1 January 2018 to 30 September 2023
NGVC_historical <- NGVC_data |>
  filter(Date >="2018-01-01" & Date <= "2023-09-30") |>
  mutate(Day = row_number()) |> 
  mutate(Date = as_date(Date)) |>
  as_tsibble(index = Day) 

# Data from 1 March 2023 to 19 October 2023
NGVC_oct <- NGVC_data |>
  filter(Date >= "2023-03-01" & Date <= "2023-10-19") |>
  mutate(Day = row_number()) |> 
  mutate(Date = as_date(Date)) |>
  as_tsibble(index = Day) 

# Check the data
NGVC
NGVC_historical
NGVC_oct

# --------------- Analysis ---------------
# Produce time series plots over the selected seven-month time-frame
NGVC |>
  autoplot(Close) +
  labs(title = "Natural Grocers by Vitamin Cottage, Inc.",
       subtitle = "1 March 2023 to 30 September 2023",
       y = "Close (USD)")

# Plot of the historical data
NGVC_historical |>
  update_tsibble(index = Date, regular = FALSE) |>
  autoplot(Close) + 
  labs(title = "Natural Grocers by Vitamin Cottage, Inc.",
       subtitle = "1 January 2018 to 30 September 2023",
       y = "Close (USD)")

# Detect patterns of seasonality
NGVC_historical |> 
  update_tsibble(index = Date, regular = FALSE) |>
  gg_season(Close, labels = "both") + 
  labs(title = "Seasonal Plot: Natural Grocers by Vitamin Cottage, Inc.", 
       subtitle = "Period: 1 year", 
       y = "Close (USD)")
NGVC_historical |> 
  update_tsibble(index = Date, regular = FALSE) |>
  gg_season(Close, labels = "both", period = "2 year") + 
  labs(title = "Seasonal Plot: Natural Grocers by Vitamin Cottage, Inc.", 
       subtitle = "Period: 2 years", 
       y = "Close (USD)")

# ACF plot 
NGVC_historical |>
  ACF(Close) |>
  autoplot() + 
  labs(title = "Natural Grocers by Vitamin Cottage, Inc.")

# STL plot 
dcmp <- NGVC_historical |>
  model(stl = STL(Close))
components(dcmp) |>
  autoplot()

# --------------- stock index - NYSE ---------------
# Read in NYSE data
NYSE_data <- read.csv("^NYA.csv")

# Data from 1 January 2018 to 30 September 2023 
NYSE <- NYSE_data |>
  filter(Date >="2018-01-01" & Date <= "2023-09-30") |>
  mutate(Day = row_number()) |> 
  mutate(Close = as.numeric(Close)) |>
  mutate(Date = as_date(Date)) |>
  as_tsibble(index = Day) 

# Standardization  
NGVC_historical <- NGVC_historical |>
  mutate(norm_Close = (NGVC_historical$Close - mean(NGVC_historical$Close))/sd(NGVC_historical$Close)) 

NYSE <- NYSE |> 
  mutate(norm_Close = (NYSE$Close - mean(NYSE$Close))/sd(NYSE$Close)) 

# Plot of NYSE and NGVC in 6 years period
ggplot() + 
  geom_line(NYSE, mapping = aes(x = Date, y = norm_Close), color = "grey") + 
  geom_line(NGVC_historical, mapping = aes(x = Date, y = norm_Close), color = "purple") + 
  labs(title = "NGVC (purple) vs NYSE (grey)",
       subtitle = "1 January 2018 to 30 September 2023",
       y = "Close (USD)") 

# --------------- Split data into training and test sets ---------------
# Training set
NGVC_training <- NGVC |>
  filter(Date < "2023-08-01")

# Test set
NGVC_test <- NGVC |>
  filter(Date >= "2023-08-01")

# Basic forecasting method
NGVC_fit <- NGVC_training |>
  model(
    Mean = MEAN(Close), 
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

NGVC_fc <- NGVC_fit |>
  forecast(new_data = NGVC_test)

NGVC_fc |>
  autoplot(NGVC_training, level = NULL) +
  autolayer(NGVC_test, Close, colour = "black") +
  guides(colour = guide_legend(title = "Forecast"))+ 
  labs(title = "Natural Grocers by Vitamin Cottage, Inc.",
       subtitle = "Forecast: training and test sets",
       y = "Close (USD)")

# Best forecast method - Drift
# Forecast for 14 consecutive trading days
NGVC_fit14 <- NGVC |>
  model(RW(Close ~ drift()))

# Check the residuals
NGVC_fit14 |>
  gg_tsresiduals()

# Forecast for 14 consecutive trading days (Cont.)
NGVC_fc14 <- NGVC_fit14 |>
  forecast(h = 14)

# Plot of both actual and forecasted values
NGVC_fc14 |>
  autoplot(NGVC_oct) +
  autolayer(NGVC, Close, colour = "black") + 
  labs(title = "Natural Grocers by Vitamin Cottage, Inc.",
       subtitle = "Forecast: 14 consecutive trading days",
       y = "Close (USD)")

# Accuracy
accuracy(NGVC_fit14)
accuracy(NGVC_fc14, NGVC_oct)

