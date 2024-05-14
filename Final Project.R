# Import the fpp3 library
library(fpp3)

# Import the csv 
cpi <- readr::read_csv("CORESTICKM159SFRBATL.csv")

# Change the names of the columns to be easier to work with
names(cpi) <- c("Date", "CPI")

# Convert the Date column to mdy format
cpi$Date <- yearmonth(ymd(cpi$Date))

# Create a tsibble from the data frame
cpi <- as_tsibble(cpi, index=Date)

# Plot the series
autoplot(cpi, CPI) +
  labs(title="Sticky Price CPI less Food and Energy")

# Create an indicator column for COVID-19
# Mar 2020 - May 2023 : 1
# Everything else: 0
cpi <- cpi |>
  mutate(COVID = if_else(Date >= as.Date("2020-03-01") & Date <= as.Date("2023-05-01"), 1, 0))


# Calculate the cutoff date by going back 5 years and 3 months 
latest_yearmonth <- max(cpi$Date)
years_back <- year(latest_yearmonth) - 5
months_back <- month(latest_yearmonth) - 3

if (months_back <= 0) {
  years_back <- years_back - 1
  months_back <- 12 + months_back
}

cutoff_yearmonth <- yearmonth(paste(years_back, months_back, sep="-"))

# Filter the data to include only the data from the 
# last 5 years and 2 months
cpi_5 <- cpi %>%
  filter(Date > cutoff_yearmonth)

# plot the most recent 5 years of cpi data
autoplot(cpi_5) + 
  labs(title="Sticky Price CPI since Jan 2019")

# Plot the most recent 5 years of CPI data with a highlight 
# for the COVID-19 pandemic period
autoplot(cpi_5) +
  geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2023-05-1"), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.01) + 
  annotate("text", x = as.Date("2021-09-01"), y = Inf, label = "COVID-19 Pandemic", vjust = 1.5, color = "black", size = 3) +
  labs(title = "Sticky Price CPI since Jan 2019") +
  theme_minimal()


# STL Decomposition
dcmp <- cpi_5 |>
  model(STL(CPI)) |>
  components()

# Plot the STL Decomposition
dcmp |>
  autoplot() + 
  labs(title="STL Decomposition")

# Extract the remainder component
remainder <- dcmp |> select(remainder)

# Plot the ACF of the remainder component
remainder |>
  ACF() |>
  autoplot() + 
  labs(title="ACF of Remainder Component")



# Leave out the most recent observation(s) (March and April 2024)
train <- cpi_5 |>
  filter(Date < yearmonth("Mar 2024"))

# Fit and forecast 4 basic models (mean, naive, seasonal naive, and drift)
fit <- train |>
  model(
    Mean = MEAN(CPI),
    `Naive` = NAIVE(CPI),
    `Seasonal naive` = SNAIVE(CPI),
    Drift = RW(CPI ~ drift())
  )

# Forecast the next (1-2) months
fc <- fit |> forecast(h=1)

# Plot the forecasts against the actual data
fc |>
  autoplot(train, level=NULL) + 
  autolayer(
    filter_index(cpi_5, "2012-12" ~ .),
    colour = "black"
  ) +
  labs(title="Simple Forecasts for Monthly CPI") +
  guides(colour = guide_legend(title="Forecast"))

# Check the accuracy of the 4 simple forecasts
accuracy(fc, cpi_5) |>
  arrange(RMSE)

# Plot the best forecast with intervals (Drift)
train |>
  model(RW(CPI ~ drift())) |>
  forecast(h=2) |>
  autoplot(cpi_5) +
  labs(title="Drift Distributional Forecast")

# Look at the residuals from the best performing model (Drift)
cpi_5 |>
  model(RW(CPI ~ drift())) |>
  gg_tsresiduals() + 
  labs(title="Rediduals from the Drift Forecast")


# Fit an ETS model (A, Ad, N)
ets_fit <- train |>
  model(ETS(CPI))

# Look at the AICc of the ETS(A, Ad, N) model
report(ets_fit)

# Forecast the next (1-2) month(s) with the ETS model
ets_fc <- forecast(ets_fit, h=1)

# Plot the forecast on the original data
ets_fc |>
  autoplot(cpi_5) + 
  labs(title="Forecast of ETS(A, Ad, N) Model")

# Check the accuracy of the ETS(A, Ad, N) model
accuracy(ets_fc, cpi_5)

# Look at the residuals from the ETS(A, Ad, N) model
gg_tsresiduals(ets_fit) + 
  labs(title="Residuals from ETS(A, Ad, N) Model")


# Plot the ACF and the PACF
gg_tsdisplay(cpi_5, plot_type="partial") + 
  labs(title="ACF and PACF")

# Gurerro (lambda = 0.515)
cpi_5 |>
  features(CPI, features = guerrero)

# Plot the Box-Cox transform on top of the original series
ggplot(cpi_5, aes(x = Date)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = box_cox(CPI, lambda=0.515), 
                colour = "Log Transformed CPI")) +
  labs(title = "Comparison of Original and Box-Cox Transformed CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Log Transformed CPI"),
                      values = c("black", "red"))

# Create a logged cpi dataset
log_cpi <- cpi_5 |>
  mutate(CPI = box_cox(CPI, lambda=0.515))

# Create a training set with the logged values
log_train <- log_cpi |>
  filter(Date < yearmonth("Mar 2024"))

# Try fitting an ETS model on the box-cox transformed series (A, Ad, N)
ets_fit_log <- log_train |>
  model(ETS(CPI))

# Look at the AICc of the ETS model on the box-cox transformed series
report(ets_fit_log)

# Forecast with the ETS(A, Ad, N) model
ets_fc_log <- forecast(ets_fit_log, h=1)

# Plot the log forecast against the log data
ets_fc_log |>
  autoplot(log_cpi) + 
  labs(title="Forecast for ETS(A,Ad,N) Model on Box-Cox Transformed CPI")

# Back-transform to get a forecast on the original data
back_tr_fc <- ets_fc_log |>
  mutate(CPI = exp(CPI))

# Plot the log ETS forecast on the original data
back_tr_fc |>
  autoplot(cpi_5) + 
  labs(title="Forecast for ETS(A,Ad,N) Model on Back-Transformed CPI")

# Look at the accuracy of the ETS model with the logged data 
accuracy(back_tr_fc, cpi_5)

# Test for stationarity
log_cpi |>
  features(CPI, unitroot_kpss)
# p-value = 0.01 < 0.05 => evidence on non-stationarity

# Determine the order of differencing needed to make the series stationary
log_cpi |>
  features(CPI, unitroot_ndiffs)

# Difference the logged series
log_cpi <- log_cpi |>
  mutate(CPI = difference(CPI))

# Recheck the logged series for stationarity
log_cpi |>
  features(CPI, unitroot_kpss)
# P-value = 0.1 > 0.05 => series is now stationary

# Rename the CPI column in log_cpi to log_CPI
log_cpi <- log_cpi |>
  rename(log_CPI_diff = CPI)

# Do a left join to combine the tsibbles
combined <- cpi_5 |>
  left_join(log_cpi, by="Date")

# Plot the differenced log cpi against the original
ggplot(combined, aes(x = Date)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = log_CPI_diff, colour = "Box-Cox Differenced CPI")) +
  labs(title = "Comparison of Original and Box-Cox Differenced CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Box-Cox Differenced CPI"),
                      values = c("black", "red"))

# Look at the ACF and PACF of the differenced log cpi
gg_tsdisplay(log_cpi, log_CPI_diff, plot_type="partial") + 
  labs(title="ACF and PACF of Differenced Box-Cox Transformed CPI")

# Try fitting an ARIMA model to the training set
arima_fit <- train |>
  model(ARIMA(CPI))

# The model chosen was an ARIMA(1,1,0)(0,0,1)[12]
# (1,1,0) means AR(1), 1st-Order Differencing, MA(0)
# (0,0,1) means seasonal AR(0), no seasonal differencing, 
# and seasonal MA(1)
# [12] is the period of seasonality (monthly data)
print(arima_fit)

# Check the AICc of the ARIMA model
report(arima_fit)

# Check the residuals from the ARIMA model
arima_fit |> gg_tsresiduals()

# Look at the fitted values against the actual data
fitted_values <- fitted(arima_fit)

# Combine the actual data with the fitted values
comparison_data <- train |>
  bind_cols(fitted_values) |>
  rename(Fitted = `.fitted`)

# Plot the fitted values against the actual data
ggplot(comparison_data, aes(x = Date...1)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = Fitted, colour = "Fitted Values")) +
  labs(title = "Comparison of Original and Fitted CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Fitted Values"),
                      values = c("black", "red"))

# Forecast the next 1-2 month(s) with the ARIMA model
arima_fc <- arima_fit |>
  forecast(h=1)

# Plot the ARIMA forecast
arima_fc |>
  autoplot(cpi_5) + 
  labs(title="Forecast of ARIMA(1,1,0)(0,0,1)[12] Model")

# check the accuracy of the ARIMA forecast
accuracy(arima_fc, cpi_5)

# Try adding a constant to the arima model
arima_fit_2 <- train |>
  model(ARIMA(CPI ~ 1 + pdq(1,1,0) + PDQ(0,0,1)))

print(arima_fit_2)

# Check the AICc of the ARIMA model w/ drift
report(arima_fit_2)

# Forecast the next 1-2 months 
arima_fc_2 <- arima_fit_2 |>
  forecast(h=1)

# Plot the forecast of the ARIMA model with drift 
arima_fc_2 |>
  autoplot(cpi_5) + 
  labs(title="Forecast of ARIMA(1,1,0)(0,0,1)[12] Model w/ Drift")

accuracy(arima_fc_2, cpi_5)

# The best model so far was the drift model
# Drift RMSE = 0.0729
# ETS RMSE = 0.264
# ARIMA RMSE = 0.161
# ARIMA (w/ Drift) RMSE = 0.0927
# ARIMA (w/ Indicator) RMSE = 0.161

# ---

# Try the ARIMA again including the covid indicator
# Try fitting an ARIMA model to the training set
arima_fit <- train |>
  model(ARIMA(CPI ~ COVID))

# The model chosen was an ARIMA(1,1,0)(1,0,0)[12]
# (1,1,0) means AR(1), 1st-Order Differencing, MA(0)
# (1,0,0) means seasonal AR(1), no seasonal differencing, 
# and seasonal MA(0)
# [12] is the period of seasonality (monthly data)
print(arima_fit)

# Check the AICc of the ARIMA model
report(arima_fit)

# Check the residuals from the ARIMA model
arima_fit |> gg_tsresiduals() + 
  labs(title="Residuals from the ARIMA(1,1,0)(1,0,0) model")

# Look at the fitted values against the actual data
fitted_values <- fitted(arima_fit)

# Combine the actual data with the fitted values
comparison_data <- train |>
  bind_cols(fitted_values) |>
  rename(Fitted = `.fitted`)

# Plot the fitted values against the actual data
ggplot(comparison_data, aes(x = Date...1)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = Fitted, colour = "Fitted Values")) +
  labs(title = "ARIMA(1,1,0)(1,0,0) Fitted CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Fitted Values"),
                      values = c("black", "red"))

# Forecast the next 1-2 month(s) with the ARIMA model
arima_fc <- arima_fit |>
  forecast(h=1, new_data=cpi_5)

# Plot the ARIMA forecast
arima_fc |>
  autoplot(cpi_5) + 
  labs(title="Forecast of ARIMA(1,1,0)(1,0,0)[12] Model")

# check the accuracy of the ARIMA forecast
accuracy(arima_fc, cpi_5)

# Try adding a constant to the arima model
arima_fit_2 <- train |>
  model(ARIMA(CPI ~ COVID + 1 + pdq(1,1,0) + PDQ(1,0,0)))

print(arima_fit_2)

# Check the AICc of the ARIMA model w/ drift
report(arima_fit_2)

# Forecast the next 1-2 months 
arima_fc_2 <- arima_fit_2 |>
  forecast(h=1, new_data = cpi_5)

# Plot the forecast of the ARIMA model with drift 
arima_fc_2 |>
  autoplot(cpi_5) + 
  labs(title="Forecast of ARIMA(1,1,0)(1,0,0)[12] Model w/ Drift")

accuracy(arima_fc_2, cpi_5)

# plot the fitted values
# Look at the fitted values against the actual data
fitted_values <- fitted(arima_fit_2)

# Combine the actual data with the fitted values
comparison_data <- train |>
  bind_cols(fitted_values) |>
  rename(Fitted = `.fitted`)

# Plot the fitted values against the actual data
ggplot(comparison_data, aes(x = Date...1)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = Fitted, colour = "Fitted Values")) +
  labs(title = "ARIMA(1,1,0)(1,0,0) w/ Drift Fitted CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Fitted Values"),
                      values = c("black", "red"))

# - - - - - 

# plot a longer forecast

# add more 0's to the covid series
new_dates <- c("2024 Apr", 
               "2024 May",
               "2024 Jun",
               "2024 Jul")

new_dates <- yearmonth(new_dates)

new_data <- tibble(Date = new_dates, 
                   CPI = NA,  # Assuming no CPI data for these future months
                   COVID = 0)  # Set COVID indicator to 0

new_data <- as_tsibble(new_data, index = Date)

extended_cpi_tsibble <- bind_rows(cpi, new_data)

arima_fit <- train |>
  model(ARIMA(CPI ~ COVID))

future_data <- extended_cpi_tsibble |>
  filter(Date > max(train$Date))

arima_fc <- arima_fit |>
  forecast(future_data)

arima_fc |>
  autoplot(cpi_5) + 
  labs(title="Forecast of ARIMA(1,1,0)(1,0,0)[12] Model")

# - - - - - - - - - - - - - -

# Try again with only 3 years of training data

# Calculate the cutoff date by going back 3 years and 3 months 
latest_yearmonth <- max(cpi$Date)
years_back <- year(latest_yearmonth) - 3
months_back <- month(latest_yearmonth) - 3

if (months_back <= 0) {
  years_back <- years_back - 1
  months_back <- 12 + months_back
}

cutoff_yearmonth <- yearmonth(paste(years_back, months_back, sep="-"))

# Filter the data to include only the data from the last 3 years and 2 months
cpi_3 <- cpi %>%
  filter(Date > cutoff_yearmonth)

# Leave out the most recent observation(s) (March and April 2024)
train <- cpi_3 |>
  filter(Date < yearmonth("Mar 2024"))

# Fit and forecast 4 basic models (mean, naive, seasonal naive, and drift)
fit <- train |>
  model(
    Mean = MEAN(CPI),
    `Naive` = NAIVE(CPI),
    `Seasonal naive` = SNAIVE(CPI),
    Drift = RW(CPI ~ drift())
  )

# Forecast the next (1-2) months
fc <- fit |> forecast(h=1)

# Plot the forecasts against the actual data
fc |>
  autoplot(train, level=NULL) + 
  autolayer(
    filter_index(cpi_3, "2012-12" ~ .),
    colour = "black"
  ) +
  labs(title="Simple Forecasts for Monthly CPI") +
  guides(colour = guide_legend(title="Forecast"))

# Check the accuracy of the 4 simple forecasts
accuracy(fc, cpi_3) |>
  arrange(RMSE)

# Plot the best forecast with intervals (Drift)
train |>
  model(RW(CPI ~ drift())) |>
  forecast(h=1) |>
  autoplot(cpi_3) +
  labs(title="Drift Distributional Forecast")

# Look at the residuals from the best performing model (Drift)
cpi_3 |>
  model(RW(CPI ~ drift())) |>
  gg_tsresiduals() + 
  labs(title="Rediduals from the Drift Forecast")


# Fit an ETS model (A, Ad, N)
ets_fit <- train |>
  model(ETS(CPI))

# Look at the AICc of the ETS(A, Ad, N) model
report(ets_fit)

# Forecast the next (1-2) month(s) with the ETS model
ets_fc <- forecast(ets_fit, h=1)

# Plot the forecast on the original data
ets_fc |>
  autoplot(cpi_3) + 
  labs(title="Forecast of ETS(A, Ad, N) Model")

# Check the accuracy of the ETS(A, Ad, N) model
accuracy(ets_fc, cpi_3)

# Look at the residuals from the ETS(A, Ad, N) model
gg_tsresiduals(ets_fit) + 
  labs(title="Residuals from ETS(A, Ad, N) Model")


# Plot the ACF and the PACF
gg_tsdisplay(cpi_3, plot_type="partial") + 
  labs(title="ACF and PACF")

# Gurerro (lambda = 1.37)
cpi_3 |>
  features(CPI, features = guerrero)

# Plot the Box-Cox transform on top of the original series
ggplot(cpi_3, aes(x = Date)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = box_cox(CPI, lambda=1.37), 
                colour = "Log Transformed CPI")) +
  labs(title = "Comparison of Original and Box-Cox Transformed CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Log Transformed CPI"),
                      values = c("black", "red"))

# Create a logged cpi dataset
log_cpi <- cpi_3 |>
  mutate(CPI = box_cox(CPI, lambda=1.37))

# Create a training set with the logged values
log_train <- log_cpi |>
  filter(Date < yearmonth("Mar 2024"))

# Try fitting an ETS model on the box-cox transformed series (A, Ad, N)
ets_fit_log <- log_train |>
  model(ETS(CPI))

# Look at the AICc of the ETS model on the box-cox transformed series
report(ets_fit_log)

# Forecast with the ETS(A, A, N) model
ets_fc_log <- forecast(ets_fit_log, h=1)

# Plot the log forecast against the log data
ets_fc_log |>
  autoplot(log_cpi) + 
  labs(title="Forecast for ETS(A,A,N) Model on Box-Cox Transformed CPI")

# Back-transform to get a forecast on the original data
back_tr_fc <- ets_fc_log |>
  mutate(CPI = exp(CPI))

# Plot the log ETS forecast on the original data
back_tr_fc |>
  autoplot(cpi_5) + 
  labs(title="Forecast for ETS(A,A,N) Model on Back-Transformed CPI")

# Look at the accuracy of the ETS model with the logged data 
accuracy(back_tr_fc, cpi_3)

# Test for stationarity
log_cpi |>
  features(CPI, unitroot_kpss)
# p-value = 0.0167 < 0.05 => evidence on non-stationarity

# Determine the order of differencing needed to make the series stationary
log_cpi |>
  features(CPI, unitroot_ndiffs)

# Difference the logged series
log_cpi <- log_cpi |>
  mutate(CPI = difference(CPI))

# Recheck the logged series for stationarity
log_cpi |>
  features(CPI, unitroot_kpss)
# P-value = 0.0313 > 0.05 => still not stationary

# Difference again 
log_cpi <- log_cpi |>
  mutate(CPI = difference(CPI))

# Recheck the logged series for stationarity
log_cpi |>
  features(CPI, unitroot_kpss)
# P-value = 0.1 > 0.05 => series is stationary

# Rename the CPI column in log_cpi to log_CPI
log_cpi <- log_cpi |>
  rename(log_CPI_diff = CPI)

# Do a left join to combine the tsibbles
combined <- cpi_3 |>
  left_join(log_cpi, by="Date")

# Plot the differenced log cpi against the original
ggplot(combined, aes(x = Date)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = log_CPI_diff, colour = "Box-Cox Differenced CPI")) +
  labs(title = "Comparison of Original and Box-Cox Differenced CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Box-Cox Differenced CPI"),
                      values = c("black", "red"))

# Look at the ACF and PACF of the differenced log cpi
gg_tsdisplay(log_cpi, log_CPI_diff, plot_type="partial") + 
  labs(title="ACF and PACF of Differenced Box-Cox Transformed CPI")

# Try fitting an ARIMA model to the training set
arima_fit <- train |>
  model(ARIMA(CPI))

# The model chosen was an ARIMA(0,2,2)(1,0,0)[12]
# (0,2,2) means AR(0), 2nd-Order Differencing, MA(2)
# (1,0,0) means seasonal AR(1), no seasonal differencing, 
# and seasonal MA(0)
# [12] is the period of seasonality (monthly data)
print(arima_fit)

# Check the AICc of the ARIMA model
report(arima_fit)

# Check the residuals from the ARIMA model
arima_fit |> gg_tsresiduals()

# Forecast the next 1-2 month(s) with the ARIMA model
arima_fc <- arima_fit |>
  forecast(h=1)

# Plot the ARIMA forecast
arima_fc |>
  autoplot(cpi_3) + 
  labs(title="Forecast of ARIMA(0,2,2)(1,0,0)[12] Model")

# check the accuracy of the ARIMA forecast
accuracy(arima_fc, cpi_3)

# Try adding a constant to the arima model
arima_fit_2 <- train |>
  model(ARIMA(CPI ~ 1 + pdq(0,2,2) + PDQ(1,0,0)))

print(arima_fit_2)

# Check the AICc of the ARIMA model w/ drift
report(arima_fit_2)

# Forecast the next 1-2 months 
arima_fc_2 <- arima_fit_2 |>
  forecast(h=1)

# Plot the forecast of the ARIMA model with drift 
arima_fc_2 |>
  autoplot(cpi_3) + 
  labs(title="Forecast of ARIMA(0,2,2)(1,0,0)[12] Model w/ Drift")

accuracy(arima_fc_2, cpi_3)

# The best performing model on the 3 year data was the drift model
# Drift RMSE = 0.025
# ETS RMSE = 0.233
# ARIMA RMSE = 0.27
# ARIMA w/ Drift RMSE = 0.293
# ARIMA w/ indicator RMSE = 0.27
# ARIMA w/ indicator and drift RMSE = 0.293

# Try again with the covid indicator
# Try the ARIMA again including the covid indicator
# Try fitting an ARIMA model to the training set
arima_fit <- train |>
  model(ARIMA(CPI ~ COVID))

# The model chosen was an ARIMA(0,2,0)(1,0,0)[12]
# (0,2,0) means AR(0), 2nd-Order Differencing, MA(0)
# (1,0,0) means seasonal AR(1), no seasonal differencing, 
# and seasonal MA(0)
# [12] is the period of seasonality (monthly data)
print(arima_fit)

# Check the AICc of the ARIMA model
report(arima_fit)

# Check the residuals from the ARIMA model
arima_fit |> gg_tsresiduals()

# Look at the fitted values against the actual data
fitted_values <- fitted(arima_fit)

# Combine the actual data with the fitted values
comparison_data <- train |>
  bind_cols(fitted_values) |>
  rename(Fitted = `.fitted`)

# Plot the fitted values against the actual data
ggplot(comparison_data, aes(x = Date...1)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = Fitted, colour = "Fitted Values")) +
  labs(title = "Comparison of Original and Fitted CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Fitted Values"),
                      values = c("black", "red"))

# Forecast the next 1-2 month(s) with the ARIMA model
arima_fc <- arima_fit |>
  forecast(h=1, new_data=cpi_3)

# Plot the ARIMA forecast
arima_fc |>
  autoplot(cpi_3) + 
  labs(title="Forecast of ARIMA(0,2,0)(1,0,0)[12] Model")

# check the accuracy of the ARIMA forecast
accuracy(arima_fc, cpi_3)

# Try adding a constant to the arima model
arima_fit_2 <- train |>
  model(ARIMA(CPI ~ COVID + 1 + pdq(0,2,0) + PDQ(1,0,0)))

print(arima_fit_2)

# Check the AICc of the ARIMA model w/ drift
report(arima_fit_2)

# Forecast the next 1-2 months 
arima_fc_2 <- arima_fit_2 |>
  forecast(h=1, new_data = cpi_3)

# Plot the forecast of the ARIMA model with drift 
arima_fc_2 |>
  autoplot(cpi_3) + 
  labs(title="Forecast of ARIMA(0,2,0)(1,0,0)[12] Model w/ Drift")

accuracy(arima_fc_2, cpi_5)

# - - - - - - - - - - - - - - 

# Try again with 2 years of training data

# Calculate the cutoff date by going back 2 years and 3 months 
latest_yearmonth <- max(cpi$Date)
years_back <- year(latest_yearmonth) - 2
months_back <- month(latest_yearmonth) - 3

if (months_back <= 0) {
  years_back <- years_back - 1
  months_back <- 12 + months_back
}

cutoff_yearmonth <- yearmonth(paste(years_back, months_back, sep="-"))

# Filter the data to include only the data from the last 3 years and 2 months
cpi_2 <- cpi %>%
  filter(Date > cutoff_yearmonth)

# Leave out the most recent observation(s) (March and April 2024)
train <- cpi_2 |>
  filter(Date < yearmonth("Mar 2024"))

# Fit and forecast 4 basic models (mean, naive, seasonal naive, and drift)
fit <- train |>
  model(
    Mean = MEAN(CPI),
    `Naive` = NAIVE(CPI),
    `Seasonal naive` = SNAIVE(CPI),
    Drift = RW(CPI ~ drift())
  )

# Forecast the next (1-2) months
fc <- fit |> forecast(h=1)

# Plot the forecasts against the actual data
fc |>
  autoplot(train, level=NULL) + 
  autolayer(
    filter_index(cpi_2, "2012-12" ~ .),
    colour = "black"
  ) +
  labs(title="Simple Forecasts for Monthly CPI") +
  guides(colour = guide_legend(title="Forecast"))

# Check the accuracy of the 4 simple forecasts
accuracy(fc, cpi_2) |>
  arrange(RMSE)

# Plot the best forecast with intervals (Drift)
train |>
  model(RW(CPI ~ drift())) |>
  forecast(h=1) |>
  autoplot(cpi_2) +
  labs(title="Drift Distributional Forecast")

# Look at the residuals from the best performing model (Drift)
cpi_2 |>
  model(RW(CPI ~ drift())) |>
  gg_tsresiduals() + 
  labs(title="Rediduals from the Drift Forecast")


# Fit an ETS model (M, A, N)
ets_fit <- train |>
  model(ETS(CPI))

# Look at the AICc of the ETS(M, A, N) model
report(ets_fit)

# Forecast the next (1-2) month(s) with the ETS model
ets_fc <- forecast(ets_fit, h=1)

# Plot the forecast on the original data
ets_fc |>
  autoplot(cpi_2) + 
  labs(title="Forecast of ETS(M, A, N) Model")

# Check the accuracy of the ETS(M, A, N) model
accuracy(ets_fc, cpi_2)

# Look at the residuals from the ETS(M, A, N) model
gg_tsresiduals(ets_fit) + 
  labs(title="Residuals from ETS(M, A, N) Model")


# Plot the ACF and the PACF
gg_tsdisplay(cpi_2, plot_type="partial") + 
  labs(title="ACF and PACF")

# Gurerro (lambda = 1.37)
cpi_3 |>
  features(CPI, features = guerrero)

# Plot the Box-Cox transform on top of the original series
ggplot(cpi_2, aes(x = Date)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = box_cox(CPI, lambda=1.37), 
                colour = "Log Transformed CPI")) +
  labs(title = "Comparison of Original and Box-Cox Transformed CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Log Transformed CPI"),
                      values = c("black", "red"))

# Create a logged cpi dataset
log_cpi <- cpi_2 |>
  mutate(CPI = box_cox(CPI, lambda=1.37))

# Create a training set with the logged values
log_train <- log_cpi |>
  filter(Date < yearmonth("Mar 2024"))

# Try fitting an ETS model on the box-cox transformed series (M, A, N)
ets_fit_log <- log_train |>
  model(ETS(CPI))

# Look at the AICc of the ETS model on the box-cox transformed series
report(ets_fit_log)

# Forecast with the ETS(M, A, N) model
ets_fc_log <- forecast(ets_fit_log, h=1)

# Plot the log forecast against the log data
ets_fc_log |>
  autoplot(log_cpi) + 
  labs(title="Forecast for ETS(M,A,N) Model on Box-Cox Transformed CPI")

# Back-transform to get a forecast on the original data
back_tr_fc <- ets_fc_log |>
  mutate(CPI = exp(CPI))

# Plot the log ETS forecast on the original data
back_tr_fc |>
  autoplot(cpi_2) + 
  labs(title="Forecast for ETS(M,A,N) Model on Back-Transformed CPI")

# Look at the accuracy of the ETS model with the logged data 
accuracy(back_tr_fc, cpi_2)

# Test for stationarity
log_cpi |>
  features(CPI, unitroot_kpss)
# P-value = 0.1 > 0.05 => series is stationary

# Rename the CPI column in log_cpi to log_CPI
log_cpi <- log_cpi |>
  rename(log_CPI_diff = CPI)

# Do a left join to combine the tsibbles
combined <- cpi_2 |>
  left_join(log_cpi, by="Date")

# Plot the differenced log cpi against the original
ggplot(combined, aes(x = Date)) +
  geom_line(aes(y = CPI, colour = "Original CPI")) +
  geom_line(aes(y = log_CPI_diff, colour = "Box-Cox Differenced CPI")) +
  labs(title = "Comparison of Original and Box-Cox Differenced CPI", 
       y = "CPI", 
       x = "Time") +
  scale_colour_manual("", 
                      breaks = c("Original CPI", "Box-Cox Differenced CPI"),
                      values = c("black", "red"))

# Look at the ACF and PACF of the differenced log cpi
gg_tsdisplay(log_cpi, log_CPI_diff, plot_type="partial") + 
  labs(title="ACF and PACF of Differenced Box-Cox Transformed CPI")

# Try fitting an ARIMA model to the training set
arima_fit <- train |>
  model(ARIMA(CPI))

# The model chosen was an ARIMA(0,2,1)
# (0,2,1) means AR(0), 2nd-Order Differencing, MA(1)
print(arima_fit)

# Check the AICc of the ARIMA model
report(arima_fit)

# Check the residuals from the ARIMA model
arima_fit |> gg_tsresiduals()

# Forecast the next 1-2 month(s) with the ARIMA model
arima_fc <- arima_fit |>
  forecast(h=1)

# Plot the ARIMA forecast
arima_fc |>
  autoplot(cpi_2) + 
  labs(title="Forecast of ARIMA(0,2,1) Model")

# check the accuracy of the ARIMA forecast
accuracy(arima_fc, cpi_2)

# The best performing model was the drift model
# Drift RMSE = 0.0886
# Naive RMSE = 0.106
# ETS RMSE = 0.163
# ARIMA RMSE = 0.235
# ARIMA w/ indicator RMSE = 0.27

# ---

# Try again with the covid indicator
# Try fitting an ARIMA model to the training set
arima_fit <- train |>
  model(ARIMA(CPI ~ COVID))

# The model chosen was an ARIMA(0,2,1)
# (0,2,1) means AR(0), 2nd-Order Differencing, MA(1)
print(arima_fit)

# Check the AICc of the ARIMA model
report(arima_fit)

# Check the residuals from the ARIMA model
arima_fit |> gg_tsresiduals()

# Forecast the next 1-2 month(s) with the ARIMA model
arima_fc <- arima_fit |>
  forecast(h=1, new_data = cpi_2)

# Plot the ARIMA forecast
arima_fc |>
  autoplot(cpi_2) + 
  labs(title="Forecast of ARIMA(0,2,1) Model")

# check the accuracy of the ARIMA forecast
accuracy(arima_fc, cpi_2)
