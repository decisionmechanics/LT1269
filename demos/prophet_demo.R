library(dplyr)
library(ggplot2)
library(lubridate)
library(prophet)
library(readr)

theme_set(theme_minimal())

# Amazon revenues

data <- amazon_revenue_data |>
  rename("y"=revenue) |>
  mutate(ds=ym(year)) |>
  select(ds, y)

model <- prophet(data)

future <- make_future_dataframe(model, freq="quarter", periods=20)

tail(future)

forecast <- predict(model, future)

tail(forecast)

plot(model, forecast, xlabel="Year", ylabel="Revenue ($)")

prophet_plot_components(model, forecast)

# COVID 19

covid_data <- read_csv("data/covid_19.csv") |>
  filter(iso_code == "GBR") |>
  select(ds=date, y=new_cases)

covid_model <- prophet(covid_data)

covid_future <- make_future_dataframe(covid_model, periods=30)

covid_forecast <- predict(covid_model, covid_future)

plot(covid_model, covid_forecast, xlabel="Day", ylabel="New cases") +
  add_changepoints_to_plot(covid_model)

dyplot.prophet(covid_model, covid_forecast)

covid_model_tuned <- prophet(
  covid_data, 
  weekly.seasonality=FALSE, 
  changepoint.range=1,
  changepoint.prior.scale=0.75
)

covid_future_tuned <- make_future_dataframe(covid_model_tuned, periods=90)

covid_forecast_tuned <- predict(covid_model_tuned, covid_future_tuned)

plot(
  covid_model_tuned, 
  covid_forecast_tuned, 
  xlabel="Day", 
  ylabel="New cases"
) +
  add_changepoints_to_plot(covid_model_tuned)
