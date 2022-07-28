library(dplyr)
library(lubridate)
library(prophet)
library(readr)

# Canadian new motor vehicle sales

vehicle_sales_data <- read_csv("data/new_vehicle_sales.csv") |>
  mutate(ds=ym(REF_DATE)) |>
  select(ds, y=VALUE)
  
View(vehicle_sales_data)

vehicle_sales_model <- prophet(vehicle_sales_data)

vehicle_sales_future <- make_future_dataframe(
  vehicle_sales_model, 
  freq="month", 
  periods=12
)

vehicle_sales_forecast <- predict(vehicle_sales_model, vehicle_sales_future)

tail(vehicle_sales_forecast)

plot(
  vehicle_sales_model, 
  vehicle_sales_forecast, 
  xlabel="Year", 
  ylabel="Vehicles"
)

dyplot.prophet(vehicle_sales_model, vehicle_sales_forecast)

prophet_plot_components(vehicle_sales_model, vehicle_sales_forecast)

# COVID 19

usa_covid_data <- read_csv("data/covid_19.csv") |>
  filter(iso_code == "USA") |>
  select(ds=date, y=new_cases)

View(usa_covid_data)

usa_covid_model <- prophet(usa_covid_data)

usa_covid_future <- make_future_dataframe(usa_covid_model, periods=90)

usa_covid_forecast <- predict(usa_covid_model, usa_covid_future)

plot(usa_covid_model, usa_covid_forecast, xlabel="Day", ylabel="New cases") +
  add_changepoints_to_plot(usa_covid_model)

dyplot.prophet(usa_covid_model, usa_covid_forecast)

usa_covid_model_tuned <- prophet(
  usa_covid_data, 
  weekly.seasonality=FALSE, 
  changepoint.range=1,
  changepoint.prior.scale=0.75
)

usa_covid_future_tuned <- make_future_dataframe(
  usa_covid_model_tuned, 
  periods=90
)

usa_covid_forecast_tuned <- predict(
  usa_covid_model_tuned, 
  usa_covid_future_tuned
)

plot(
  usa_covid_model_tuned, 
  usa_covid_forecast_tuned, 
  xlabel="Day", 
  ylabel="New cases"
) +
  add_changepoints_to_plot(usa_covid_model_tuned)
