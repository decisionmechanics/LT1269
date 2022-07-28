library(dplyr)
library(lubridate)
library(readr)
library(TTR)
library(xts)

# Oil prices

oil_price_data <- read_csv("brent_spot_price.csv")

View(oil_price_data)

oil_prices <- xts(oil_price_data$price, order.by=ym(oil_price_data$month))

str(oil_prices)

head(oil_prices)
tail(oil_prices)

plot(oil_prices)

oil_prices["2000"]
oil_prices["2000/2005"]
oil_prices["2020/"]

oil_prices_1990s <- oil_prices["1990/199912"]
oil_prices_2010s <- oil_prices["2010/201012"]

xts::first(oil_prices, 3)
xts::first(oil_prices, "2 years")

tmp <- oil_prices
tmp["1991"] <- NA

View(tmp)

na.locf(tmp)
na.approx(tmp)

rbind(oil_prices_2010s, oil_prices_1990s)

oil_prices["1999"] * 1.1
oil_prices["1999"] + oil_prices["2000"]

to.yearly(oil_prices)

# COVID 19

covid_data <- read_csv("covid_19.csv") |>

covid_data <- filter(covid_data, iso_code == "GBR")

new_cases_raw <- xts(covid_data$new_cases, order.by=covid_data$date)
plot(new_cases_raw)

new_cases <- xts(covid_data$new_cases_smoothed, order.by=covid_data$date)
plot(new_cases)

ts <- rollapply(new_cases_raw, width = 7, FUN = mean)
plot(ts)

new_deaths <- xts(covid_data$new_deaths_smoothed, order.by=covid_data$date)
plot(new_deaths)

normalised_cases <- new_cases / max(new_cases, na.rm=TRUE)
normalized_deaths <- new_deaths / max(new_deaths, na.rm=TRUE)

covid_ts <- cbind(normalized_cases, normalized_deaths)

plot(covid_ts)

shifted_deaths <- lag.xts(normalized_deaths, k = -14)
plot(cbind(normalized_cases, shifted_deaths))

plot(ROC(new_cases))

to.weekly(new_cases_raw)
