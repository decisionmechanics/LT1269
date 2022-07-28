library(corrplot)
library(dplyr)
library(forecast)
library(janitor)
library(lubridate)
library(PerformanceAnalytics)
library(readr)
library(RColorBrewer)
library(TTR)

setwd("/home/user/course")

# Champagne sales

champagne_sales_data <- read_csv("data/champagne_sales.csv")

champagne_sales <- xts(
  champagne_sales_data$sales,
  order.by=ym(champagne_sales_data$month)
)

plot(champagne_sales, main="Champagne sales")

plot(champagne_sales, type="h", main="Champagne sales")

chart.TimeSeries(
  champagne_sales,
  period.areas = c("1969"),
  period.color = "#0000FF22",
  event.lines = c("Jan 64"),
  event.labels = c("")
)

# COVID 19

covid_data <- read_csv("data/covid_19.csv") |>
  filter(iso_code == "GBR")

new_cases <- xts(covid_data$new_cases_smoothed, order.by=covid_data$date)
new_deaths <- xts(covid_data$new_deaths_smoothed, order.by=covid_data$date)

par(mfrow = c(2, 1))
plot(new_cases, main = "New cases")
plot(new_deaths, main = "New deaths")
dev.off()

lattice::xyplot(cbind(new_cases, new_deaths))

plot_panel <- function(x, ...) {
  lines(x, ...)
  abline(v = as.Date("2021-01-10"), col = "red")
  abline(v = as.Date("2021-01-24"), col = "red")
}

plot.zoo(cbind(new_cases, new_deaths), main = "COVID 19", panel=plot_panel)

new_cases_change <- ROC(new_cases)

lattice::xyplot(cbind(new_cases, new_cases_change))

colors=c("red", "black")
covid <- merge(new_cases, new_deaths, all = FALSE)
barplot(covid["2020-03::2020-04"], col=colors)
graphics::legend(
  "topleft",
  c("New deaths", "New cases"),
  col=rev(colors),
  lwd=5
)

# Amazon revenue

amazon_revenue_data <- read_csv("data/amazon_revenue.csv")

amazon_revenues <- xts(
  amazon_revenue_data$revenue,
  order.by=ym(amazon_revenue_data$quarter)
)

plot(amazon_revenues)

amazon_revenue_change <- ROC(amazon_revenues)

lattice::xyplot(cbind(amazon_revenues, amazon_revenue_change))

hist(amazon_revenue_change)

d <- density(amazon_revenue_change, na.rm=TRUE)
lines(d, col = "red", lwd = 2)

Acf(
  amazon_revenue_change,
  na.action = na.pass,
  main = "Amazon returns ACF"
)

decomposition <- decompose(ts(amazon_revenues, frequency = 4))
plot(decomposition)

# Tech stocks

tech_stock_data <- read_csv("data/tech_stocks.csv")

tech_stocks <- xts(
  select(tech_stock_data, -date),
  order.by = ymd(tech_stock_data$date)
)

tech_stock_returns <- ROC(tech_stocks)

boxplot(
  cbind(tech_stock_returns$aapl, tech_stock_returns$ibm),
  horizontal = TRUE,
  col = "red"
)

Acf(
  tech_stock_returns$ibm,
  na.action = na.pass,
  main = "IBM returns ACF"
)

qqnorm(tech_stock_returns$ibm, main = "IBM returns Q-Q plot")
qqline(tech_stock_returns$ibm, col = "red")

plot(coredata(tech_stock_returns[, c("ibm", "msft")]))
abline(
  reg=lm(tech_stock_returns$ibm ~ tech_stock_returns$msft),
  col="red",
  lwd=2
)

pairs(coredata(tech_stock_returns))

tech_stock_correlations <- cor(
  coredata(tech_stock_returns),
  use = "pairwise.complete.obs"
)

View(tech_stock_correlations)

corrplot(tech_stock_correlations)

corrplot(tech_stock_correlations, method = "color", type = "upper")
