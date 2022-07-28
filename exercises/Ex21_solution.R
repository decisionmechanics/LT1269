mtcars[1, 2]

str(iris)
levels(iris$Species)

mtcars["Merc 280C", "mpg"]

nrow(mtcars)

ncol(mtcars)

head(mtcars)

mtcars[c("mpg", "gear")]

mtcars[c(1,5:10)]

my_vars <- names(mtcars) %in% c("mpg", "cyl", "disp")
mtcars[!my_vars]

mtcars[c(-3,-5)]

mtcars_copy <- mtcars 
mtcars_copy$qsec <- mtcars_copy$vs <- NULL
mtcars_copy

mtcars[[9]]

mtcars[["am"]]

mtcars$am

mtcars[, "am"]

mtcars[1:5,]

library(rattle)
data(weather, package="rattle")

weather[which(weather$Location == "Canberra" & weather$Rainfall > 16), ]

attach(weather)
weather[which(Location == "Canberra" & Rainfall > 16), ]

subset(weather, Rainfall >= 15, select=c(Location, Date, Rainfall))

subset(weather, WindGustDir == "NW" & RainTomorrow == "Yes", select=MinTemp:Sunshine)