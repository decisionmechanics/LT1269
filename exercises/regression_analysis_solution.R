library(car)

str(Prestige)

summary(Prestige$prestige)

hist(Prestige$prestige)

table(Prestige$type)

cor(Prestige$income, Prestige$education)

cor(Prestige[c("education","income","women")])

pairs(Prestige[c("education","income","women")])

library(stats)

prestige_model <- lm(prestige ~ ., data = Prestige)
# prestige_model <- lm(prestige ~ education + women + income + type + census, data = Prestige)

prestige_model

summary(prestige_model)
