library(car)

income <- read.csv2("SOEP_2021_PS2.csv")
head(income)
fit1 <- lm(formula = HHnetinc ~ schooling, data = income)
fit2 <- lm(formula = HHnetinc ~ schooling + expFT, data = income)

AIC(fit1, fit2)
BIC(fit1, fit2)

#AIC heißt kleiner ist besser -> fit2 ist besser
#BI heißt auch kleiner ist besser -> fit1 ist besser, beim BIC ist die Bestrafung für zusätzliche Variablen höher

# R^2 (größer ist besser)
summary(fit1)$r.squared
summary(fit2)$r.squared
# => Modell 2 ist "besser"

# adjusted R^2 (größer ist besser)
summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
# => Modell 2 ist "besser"

fit3 <- lm(formula = HHnetinc ~ schooling + expFT + expPT, data = income)

best <- step(fit3)
summary(best)
#Es wird das Modell ohne expPT ausgewählt
