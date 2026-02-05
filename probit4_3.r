rm(list = ls())

# 2. Einlesen der Daten
femlab = read.table("female_labor.txt", header = TRUE)
head(femlab)

femlab$inlf <- factor(femlab$inlf)
femlab$kidslt6 <- factor(femlab$kidslt6)
femlab$kidsge6 <- factor(femlab$kidsge6)

probit_fit <- glm(inlf ~ kidslt6 +kidsge6 +educ +exper, 
                  data = femlab,
                  family = binomial(link = "probit"))
summary(probit_fit)

#Was können wir jetzt darüber sagen, ohne weitere Dinge zu 
#berechnen?
#wir können Aussagen über die Signifikanz und Richtung von Effekten machen

#pnorm ist probit Gegenstück zu plogis, macht aus latenten index eine wahrscheinlichkeit

predict(probit_fit, newdata = data.frame(kidslt6=factor(1),
                                         kidsge6=factor(1),
                                         educ=16,
                                         exper=3), 
        type = "response")
#Wir bekommen die Wahrscheinlichkeit der Erwerbstätigkeit
#mit 40 Prozent.


#Durchschnitts Individuum Wahrscheinlichkeit Erwerbstätigkeit

#mittelwert jeweils

avg_kidslt6 = table(femlab$kidslt6)/length(femlab$kidslt6)
avg_kidsge6 = table(femlab$kidsge6)/length(femlab$kidsge6)

#durchschnittliches individuum

bsp_A3d = c(1, avg_kidslt6[2:4], avg_kidsge6[2:9],
            mean(femlab$educ), mean(femlab$exper))

summary(bsp_A3d)

# Erwartete Wahrscheinlichkeit 
pnorm(bspsummary_A3d%*%probit_fit$coefficients)
mean(femlab$inlf)

library(mfx)

marginali = probitmfx(inlf ~ kidslt6 + kidsge6 + educ + exper, data = femlab)

marginali

#Die Wahrscheinlichkeit erwerbstätig sein ist im durchschnitt um 36 prozent schlechter für Frauen mit zwei Kinder
#unter sechs Jahren als ohne Kinder

library(marginaleffects)
slopes(probit_fit, newdata = "median", type = "response")

# Median berechnen für Faktor-Variablen
med_kidslt6 = median(as.numeric(as.character(femlab$kidslt6)))
med_kidsge6 = median(as.numeric(as.character(femlab$kidsge6)))
med_kidslt6 # Median ist 0 Kinder unter 6 Jahren
med_kidsge6 # Median ist 1 Kind zwischen 6 und 18 Jahren
# Median-Individuum
fem_median = data.frame(
  kidslt6 = factor(med_kidslt6, levels = levels(femlab$kidslt6)),
  kidsge6 = factor(med_kidsge6, levels = levels(femlab$kidsge6)),
  educ = median(femlab$educ),
  exper = median(femlab$exper)
)
# Marginaler Effekt am  für eine stetige Variable (z. B. educ)
dnorm(predict(probit_fit, fem_median)) * probit_fit$coefficients["educ"]

# Ein zusätzliches Jahr an Bildung erhöht die Wahrscheinlichkeit in Arbeit zu sein für die Median-Frau um 4,8 ProzentPUNKTE!

#Am Durchschnitt:
library(mfx)
probitmfx(inlf ~kidslt6 + kidsge6 + educ + exper, data = femlab,
          atmean = TRUE)
#2.5 Prozent mehr Wahrschieinlichkeit in Arbeit zu sein bei einem Jahr mehr Berufserfahrung
#bei einer durchschnittlichen Frau
