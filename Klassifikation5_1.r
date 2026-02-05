setwd("/home/richard/Documents/Uni/Master/statistik/R-Sachen")

rm(list=ls())
miet <- read.table("MieteMuenchen2015_PS4.raw", header = TRUE)

logit_fit <- glm(badextra ~ mieteqm +  flaeche, 
                 family = binomial(
                   link = "logit"
                 ), data = miet)

#Wir wollen eine Klassifikationsmatrix erstellen, in der eine Wohnung nur dann mit zu
#zusätzlichem Bad prognostiziert wird, wenn die geschätze zugehörige Wahrscheinlichkeit über
#50 Prozent liegt

#wir erstellen einen Vektor mit den Wahrscheinlichkeiten der jeweiligen
#Wohnungen ein Bad extra zu haben
y_prob <- predict(logit_fit, newdata = miet, type = "response")

#wir erstellen einen zusätzlichen vektor mit 1=TRUE/0=FALSE ob größer als 50 Prozent

y_hat <- ifelse(y_prob > 50, 1, 0)

#Klassifikationsmatrix erstellen

tab <- table(y_hat, miet$badextra)
tab
prop.table(tab)
mean(miet$badextra == 0)

#Wahrscheinlichkeit für richtig ist
sum(diag(tab))/sum(tab)

#Wie schneidet die Qualifikation gegenüber eine zufälligen Klassfikation ab?
#Zufällig wäre 50/50, wir haben 80 Prozent Modellgenauigkeit

#Wahrscheinlichkeit für richtig 
alpha <- sum(diag(tab))/sum(tab)
alpha


# Press-Q-Wert bestimmen
n = length(miet$badextra)
G = 2;
pq = (n - n*G*alpha)^2/(n*(G-1))

# Bestimmung p-Wert
1 - pchisq(pq, 1)

fub = c("#CCFF00", "#004659", "#813353", "#00A4D1", "#E57050", "#58756A");

################################################
## ZUSATZ: Dichte der Chi-Quadrat Verteilung ###
################################################
x = seq(0, qchisq(0.99, 2), length.out = 1000)
fx = dchisq(x, 2);
plot(x, fx, type = "l",
     main = "Chi-Quadrat Dichte",
     xlab = "x",
     ylab = "f(x)",
     ylim = c(0, 0.5))
# 5% Fläche
x_tail = x[x >= qchisq(0.95, 2)]
y_tail = dchisq(x_tail, 2)
polygon(
  c(qchisq(0.95, 2), x_tail, max(x_tail)),
  c(0, y_tail, 0),
  col = fub[2],
  border = NA
)
abline(v = qchisq(0.95, 2), lty = 2, lwd = 2, col = fub[5])
text(qchisq(0.95, 2), 0.1,
     col = fub[5], 
     labels = "5%",
     pos = 4)


#Wahrscheinlichkeit für eine Wohnung die 60 qm hat und 7,50 kostet

predict(logit_fit, newdata = data.frame(mieteqm = 7.5, flaeche = 60), type = "response")

#5 Prozent Wahrscheinlikeit


predict(logit_fit, newdata = data.frame(mieteqm = 20.5, flaeche = 60), type = "response")

#Wir führen eine Waldtest durch

miet$lage <- factor(miet$lage)
levels(miet$lage)= list(normal=0, gut=1, sehrgut=2)
logit_fit_R <- glm(badextra ~ mieteqm + flaeche  + lage,
                   family = binomial(link = "logit"),
                   data = miet)

library(lmtest)
lrtest(logit_fit, logit_fit_R)

waldtest(logit_fit, logit_fit_R)

#Interpretation lrtest:
#loglik heißt höher ist besser, Unterschied minimal besser bei neuem
#Modell. P Wert von Chisquared (UNterschied zwischen Modellen) ist nicht besonders klein,
#nicht signifikanter Unterschied beim üblichen Signifikanzniveau

#Waldtest führt zu selben Ergebnis