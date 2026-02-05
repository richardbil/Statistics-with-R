rm(list = ls())

getwd()

wald <- read.csv2("buche.csv")
head(wald)

wald$frische <- factor(wald$frische)
unique(wald$frische)

#wir haben aus den Aufgben keine wirkliche Übersicht über die Daten,
#Schirm wird anscheinend auf eine Art erfasst bei der es Sinn macht es mit 100 zu multiplizieren

wald$schirm <- wald$schirm*100

#Wieviele Logit Gleichungen und Parameter werden in unserem Multinomialen Modell geschätzt? 

#Wenn J Ausprägungen der abhängigen Variable -> J-1 Logit Gleichungen
#Weil eine Kategorie als Referenzkategorie festgelegt wird und für jede andere eine
#gerechnet wird. 
#Wenn K erklärende Variablen -> (K+1) * (J-1) Parameterschätzungen
#Weil pro Gleichung K+1 Parameter wegen K Variablen plus Intercept

#In unserem Fall, Ausprägungen anzeigen lassen: 

unique(wald$schaedigung)
#Keine, Leichte, Schwere also 2 Logit Gleichungen
#und Parameter: Konstante, alter, schirm, hoehe, frische also 4, also
#werden 8 Parameter geschätzt
#Falsch frische hat drei Ausprägungen also zwei DummyVariablen, also 
#12

library(nnet)
mlogit <- multinom(schaedigung ~ alter + schirm + hoehe + frische, data = wald)
summary(mlogit)

library(broom)
tidy(mlogit)
#ylevel ist die Kategorie von Schaedigung, keine ist der Referenzwert
#Koeff bei Estimate sagt uns wie sich die logodds im Verhältnis zur Referenzkategorie ändern, 
#also die logarithmierten Odds (Odds ist das Verhältnis von Wahrscheinlichkeit zur Gegenwahrscheinlichkeit)
#ceteris paribus

#wir wollen die durchschnittlichen marginalen effekte für Alter
#also den Mittelwert der marginalen Effekte bei jeder Beobachtung

library(marginaleffects)
avg_slopes()