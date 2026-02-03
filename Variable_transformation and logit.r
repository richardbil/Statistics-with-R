soep <- read.csv2("SOEP_2021_PS2.csv")

head(soep)
summary(soep)
soep$age <- 2021 - soep$ybirth
head(soep)
soep$logHH <- log(soep$HHnetinc)
head(soep)

soep$age2 <- soep$age**2
head(soep)

fit1 <- lm(formula = logHH ~ schooling + age + age2 + partner, data = soep)

#beim logarithmieren müssen alle werte entfernt werden die null sind, deshalb Fehler  
soep <- soep[soep$HHnetinc>0, ]
soep$logHH <- log(soep$HHnetinc)
fit1 <- lm(formula = logHH ~ schooling + age + age2 + partner, data = soep)

summary(fit1)
#idee hinter quadratisch? Was wenn ein nicht lineares verhältnis besteht. zum beispiel sind zb höhepunkte im mittleren alter
# was ist die interpretation von fit1? 
# alle koeff sind bei 0.05 signifikant 

alter_seq = seq(18, 80, by = 1)
#erstellt vektor mit werten 18 bis 80

#predict function nimmt die werte die man reingibt, nimmt die coeffizienten aus dem modell
# und errechnet für hier zum beispiel jedes alter 
#wie das einkommen wahrscheinlich aussieht, das ergebnis ist ein vektor mit den werten für jedes alter in diesem fall
schooling_constant = 12
partner_const = 0

predict_lnHHnetinc <- predict(fit1, newdata = data.frame(
  schooling = schooling_constant,
  age = alter_seq,
  age2 = alter_seq**2,
  partner = partner_const
))

plot(alter_seq, predict_lnHHnetinc,
     type = "l",
     col = "purple",
     lwd = 3,
     xlab = "Alter in Jahren",
     ylab = "Errechnetes Einkommen Haushalt",
     main = "geschätztes einkommen aus log lm")  


#nullhyothese:
# Die Koeff. von Alter und qudaratierem Alter sind beide gleich null
#Alternativhypothese: 
#Einer von beiden Koeff. ist ungleich null oder beide sind ungleich null

#Test der Hypothese 

library(car)
linearHypothesis(fit1, c("age=0", "age2=0"))

#Model 1 ist restricted auf beide Koeff. sind null, Model 2 ist nicht 
#restricted, also das volle Modell und das Ergebnis des Tests ist,
# dass die Nullhypothese zu jedem gängigen Signifikanniveau abgelehnt werden kann

#Optimales Modell ermitteln, mit Vorwärts und Rückwärts Auswahl

maxi_modell <- lm(formula = logHH ~ schooling + age + age2 + partner
                  +expPT + expFT, data = soep)

opt_modell_back <- step(maxi_modell, direction = "backward")
summary(opt_modell_back)

#Optimales Modell ist volles mit quadriertem Alter, ohne normalem Alter

#Bei Forward müssen wir der Funktion sagen, welche Variablen hinzugefügt werden sollen.

smallest_modell <- lm(formula = logHH ~1, data = soep)
opt_modell_for <- step(smallest_modell, direction = "forward",
                       scope = (~schooling + age +age2 +partner 
                                +expPT + expFT))
summary(opt_modell_for)
#Das selbe Modell wird ermittelt

#Was wenn unterschiedliche Modelle ermittelt werden? 
#Entweder Theoriegetriebene Auswahl oder alle Modelle einmal miteinander vergleichen
#Merken, dass bei step-Verfahren unterschiedliche Pfade abgegangen werden können 
#und keine Variablen wieder entfernt werden -> kann unterschiedliches Ergebnis liefern

#den Workspace wieder leeren um Übersichtlichkeit herzustellen
rm(list = ls())

sambia <- read.csv2("sambia92.csv")

sambia$m_bildung <- factor(sambia$m_bildung)
levels(sambia$m_bildung) = c("keine Ausbildung", "Grundschule",
                             "Volksschule",
                             "höherer Abschluss")
sambia$region <- factor(sambia$region)
levels(sambia$region) = c("Central","Copperbelt", "Eastern", "Luapula",
                          "Lusaka", "Northern", "North-Western", "Southern
                          ", "Western")

fitsam <- lm(formula = zscore ~ m_alterg + m_groesse + m_bmi
             + m_bildung + region, data = sambia)
summary(fitsam)

#Wir sehen, dass manche Regionen signifikant sind und andere nicht, es wäre sinnvoll 
#zusammen zu kontrollieren, ob die Variable Sinn macht. Je nach politischer 
#Zustand macht die Variable Region unterschiedlich viel Sinn, die Regression
#lässt vermuten, dass manche Regionen starke Auswirkungen haben.

#Nullhypothese:
#Die Regionen haben alle einen nicht von null verschiedenen Koeff.

#Alternativhypothese:
#Mindestens einer der Koeff. von Region ist ungleich null.

#Für lm wird in linearHypothsis ein Ftest normalerweise genutzt. 
#Anders bei glm, dann wird standartmäßig ein Waldtest genutzt
# Die Teststatistik folgt unter der H0 einer F-Verteilung F ~ F(r = 8, n-(k+1) = 4421-15 = 4406)
library(car)
test = linearHypothesis(fitsam, c("regionCopperbelt = 0", 
                           "regionEastern = 0", 
                           "regionLuapula = 0", 
                           "regionLusaka = 0", 
                           "regionNorthern = 0", 
                           "regionNorth-Western = 0", 
                            
                           "regionWestern = 0"))


summary(test)
print(test)

#Nullhypothese wird bei allen gängigen Signifikanzniveaus abgelehnt

#Workspace leeren
rm(list = ls())

golf <- read.csv2("golf.csv")

golf$sonderaus1 = factor(golf$sonderaus1)
levels(golf$sonderaus1) = c("ABS vorhanden", "ABS nicht vorhanden")

golf$sonderaus2 <- factor(golf$sonderaus2)
levels(golf$sonderaus2)= c("Schiebedach nicht vorhanden", "Schiebedach vorhanden")

fitgolf <- lm(formula = preis ~ alter + kilstand + tuev +sonderaus1 + sonderaus2,
              data = golf)
summary(fitgolf)

#Wenn das Auto um einen Monat älter ist und alles andere konstakt bleibt, sinkt der Preis um 0.038 Einheiten und der Einfluss ist 
#zu jedem üblichen Signifikanzniveau signifikant

#Wenn kein ABS vorhanden ist und alles andere konstant bleibt, sinkt der Preis 
# um 0.23 Einheiten und auch signifikant

plot(fitgolf)

#Residuals vs fitted
#Plot prüft Homoskedastizität (also die gleichmäßige Streuung der Residuen). 
#Interpretation hier: Sie fallen einigermaßen gleichmäßig um das die Nulllinie

#Q-Q Residuals: 
#Plot prüft die Normalverteilung der Residuen ( auf der x achse theoretische quantile der Normalverteilung, auf der y-achse 
#empirischen quantile meiner daten)(Also prüft ob die Daten aus einer Normalverteilung kommen)
#Wenn die Daten auf der Diagonale liegen ist alles gut, das tun sie

#Scale Location:
#Prüfung ob die Varianz der Residuen konstant ist um heteroskedastizität zu erkennen.
#Nicht auffällig hier. 

#Residuals vs. Leverage
#Beobachtungen sind problematisch wenn sie weit rechts liegen(also hoher leverage und gleichzeitig weit weg 
#von der 0 linie also, also ein hohes Residuum hat)
#Soll zeigen ob manche Beobachtungen problematisch einflussreich sind
#Hier: nein sind sie nicht
#Wenn cooks Distance nicht zu sehen ist, ist normalerweise alles gut 


rm(list = ls())

miet <-read.csv2("MieteMuenchen2015_PS2.csv")

head(miet)
summary(miet)

KQ_fit <- lm(badextra ~ mieteqm + flaeche, data = miet)
summary(KQ_fit)

#Interpretation: miet einem qm mehr und allem anderen gleich, steigt die wahrscheinlichkeit von badexta um 0.003 und der einfluss ist 
#signifikant. 

predict(KQ_fit, newdata = data.frame(mieteqm = 10, flaeche = 50))

#Der Wert ist 0.04 was 4% wahrscheinlichkeit für badextra bedeutet in diesem fall

plot(KQ_fit)

#REsiduals vs fitted zeigt keine gleichmäßige Streuung um null, Hinweis auf Hetereoskedastizität
#QQplot: keine Diagonale, also keine perfekte Normalverteilung
#Scale Location: Variierende Streuung deutet auf nicht gleichverteilte varianz hin
#Residuals vs. Leverage: Einzelne Beobachtung weit weg mit hohem Leverage
#allerdings keine Punkte über Cooks Distance.
rm(list=ls())

miet <- read.table("MieteMuenchen2015_PS4.raw", header = TRUE)
#erinnerung, dass table einen Header braucht

logit_fit <- glm(badextra ~ mieteqm + flaeche, family = binomial(link = "logit"), data = miet)
head(miet)

summary(logit_fit)

#Interpretation von log odds
#Wenn die Miete pro qm um eine Einheit steigt, steien die logodds für y /badextra
#um 0.11

# Die odds Ratios sind das eigentlich interessante für uns, sie ergeben sich
#durch exponentieren der Koeff. im logit

exp(logit_fit$coefficients)
#Die hierbei herauskommenden Werte sind das Verhältnis von der alten chance dass Badextra=1
#zu der neuen Chance dafür wenn ceteris paribus die Variable pro Koeff.
#um eine Einheit steigt

#Interpretation von Odds Ratios:
#Wenn die Miete pro qm um einen Euro steigt, steigen die Odds für 
#Badextra um den Faktor 1.126

#wir wollen eine bestimte wahrscheinlichkeit ausrechnen lassen, 
#nämlich die Wahrscheinlichkeit für ein Bad extra, 
#wenn mieteqm 11 Euro ist und wenn die Fläche 50 qm ist

#direkte Rechnung, also wir benutzen plogis(z) = 1/1+e^-z
#macht aus log_odds Wahrscheinlichkeiten

plogis(predict(logit_fit, newdata = data.frame(mieteqm = 11, flaeche = 50)))
#Eine Wahrscheinlichkeit von 0.07, also 7 Prozent, dass diese Wohnung ein Bad extra hat

plogis(predict(logit_fit, newdata = data.frame(mieteqm = 15, flaeche = 50)))

#Wenn die Fläche gleich bleibt, aber sich die mieteqm erheblich erhöht (auf 15)
#dann steigt die Wahrscheinlichkeit auf 10 Prozent

#Anderer Weg zum selben Ergebnis 

predict(logit_fit, newdata = data.frame(mieteqm = 11, flaeche = 50), type = "response")

predict(logit_fit, newdata = data.frame(mieteqm = 20, flaeche = 50), type = "response")

# um Änderungen von Wahrscheinlichkeiten zu sehen, brauchen wir marginale Effekte

library(mfx)
#Durchschnittliche marginale Effekte
logitmfx(badextra ~ mieteqm + flaeche, data = miet, atmean = FALSE)

#Interpretation marginale Effekte: Durchschnittlich erhöht sich die Wahrscheinlichkeit für ein zusätzliches Bad um
#1.15 Prozentpunkte wenn sich mieteqm um eine Einheit nach oben verändert

#Marginale Effekte am Durchschnitt

logitmfx(badextra ~ mieteqm + flaeche, data = miet, atmean = TRUE)

#Interpretation marginale Effekte am Durchschnitt: 
#Für eine durchschnittliche wohnung steigt die wahrscheinlichkeit um 1.09 Prozent 
#wenn sie mieteqm um eine Einheit nach oben ädert


#Marginaler Effekt am Median

library(marginaleffects)

slopes(logit_fit, newdata = "median", type = "response")

#INterpretation:
#Die Wahrscheinlichkeit ein zusätzliches BAd zu haben steigt für die median Wohnung um 1.06 Prozentpunkte 
#wenn die Mieteqm um eine Euro steigt.
