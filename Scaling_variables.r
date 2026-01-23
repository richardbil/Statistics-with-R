miet <- read.csv2("MieteMuenchen2015_PS2.csv")
head(miet)
summary(miet)
miet$alter <- 2015 - miet$bjahr
head(miet)
miet$alter2 = 2015 - miet$bjahr
head(miet)
str(miet)
levels(miet)
miet$badextra <- factor(miet$badextra)
levels(miet$badextra) <- list(keins=0, vorschmanden = 1)
str(miet)
miet$zh <- factor(miet$zh)
levels(miet$zh) <- list(nein = 0, ja = 1)
miet$kueche <- factor(miet$kueche)
levels(miet$kueche) <- list(hatkene = 0, hatene = 1)
miet$lage <- factor(miet$lage)
levels(miet$lage) = list(netnice = 0, okay = 1, nice = 2)

fit <- lm(formula = mieteqm ~ flaeche + alter + badextra + lage, data = miet)
summary(fit)


#Prinz. Modell für Interpretation metrischer variablen: 
#wenn a ~ b + c + d
#wenn sich b um ene Einheit steigt und c und d konstant gehalten werden,
#ändert sich a um den coeffizienten der bei Estimate steht.
#Beispiel wenn in diesem Fall flaeche um eine einheit steigt, sinkt die 
#vaiable mieteqm um 0,01 einheiten, wenn alles andere gleich bleibt 
#macht sinn wegen geringen quadratmeterpreis bei mehr quadratmeter
#Test

miet$gesamtmiete <- miet$mieteqm * miet$flaeche

fit2 <- lm(formula = gesamtmiete ~ flaeche + alter + badextra + lage, data = miet)
summary(fit2)

# Test mit neuem Modell, jetzt wenn flaeche um eine Einheit steigt, steigt gesamtmiete
# um 10.02 Einheiten wenn alles andere konstant bleibt. Also Interpretation und Test davor macht Sinn. 
#
#Prinzipielles Muster für Interpretation von Dummyvariablen (0 oder 1 für Merkmalsausprägung, also bei 
#lagegut, sehr gut oder schlecht für eine schlechte Wohnung 0,0,1)
#wenn a ~ b + c + d
#wenn c 1 ist und b und d konstant bleiben, dann steigt durchschnittlich
# a um dass was bei estimate steht (den coefficient). in diesem Fall, fit1, 
#wenn lagenice ist, ist die wohnung im quadratmeterpreis im durchschnitt um 1.64 Einheiten 
#höher als wenn die Lage okay ist (wenn alles andere konstant ist)


install.packages("car")
library(car)
linearHypothesis(fit, "2*lageokay = lagenice")

# beispiel interpretation: linearHypothesis nimmt zwei Modelle und testet ob das erste oder das zweite besser ist. 
# model 1: restricted
# model 2: komplett wie zuvor
#Nullhypothese ist dass die restriktionen genau so gut wie das andere modell
#p wert groß -> restirktionen sind nicht schlechter als das andere 
#pwert klein -> restriktionen sind schlechter und h0 wird verworfen

#Die Nullhypothese besagt, dass das eingeschränkte Modell korrekt spezifiziert ist und das uneingeschränkte Modell keinen signifikanten Zusatznutzen bringt.
#p wert bei PR(>F) ist 0.0001465, das ist extrem klein, und bei üblichen niveau von 0.05 wird die H0 verworfen, das heißt der test ist nicht besser

linearHypothesis(fit, c("flaeche=0", "alter=0", "badextravorschmanden=0"))

#selbe inspo wie davor, model1 restricted und model2 nicht, 
#p wert extrem klein, also kann H0 verworfen werden, also ist das modell nicht besser