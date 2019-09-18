# AUFGABENBLATT 2
# Cordula Eggerth (00750881)

# Verwendete Literaturquellen:
# Folien und R-Codes zu den bisher vorgetragenen Kapiteln aus UK Erweiterungen des linearen Modells (Prof. Marcus Hudec).
#	Model Selection Criteria and Predictive Power of Regression (Github) - PRESS (Tom Hopper, 2018); https://gist.github.com/tomhopper/8c204d978c4a0cbcb8c0; Zugriff am 12.04.2019.
# PRESS Diagnostic (2016); https://stats.stackexchange.com/questions/248603/how-can-one-compute-the-press-diagnostic; Zugriff am 12.04.2019.
# Coding for Categorical Variables in Regression Models; UCLA Institute for Digital Research & Education (2018); https://stats.idre.ucla.edu/r/modules/coding-for-categorical-variables-in-regression-models/; Zugriff am 12.04.2019.

rm(list=ls())

install.packages("qpcR")
installpackages("DAAG")
library(car)
library(qpcR)
library(MASS)
library(DAAG)

path <- "C:/Users/Coala/Desktop/A2_ERWEIT"

#***********************************************************************************************
# AUFGABE 1
#***********************************************************************************************
# 1. Fuehren Sie mit dem Datensatz realestate.txt eine regressionsanalytische 
#    Modellierung durch. Evaluieren Sie die erzielte Vorhersage-Guete mittels 
#    des PRESS-criterion (prediction sum of squares - siehe Skriptum).

# daten einlesen
realestate_data <- read.table(file="C:/Users/Coala/Desktop/A2_ERWEIT/realestate.txt",
                              header=FALSE, stringsAsFactors=FALSE)
head(realestate_data, n=20)
variablenames <- c("SalesPrice","SqFeet","Beds","Baths","AirCond","Garage","Pool",
                   "Year","Quality","Style","Lot","Highway")
realestate_data <- data.frame(realestate_data[2:nrow(realestate_data), ])
colnames(realestate_data) <- variablenames
for(i in 1:ncol(realestate_data)){
  realestate_data[ ,i] <- as.numeric(realestate_data[ ,i])
}

# deskriptive statistiken
nrow(realestate_data)
plot(realestate_data$SalesPrice) # abhaengige variable
plot(realestate_data$SqFeet, realestate_data$SalesPrice)
plot(as.factor(realestate_data$Beds), realestate_data$SalesPrice,
     xlab="Beds", ylab="SalesPrice")
plot(realestate_data$Year, realestate_data$SalesPrice)
plot(as.factor(realestate_data$Quality), realestate_data$SalesPrice,
     xlab="Quality", ylab="SalesPrice")
plot(realestate_data$Lot, realestate_data$SalesPrice)
summary(realestate_data)

# erklaere SalesPrice (V1) durch (kombi der) verbleibenden variablen
attach(realestate_data)

#-------------------------------------------------------------------------------
# MODELL 1
#-------------------------------------------------------------------------------
# multiple lineare regression (modell mit allen regressoren, additiv)
# (i.e. variablen in der anwesenheit anderer variablen testen)
lm1 <- lm(SalesPrice ~ SqFeet + Beds + Baths + AirCond + Garage + Pool
          + Year + Quality + Style + Lot + Highway)
summary(lm1)

par(mfrow=c(2,2))
plot(lm1) # diagnostic plots

par(mfrow=c(1,1))

# anova 
# (beitrag der variablen sequenziell beurteilen - 
#  nachteil: p-werte abhaengig von der reihenfolge der variablen)
anova_lm1 <- anova(lm1)

# Anova 
# (beitrag der variablen unabhaengig von der reihenfolge beurteilen)
# (nur falls die variablen alle orthogonal zu einander sind, dann
#  ergeben anova() und Anova() dasselbe)
Anova_lm1 <- Anova(lm1)

# Method 1: PRESS (see sources 1-3, Hudec chapter "Multiple Regression" slide 36)
#           simplified formula for linear models
PRESScriterion <- function(linmod) {
  predictiveResid <- residuals(linmod)/(1 - lm.influence(linmod)$hat)
  sum(predictiveResid^2) # PRESS formula form slide 36
}

PRESS_lm1_method1 <- PRESScriterion(lm1) # PRESS for MODELL 1

# Method 2: PRESS (using package qpcR: 
# https://www.rdocumentation.org/packages/qpcR/versions/1.4-1/topics/PRESS)
PRESS_lm1_method2 <- PRESS(lm1)$stat

# beide methoden ergeben dasselbe PRESS criterion, daher ueberpruefung in ordnung

#-------------------------------------------------------------------------------
# MODELL 2
#-------------------------------------------------------------------------------
# multiple lineare regression (modell mit allen regressoren, mit interaktion)
lm2 <- lm(SalesPrice ~ SqFeet + Beds + Baths + AirCond + Garage + Pool
          + Year + Quality + Style + Lot + Highway + SqFeet:Beds + 
            SqFeet:Year + SqFeet:Pool + Quality:Style + Beds:Pool)
summary(lm2)

# PRESS
PRESS_lm2_method1 <- PRESScriterion(lm2)  
PRESS_lm2_method2 <- PRESS(lm2)$stat  

#-------------------------------------------------------------------------------
# MODELL 3
#-------------------------------------------------------------------------------
# multiple lineare regression (modell mit weniger variablen, mit interaktion)
lm3 <- lm(SalesPrice ~ SqFeet + Beds + Year + Quality + Style + Lot + 
          SqFeet:Beds + SqFeet:Year + Quality:Style)
summary(lm3)

# PRESS
PRESS_lm3_method1 <- PRESScriterion(lm3)  
PRESS_lm3_method2 <- PRESS(lm3)$stat 

#-------------------------------------------------------------------------------
# MODELL 4
#-------------------------------------------------------------------------------
# multiple lineare regression (modell mit weniger variablen, mit interaktion)
lm4 <- lm(SalesPrice ~ SqFeet + Beds + Year + Quality + Style + SqFeet:Beds
          + SqFeet:Year)
summary(lm4)

# PRESS
PRESS_lm4_method1 <- PRESScriterion(lm4)  
PRESS_lm4_method2 <- PRESS(lm4)$stat 

# CONCLUSIO: modell 3 ist hier das beste, da es den kleinsten PRESS-wert, 
#            und somit die hoechste vorhersageguete der betrachteten 4
#            modelle hat


#***********************************************************************************************
# AUFGABE 2
#***********************************************************************************************
# 2. Der Datensatz crabs aus der library (MASS) enthält die Daten von 
#    50 weiblichen und männlichen Tieren. Untersuche den linearen Zusammenhang 
#    zwischen BD (abhängige Variable und den anderen Variablen). 
#    Modelliere den Geschlechtseffekt mittels Indikatorvariablen und diskutiere 
#    die Ergebnisse.

# deskriptive statistiken
attach(crabs)
head(crabs,n=20)
nrow(crabs)
summary(crabs)

plot(crabs$BD) # abhaengige variable
plot(crabs$sp, crabs$BD, 
     xlab="sp", ylab="BD")
plot(crabs$sex, crabs$BD,
     xlab="sex", ylab="BD")
plot(crabs$FL, crabs$BD)
plot(crabs$RW, crabs$BD)
plot(crabs$CW, crabs$BD)

# MODELL 1
# multiple lineare regression (modell mit allen regressoren, additiv)
lm_crabs1 <- lm(BD ~ sp + sex + index + FL + RW + CL + CW)
summary(lm_crabs1)

par(mfrow=c(2,2))
plot(lm_crabs1) # diagnostic plots
crPlots(lm_crabs1)
# conclusion: linearer zshg sichtbar, da in den plots "residulas vs.
#             fitted" und "scale-location" die residuen bzw. stand-
#             ardisierten residuen zufaellig um die nulllinie liegen.
#             auch in den C+R-Plots (Component+Residual-Plots) wird
#             der lineare zshg ersichtlich.

par(mfrow=c(1,1))

# MODELL mit INDIKATORVARIABLEN fuer geschlecht 
# mittels contrast treatment: 
crabs$sex.f <- factor(crabs$sex)
is.factor(crabs$sex.f)

crabs <- within(crabs, {
         sex.ct <- C(sex.f, treatment)
         print(attributes(sex.ct))
         })

summary(lm(BD ~ sex.ct, data=crabs))

# mittels helmert coding:
crabs <- within(crabs, {
         sex.ch <- C(sex.f, helmert)
         print(attributes(sex.ch))
         })

summary(lm(BD ~ sex.ch, data=crabs))

# mittels contrasts(): 
contrasts(crabs$sex.f) <- contr.treatment(2, base=1)
summary(lm(BD ~ sex.f, data=crabs))

# plots der regressionsgeraden gesamt und
# unter beachtung des geschlechts:

colors <- c("deeppink1", "deepskyblue")

# erklaerung von BD durch RW:
plot(BD ~ RW, main="Erkläre BD durch RW", col=colors[sex])
# gesamte regressionsgerade:
abline(lm(BD ~ RW), col="lightgoldenrod3", lwd=2)
# regressionsgerade fuer M (maennlich):
abline(lm(BD ~ RW, subset=(sex=="M")), col="deepskyblue", lwd=2)
# regressionsgerade fuer F (weiblich):
abline(lm(BD ~ RW, subset=(sex=="F")), col="deeppink1", lwd=2)
# legend
legend("topleft", legend=c("gesamt", "M (male)", "F (female)"), 
      col=c("lightgoldenrod3", "deepskyblue", "deeppink1"), lwd=3)

# erklaerung von BD durch FL:
plot(BD ~ FL, main="Erkläre BD durch FL", col=colors[sex])
# gesamte regressionsgerade:
abline(lm(BD ~ FL), col="lightgoldenrod3", lwd=2)
# regressionsgerade fuer M (maennlich):
abline(lm(BD ~ FL, subset=(sex=="M")), col="deepskyblue", lwd=2)
# regressionsgerade fuer F (weiblich):
abline(lm(BD ~ FL, subset=(sex=="F")), col="deeppink1", lwd=2)
# legend
legend("topleft", legend=c("gesamt", "M (male)", "F (female)"), 
       col=c("lightgoldenrod3", "deepskyblue", "deeppink1"), lwd=3)

# erklaerung von BD durch CL:
plot(BD ~ CL, main="Erkläre BD durch CL", col=colors[sex])
# gesamte regressionsgerade:
abline(lm(BD ~ CL), col="lightgoldenrod3", lwd=2)
# regressionsgerade fuer M (maennlich):
abline(lm(BD ~ CL, subset=(sex=="M")), col="deepskyblue", lwd=2)
# regressionsgerade fuer F (weiblich):
abline(lm(BD ~ CL, subset=(sex=="F")), col="deeppink1", lwd=2)
# legend
legend("topleft", legend=c("gesamt", "M (male)", "F (female)"), 
       col=c("lightgoldenrod3", "deepskyblue", "deeppink1"), lwd=3)

# erklaerung von BD durch CW:
plot(BD ~ CW, main="Erkläre BD durch CW", col=colors[sex])
# gesamte regressionsgerade:
abline(lm(BD ~ CW), col="lightgoldenrod3", lwd=2)
# regressionsgerade fuer M (maennlich):
abline(lm(BD ~ CW, subset=(sex=="M")), col="deepskyblue", lwd=2)
# regressionsgerade fuer F (weiblich):
abline(lm(BD ~ CW, subset=(sex=="F")), col="deeppink1", lwd=2)
# legend
legend("topleft", legend=c("gesamt", "M (male)", "F (female)"), 
       col=c("lightgoldenrod3", "deepskyblue", "deeppink1"), lwd=3)


#***********************************************************************************************
# AUFGABE 3
#***********************************************************************************************
# 3. Analysiere den Datensatz leaftemp aus der library (DAAG). Untersuche den 
#    linearen Zusammenhang zwischen tempDiff (abhängige Variable und den anderen 
#    Variablen). Modelliere den Einfluss von CO2level mittels Indikatorvariablen 
#    sowohl mit contrast.treatment als auch mit contrast.sum und diskutiere die 
#    Ergebnisse.  

# deskriptive statistiken
attach(leaftemp)
head(leaftemp,n=20)
nrow(leaftemp)
summary(leaftemp)

plot(leaftemp$tempDiff) # abhaengige variable
plot(leaftemp$vapPress, leaftemp$tempDiff)
plot(leaftemp$CO2level, leaftemp$tempDiff,
     xlab="CO2level", ylab="tempDiff")
plot(leaftemp$BtempDiff, leaftemp$tempDiff)

# MODELL 1
# multiple lineare regression (modell mit allen regressoren, additiv)
lm_leaf1 <- lm(tempDiff ~ vapPress + CO2level + BtempDiff)
summary(lm_leaf1)

par(mfrow=c(2,2))
plot(lm_leaf1) # diagnostic plots
crPlots(lm_leaf1)
# conclusion: linearer zshg sichtbar, da in den plots "residuals vs.
#             fitted" und "scale-location" die residuen bzw. stand-
#             ardisierten residuen zufaellig um die nulllinie liegen.
#             auch in den C+R-Plots (Component+Residual-Plots) wird
#             der lineare zshg ersichtlich.

par(mfrow=c(1,1))

# plots der regressionsgeraden gesamt und
# unter beachtung des CO2level:

colors <- c("green", "orange", "red3")

# erklaerung von tempDiff durch vapPress:
plot(tempDiff ~ vapPress, col=colors[leaftemp$CO2level],
     main="Erkläre tempDiff durch vapPress")
# gesamte regressionsgerade:
abline(lm(tempDiff ~ vapPress), col="lightgoldenrod3", lwd=2)
# regressionsgerade fuer low:
abline(lm(tempDiff ~ vapPress, subset=(CO2level=="low")), 
       col="green", lwd=2)
# regressionsgerade fuer medium:
abline(lm(tempDiff ~ vapPress, subset=(CO2level=="medium")), 
       col="orange", lwd=2)
# regressionsgerade fuer high:
abline(lm(tempDiff ~ vapPress, subset=(CO2level=="high")), 
       col="red3", lwd=2)
# legend
legend("topright", legend=c("gesamt", "low", "medium", "high"), 
       col=c("lightgoldenrod3", "green", "orange", "red3"), lwd=3)

# erklaerung von tempDiff durch BtempDiff:
plot(tempDiff ~ BtempDiff, main="Erkläre tempDiff durch BtempDiff", 
     col=colors[CO2level])
# gesamte regressionsgerade:
abline(lm(tempDiff ~ BtempDiff), col="lightgoldenrod3", lwd=2)
# regressionsgerade fuer low:
abline(lm(tempDiff ~ BtempDiff, subset=(CO2level=="low")), 
       col="green", lwd=2)
# regressionsgerade fuer medium:
abline(lm(tempDiff ~ BtempDiff, subset=(CO2level=="medium")), 
       col="orange", lwd=2)
# regressionsgerade fuer high:
abline(lm(tempDiff ~ BtempDiff, subset=(CO2level=="high")), 
       col="red3", lwd=2)
# legend
legend("topleft", legend=c("gesamt", "low", "medium", "high"), 
       col=c("lightgoldenrod3", "green", "orange", "red3"), lwd=3)


# MODELL mit INDIKATORVARIABLEN fuer geschlecht 
# mittels contrast treatment: 
leaftemp$co2.f <- factor(leaftemp$CO2level)
is.factor(leaftemp$co2.f)

leaftemp <- within(leaftemp, {
  co2.ct <- C(co2.f, treatment)
  print(attributes(co2.ct))
})

summary(lm(tempDiff ~ vapPress + co2.f + BtempDiff, data=leaftemp))

# weitere modelle mit indikatorvariablen:
  # additiv (ohne interaktion)
model.matrix(tempDiff ~ vapPress + BtempDiff + CO2level)
  # mit interaktion
model.matrix(tempDiff ~ vapPress * CO2level * BtempDiff)

# mittels contrast.treatment:
# (i.e. fitten auf non-treatment gruppe als baseline, 
# und hinzugeben des gruppenspezifischen effekts als 
# differenz zur baseline)
options(contrasts=c("contr.treatment", "contr.poly"))
contrasts(CO2level) # baseline hier: "low"
  # contrast.treatment bei additivem modell:
summary(res_treat_1 <- lm(tempDiff ~ vapPress + BtempDiff + CO2level))
  # contrast.treatment bei modell mit interaktion:
summary(res_treat_2 <- lm(tempDiff ~ vapPress * CO2level * BtempDiff))

par(mfrow=c(2,2))
plot(res_treat_1)
plot(res_treat_2)
par(mfrow=c(1,1))

press_res_treat_1 <- PRESScriterion(res_treat_1)
press_res_treat_2 <- PRESScriterion(res_treat_2)
# vorhersageguete von modell res_treat_1 ist besser laut PRESS

# mittels contrast.sum:
# (i.e. fitten einer "durchschnittlichen" gerade, und 
# abbildung des gruppenspezifischen effekts als 
# abweichung vom "durchschnitt")
options(contrasts=c("contr.sum", "contr.poly"))
contrasts(CO2level)  
# contrast.sum bei additivem modell:
summary(res_sum_1 <- lm(tempDiff ~ vapPress + BtempDiff + CO2level))
# contrast.sum bei modell mit interaktion:
summary(res_sum_2 <- lm(tempDiff ~ vapPress * CO2level * BtempDiff))

par(mfrow=c(2,2))
plot(res_sum_1)
plot(res_sum_2)
par(mfrow=c(1,1))

press_res_sum_1 <- PRESScriterion(res_sum_1)
press_res_sum_2 <- PRESScriterion(res_sum_2)

# vorhersageguete von modell res_treat_1 ist besser laut PRESS


