################################################
## Author: Eduardo Clark
## Title: Homicides and temperature in Mexico
## Date: January 2015
##############################################

#Load Data
Panel <- read.csv("data-out/WeatherHomicidePanel.csv", stringsAsFactors=FALSE)

##Test for overdispersion to choose between Poisson or NegBin
S1 <- glm.nb(Homicides ~ TMedia + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area),  data = Panel)## NegBin
S1.1 <- update(S1, . ~ . - TMedia)
anova(S1, S1.1)
S1B <- glm(Homicides ~ TMedia + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
pchisq(2 * (logLik(S1) - logLik(S1B)), df = 1, lower.tail = FALSE)

##Modeling Simpĺe Modeling with Mean, Max and Min temperatuares
S1 <- glm.nb(Homicides ~ TMedia + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area),  data = Panel)## NegBin
S2 <- glm(Homicides ~ TMax + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
S3 <- glm(Homicides ~ TMin + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson

##Modeling with Mean temperatures and a lag for 1,2 and 3 Days
L1 <- glm(Homicides ~ TMedia + TMediaLag1 + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
L2 <- glm(Homicides ~ TMedia + TMediaLag1 + TMediaLag2 + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
L3 <- glm(Homicides ~ TMedia + TMediaLag1 + TMediaLag2 + TMediaLag3 + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson

##Models with temperatures above 30C, aditionally with temperatures above treshold for 1,2,3,4 and 5 days
T1 <- glm(Homicides ~ as.factor(Panel$HighTempMean1Day) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T2 <- glm(Homicides ~ as.factor(Panel$HighTempMean2Day) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T3 <- glm(Homicides ~ as.factor(Panel$HighTempMean3Day) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T4 <- glm(Homicides ~ as.factor(Panel$HighTempMean4Day) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T5 <- glm(Homicides ~ as.factor(Panel$HighTempMean5Day) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T6 <- glm(Homicides ~ as.factor(Panel$HighTempMean6Day) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson

##Models with temperatures above 25C, aditionally with temperatures above treshold for 1,2,3,4 and 5 days
T1B <- glm(Homicides ~ as.factor(Panel$HighTempMean1Day25) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T2B <- glm(Homicides ~ as.factor(Panel$HighTempMean2Day25) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T3B <- glm(Homicides ~ as.factor(Panel$HighTempMean3Day25) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T4B <- glm(Homicides ~ as.factor(Panel$HighTempMean4Day25) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T5B <- glm(Homicides ~ as.factor(Panel$HighTempMean5Day25) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson
T6B <- glm(Homicides ~ as.factor(Panel$HighTempMean6Day25) + as.factor(Year) + as.factor(Month) + DayOfTheWeek + as.factor(metro_area), family = "poisson", data = Panel)## Poisson


#Lets do case studies
vm2 <- glm(Homicides ~ TMedia + as.factor(Year) + as.factor(Month) + DayOfTheWeek, family = "poisson", data = Panel[Panel$metro_area=="Mazatlán, Sin",])## Poisson
vm1 <- glm(Homicides ~ as.factor(HighTempMean) + as.factor(Year) + as.factor(Month) + DayOfTheWeek , family = "poisson", data = Panel[Panel$metro_area=="Mazatlán, Sin",])## Poisson


screenreg(list(T1,T2,T3,T4,T5,T6))
