################################################
## Author: Eduardo Clark
## Title: Homicides and temperature in Mexico
## Date: January 2015
##############################################

##Join Homicide and Weather Data
Homicides <- tbl_df(read.csv("data-out/HomicidesMetroAreaDay.csv", stringsAsFactors=FALSE))
Weather <- tbl_df(read.csv("data-out/WeatherPanel.csv",stringsAsFactors=FALSE))
names(Weather)[2] <- c("date_occur")
Homicides$date_occur <- substr(Homicides$date_occur,1,10)
Weather$date_occur <- substr(Weather$date_occur,1,10)
Panel <- merge(Homicides, Weather, by=c(1,2))
remove(Weather, Homicides)
Panel <- tbl_df(Panel)


##Add some controls
Panel$Year <- substr(Panel$date_occur, 1,4)
Panel$Month <- substr(Panel$date_occur,6,7)
Panel$date_occur <- ymd(Panel$date_occur)
Panel$DayOfTheWeek <- wday(Panel$date_occur,label = TRUE,abbr = TRUE)


#NA those non real values
Panel$TMedia <- round(ifelse(Panel$TMedia>55 | Panel$TMedia<(-20),NA,Panel$TMedia),1)
Panel$TMax <- round(ifelse(Panel$TMax>55 | Panel$TMax<(-20),NA,Panel$TMax),1)
Panel$TMin <- round(ifelse(Panel$TMin>55 | Panel$TMin<(-20),NA,Panel$TMin),1)

##Drop NA values 
Panel <- Panel[is.na(Panel$TMedia)==FALSE,]
Panel <- Panel[is.na(Panel$TMax)==FALSE,]
Panel <- Panel[is.na(Panel$TMin)==FALSE,]

##Lag Days 1-5
Lag1 <- function(i){ #one Day
  if(Panel[i-1,2] - Panel[i,2] == -1 & Panel[i-1,1] == Panel[i,1]){
  tmp <- Panel[i-1,4]}
  else{tmp <- NA}
  return(tmp)
}
Panel$TMediaLag1 <- c(NA,sapply(2:nrow(Panel),Lag1))
Panel$TMediaLag1 <- unlist(Panel$TMediaLag1)
Lag2 <- function(i){ #two Day
  if(Panel[i-2,2] - Panel[i,2] == -2 & Panel[i-2,1] == Panel[i,1]){
    tmp <- Panel[i-2,4]}
  else{tmp <- NA}
  return(tmp)
}
Panel$TMediaLag2 <- c(NA,NA,sapply(3:nrow(Panel),Lag2))
Panel$TMediaLag2 <- unlist(Panel$TMediaLag2)
Lag3 <- function(i){ #two Day
  if(Panel[i-3,2] - Panel[i,2] == -3 & Panel[i-3,1] == Panel[i,1]){
    tmp <- Panel[i-3,4]}
  else{tmp <- NA}
  return(tmp)
}
Panel$TMediaLag3 <- c(NA,NA,NA,sapply(4:nrow(Panel),Lag3))
Panel$TMediaLag3 <- unlist(Panel$TMediaLag3)
Lag4 <- function(i){ #two Day
  if(Panel[i-4,2] - Panel[i,2] == -4 & Panel[i-4,1] == Panel[i,1]){
    tmp <- Panel[i-4,4]}
  else{tmp <- NA}
  return(tmp)
}
Panel$TMediaLag4 <- c(NA,NA,NA,NA,sapply(5:nrow(Panel),Lag4))
Panel$TMediaLag4 <- unlist(Panel$TMediaLag4)
Lag5 <- function(i){ #two Day
  if(Panel[i-5,2] - Panel[i,2] == -5 & Panel[i-5,1] == Panel[i,1]){
    tmp <- Panel[i-5,4]}
  else{tmp <- NA}
  return(tmp)
}
Panel$TMediaLag5 <- c(NA,NA,NA,NA,NA,sapply(6:nrow(Panel),Lag5))
Panel$TMediaLag5 <- unlist(Panel$TMediaLag5)
remove(list=ls(pattern="Lag"))

## Create controls for days over 25C and for continued days with more than that temp
Panel$HighTempMean1Day <- ifelse(Panel$TMedia>=30,1,0)
Panel$HighTempMax1Day <- ifelse(Panel$TMax>=30,1,0)
Panel$HighTempMin1Day <- ifelse(Panel$TMin>=30,1,0)
Panel$HighTempMean2Day <- ifelse(Panel$TMedia>=30 & Panel$TMediaLag1>=30,1,0)
Panel$HighTempMean3Day <- ifelse(Panel$TMedia>=30 & Panel$TMediaLag1>=30 & Panel$TMediaLag2>=30,1,0)
Panel$HighTempMean4Day <- ifelse(Panel$TMedia>=30 & Panel$TMediaLag1>=30 & Panel$TMediaLag2>=30 & Panel$TMediaLag3>=30,1,0)
Panel$HighTempMean5Day <- ifelse(Panel$TMedia>=30 & Panel$TMediaLag1>=30 & Panel$TMediaLag2>=30 & Panel$TMediaLag3>=30 & Panel$TMediaLag4>=30,1,0)
Panel$HighTempMean6Day <- ifelse(Panel$TMedia>=30 & Panel$TMediaLag1>=30 & Panel$TMediaLag2>=30 & Panel$TMediaLag3>=30 & Panel$TMediaLag4>=30 & Panel$TMediaLag5>=30,1,0)


## Create controls for days over 30C and for continued days with more than that temp
Panel$HighTempMean1Day25 <- ifelse(Panel$TMedia>=25,1,0)
Panel$HighTempMax1Day25 <- ifelse(Panel$TMax>=25,1,0)
Panel$HighTempMin1Day25 <- ifelse(Panel$TMin>=25,1,0)
Panel$HighTempMean2Day25 <- ifelse(Panel$TMedia>=25 & Panel$TMediaLag1>=25,1,0)
Panel$HighTempMean3Day25 <- ifelse(Panel$TMedia>=25 & Panel$TMediaLag1>=25 & Panel$TMediaLag2>=25,1,0)
Panel$HighTempMean4Day25 <- ifelse(Panel$TMedia>=25 & Panel$TMediaLag1>=25 & Panel$TMediaLag2>=25 & Panel$TMediaLag3>=25,1,0)
Panel$HighTempMean5Day25 <- ifelse(Panel$TMedia>=25 & Panel$TMediaLag1>=25 & Panel$TMediaLag2>=25 & Panel$TMediaLag3>=25 & Panel$TMediaLag4>=25,1,0)
Panel$HighTempMean6Day25 <- ifelse(Panel$TMedia>=25 & Panel$TMediaLag1>=25 & Panel$TMediaLag2>=25 & Panel$TMediaLag3>=25 & Panel$TMediaLag4>=25 & Panel$TMediaLag5>=25,1,0)

##Out
write.csv(Panel, "data-out/WeatherHomicidePanel.csv", row.names=FALSE)
remove(Panel)
