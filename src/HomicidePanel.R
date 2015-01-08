################################################
## Author: Eduardo Clark
## Title: Homicides and temperature in Mexico
## Date: January 2015
##############################################

#Read Homicide Data
Homicides <- read.csv("data/Homicidios.csv", stringsAsFactors=FALSE) #read homicide data 2004-2011
Homicides <- tbl_df(Homicides) #Convert into a dplyr df

#Create all days and metroareas
Days <- expand.grid(metro_area=unique(as.character(Homicides$metro_area)),
                    date_occur=seq(as.Date("2004-01-01"),
                                   as.Date("2011-12-31"), by="days"))
Days <- Days[is.na(Days$metro_area)==FALSE,]
Days$date_occur <- ymd(Days$date_occur)
Days$metro_area <- as.character(Days$metro_area)

##Create a panel by Metropolitan Area and Day
Panel <- Homicides %>% group_by(metro_area,date_occur) %>%
  summarise(Homicides=n()) %>% filter(nchar(date_occur)==10) %>%
  mutate(date_occur=ymd(date_occur)) %>% arrange(date_occur) 

##Join with day list to add days with 0 homicides
Panel <- left_join(Days, Panel,by=c("metro_area","date_occur"))
remove(Days, Homicides)

#Change NA to 0
Panel <- Panel %>% mutate(Homicides,Homicides= ifelse(is.na(Homicides)==TRUE,0, Homicides)) %>%
  arrange(date_occur, metro_area) %>% filter(date_occur>="2004-01-01") 

##Writeout 
write.csv(Panel,"data-out/HomicidesMetroAreaDay.csv", row.names=FALSE)
remove(Panel)

