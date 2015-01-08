################################################
## Author: Eduardo Clark
## Title: Homicides and temperature in Mexico
## Date: January 2015
##############################################

##Read Datasets
Temp <- read.csv("data/Temperaturas.csv",stringsAsFactors=FALSE)
Temp <- tbl_df(Temp)
Rain <- read.csv("data/Precipitacion.csv",stringsAsFactors=FALSE)
Rain <- tbl_df(Rain)

#Extract Weather Stations
Stations <- unique(rbind(Rain[,3:4], Temp[,6:7]))
Stations$ID <- 1:nrow(Stations)

##Match each station to a municipality
Muns <- readOGR("data/Municipios","municipal",stringsAsFactors = FALSE)
MunCode <- unique(Muns$CVEGEO)
InWhichMun <- function(Municipality){
Muns1 <- Muns[Muns$CVEGEO==Municipality,]
Muns1 <- fortify(Muns1)
Muns2 <- point.in.polygon(Stations$Longitud, Stations$Latitud, Muns1$long, Muns1$lat)
Muns2 <- gsub(1,Municipality,Muns2)
Muns2 <- data.frame(Stations=Stations, Mun=Muns2)
Muns2 <- Muns2[Muns2$Mun!=0,]
return(Muns2)
}
Stations2 <- lapply(MunCode,InWhichMun)
Stations2 <- ldply(Stations2,data.frame)
write.csv(Stations2,"data-out/EstacionMunicipio.csv", row.names=FALSE)
remove(Stations,InWhichMun, MunCode, Muns)
Stations2 <- tbl_df(Stations2)
names(Stations2)[1:2] <- c("Longitud","Latitud")

#Subset municipalities in metropolitan areas
Metro <- unique(read.csv("data/Homicidios.csv",stringsAsFactors=FALSE)[,52:53])
Metro$fips <- formatC(Metro$fips,width = 5,flag = 0)
Metro <- Metro[is.na(Metro$metro_area)==FALSE,]
Metro$Keep <- 1
Stations2 <- merge(Metro,Stations2, by.x=1,by.y=4)
names(Stations2)[4:5] <- c("Longitud","Latitud")

###################Rain####################################
##Add municipality to Rain and Temp DF
Rain <- left_join(Stations2, Rain, by = c("Latitud","Longitud"))
Rain <- Rain %>% mutate(Fecha=mdy_hms(Fecha)) %>% filter(Fecha >= "2004-01-01 12:00:00 UTC") %>%
  group_by(metro_area, Fecha) %>% summarise(Precipitacion=mean(Precipitacion,na.rm = TRUE))

###################Temperature#################################
Temp <- left_join(Stations2, Temp, by = c("Latitud","Longitud"))
Temp <- tbl_df(Temp)
Temp <- Temp %>% mutate(Fecha=mdy_hms(Fecha)) %>% filter(Fecha >= "2004-01-01 12:00:00 UTC") %>%
  group_by(metro_area, Fecha) %>% summarise(TMedia=mean(TMedia,na.rm = TRUE), TMax=max(TMax,na.rm = TRUE),
                                            TMin = min(TMin,na.rm = TRUE))
remove(Metro, Stations2)


##Join Rain and Temp
Weather <- merge(Temp, Rain, by=1:2,all=TRUE)
write.csv(Weather,"data-out/WeatherPanel.csv",row.names=FALSE)
remove(Rain, Temp, Weather)

remove(Muns)
5253

