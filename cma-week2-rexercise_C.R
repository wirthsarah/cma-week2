# Exercise C----
library("jsonlite")
library("readr")
library("sf")
library("dplyr")
library("tmap")
library("lubridate")
library("stringr")
library("tidyr")

Sarah_Wirth_Data<-fromJSON("location-history.json")

head(Sarah_Wirth_Data)
str(Sarah_Wirth_Data)

##Extract coordinates----
Sarah_Wirth_Data$startplace_n <-substring(Sarah_Wirth_Data$activity$start,5, 13)
Sarah_Wirth_Data$startplace_e <-substring(Sarah_Wirth_Data$activity$start,15, 22)
Sarah_Wirth_Data$endplace_n <-substring(Sarah_Wirth_Data$activity$end,5, 13)
Sarah_Wirth_Data$endplace_e <-substring(Sarah_Wirth_Data$activity$end,15, 22)

Sarah_Wirth_Data <-Sarah_Wirth_Data[!(Sarah_Wirth_Data$startplace_n %in% NA),]#drop rows without coordinates

##Extract time ----
Sarah_Wirth_Data$tz <-str_sub(Sarah_Wirth_Data$endTime, -5,-1)#get timezones, as the data has different timezones (+01, +02) for reasons
Sarah_Wirth_Data <-Sarah_Wirth_Data[!(Sarah_Wirth_Data$tz %in% ".000Z"),]#drop visits, which coincidentally have the "timezone" .000Z

#i have now spent way to much time on trying to format the dates to two timezones and failed every time. 
#therefore i will treat it as if its the same from now on.

Sarah_Wirth_Data$startTime <- as.POSIXct(Sarah_Wirth_Data$startTime, format="%Y-%m-%dT%H:%M:%OS")#Format start and endtime while losing miliseconds

Sarah_Wirth_Data$endTime <- as.POSIXct(Sarah_Wirth_Data$endTime, format="%Y-%m-%dT%H:%M:%OS")#Format start and endtime while losing miliseconds


Sarah_Wirth_Data <-Sarah_Wirth_Data |> 
  select(endTime,startTime,startplace_n,startplace_e,endplace_n,endplace_e, tz)#drop every row which isn't a time or a coordinate or tz


Sarah_Wirth_Start <-Sarah_Wirth_Data |> 
  select(startTime,startplace_n,startplace_e)
colnames(Sarah_Wirth_Start) <- c("Datetime","N", "E")

Sarah_Wirth_End <-Sarah_Wirth_Data |> 
  select(endTime,endplace_n,endplace_e)
colnames(Sarah_Wirth_End) <- c("Datetime","N", "E")

Sarah_Wirth_fixed <-rbind(Sarah_Wirth_Start,Sarah_Wirth_End)

Sarah_Wirth_fixed <- Sarah_Wirth_fixed |> 
  arrange(Datetime)

row.names(Sarah_Wirth_fixed)<-NULL#fix row names

##Spatial Objects ----
Sarah_Wirth_sf4326 <- st_as_sf(Sarah_Wirth_fixed, coords = c("E", "N"), crs = 4326)
Sarah_Wirth_sf<-st_transform(Sarah_Wirth_sf4326, 2056)

tmap_mode("view")

Sarah_Wirth_sf_line <- Sarah_Wirth_sf |> 
  # dissolve to a MULTIPOINT:
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_options(basemaps = "OpenStreetMap")

tm_shape(Sarah_Wirth_sf_line)+
  tm_lines() +
  tm_shape(Sarah_Wirth_sf) + 
  tm_dots()

#text