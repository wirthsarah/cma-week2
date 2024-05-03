# Exercise A----
## Task 1----
library("readr")
library("sf")
library("ggplot2")

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056)

## Task 2----
difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))}

wildschwein_BE <- wildschwein_BE |>                         group_by(TierID) |>
  mutate(timelag = difftime_secs(lead(DatetimeUTC), DatetimeUTC))

wildschwein_BE_Info<-st_drop_geometry(wildschwein_BE)

unique(wildschwein_BE_Info$TierName)
# How many individuals were tracked? 3

wildschwein_BE_Info |> 
  group_by(TierName) |> 
  summarise(min=min(DatetimeUTC, na.rm = TRUE), max=max(DatetimeUTC, na.rm = TRUE), mean=mean(timelag, na.rm = TRUE), median=median(timelag, na.rm=TRUE),maxlag=max(timelag, na.rm = TRUE))  

# For how long were the individual tracked? Rosa: 2014-11-07 07:45:44 - 2015-06-29 23:45:11, Ruth: 2014-11-07 18:00:43 2015-07-27 09:45:15, Sabi:2014-08-22 21:00:12 2015-07-27 11:00:14
# Are there gaps? yes, probably several for all individuals
# Were all individuals tracked concurrently or sequentially? concurrently
# What is the temporal sampling interval between the locations? ca 903secs

## Task 3----
later <- lag(wildschwein_BE$geometry)
now <- wildschwein_BE$geometry

st_distance(later, now, by_element = TRUE)  # by_element must be set to TRUE

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE))}

wildschwein_BE$steplength <-distance_by_element(later,now)

## Task 4----
# getting rid of difftime Object type
wildschwein_BE$speed <-wildschwein_BE$steplength/wildschwein_BE$timelag

## Task 5----
wildschwein_sample <- wildschwein_BE |>
  filter(TierName == "Sabi") |> 
  head(100)

library(tmap)
tmap_mode("view")

tm_shape(wildschwein_sample) + 
  tm_dots()

wildschwein_sample2 <- wildschwein_BE |>
  filter(TierName == "Sabi") |> 
  slice_tail(n=100)#trying out the slice tools

tm_shape(wildschwein_sample2) + 
  tm_dots()

wildschwein_sample_line <- wildschwein_sample |> 
  # dissolve to a MULTIPOINT:
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_options(basemaps = "OpenStreetMap")

tm_shape(wildschwein_sample_line) +
  tm_lines() +
  tm_shape(wildschwein_sample) + 
  tm_dots()