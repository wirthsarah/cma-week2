# Exercise B----
library("readr")
library("sf")
library("dplyr")
library("tmap")

difftime_secs <- function(x, y){
  as.numeric(difftime(x, y, units = "secs"))
}

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

caro <- read_delim("caro60.csv", ",") |>
  st_as_sf(coords = c("E","N"), crs = 2056) |> 
  select(DatetimeUTC)

## Task 1----
caro <- caro |>                        
  mutate(timelag = difftime_secs(lead(DatetimeUTC), lag(DatetimeUTC)))

later <- lag(caro$geometry)
now <- lead(caro$geometry)

caro$steplength <-distance_by_element(later,now)

caro$speed<-caro$steplength/caro$timelag
head(caro)

## Task 2----
caro <- caro |>                        
  mutate(timelag2 = difftime_secs(lead(DatetimeUTC, n=2), lag(DatetimeUTC, n=2)))

later <- lag(caro$geometry, n=2)
now <- lead(caro$geometry, n=2)

caro$steplength2 <-distance_by_element(later,now)

caro$speed2<-caro$steplength2/caro$timelag2
caro |> 
  # drop geometry and select only specific columns
  # to display relevant data only
  st_drop_geometry() |> 
  select(timelag2, steplength2, speed2) |> 
  head()

## Task 3----
caro <- caro |>                        
  mutate(timelag3 = difftime_secs(lead(DatetimeUTC, n=4), lag(DatetimeUTC, n=4)))

later <- lag(caro$geometry, n=4)
now <- lead(caro$geometry, n=4)

caro$steplength3 <-distance_by_element(later,now)

caro$speed3<-caro$steplength3/caro$timelag3

caro |> 
  # drop geometry and select only specific columns
  # to display relevant data only
  st_drop_geometry() |> 
  select(timelag3, steplength3, speed3) |> 
  head()

## Task 4----
caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

library("ggplot2")

ggplot(caro, aes(y = speed)) + 
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)#d.

library(tidyr)

# before pivoting, let's simplify our data.frame
caro2 <- caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

caro_long <- caro2 |> 
  pivot_longer(c(speed, speed2, speed3))

head(caro_long)

ggplot(caro_long, aes(name, value)) +
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)

#A steady decrease in median speed as the temporal analysis scale increases; yes
#A decrease in the overall variance in speed as the temporal scale increases; yes
#Lower minimum values at the shortest temporal scales; as in lower boxes :yes

