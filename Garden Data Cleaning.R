library(plotly)
library(datasets)
library(lubridate)
library(tidyverse)
library(ggplot2)

#reading in files
garden1 <- read.csv("Garden_Data_425.csv")
garden2 <- read.csv("Garden_Data_2_425.csv")
garden1 <- as_tibble(garden1)
garden2 <- as_tibble(garden2)


#converting date and time information
garden1$Time <- parse_date_time(garden1$Time, "%B %d, %Y %I:%M%p")
garden2$DateTime <- parse_date_time(garden2$DateTime, "%B %d, %Y %I:%M%p")

#saving day and hour as separate column with hour as factor
garden1$Day <- day(garden1$Time)
garden1$Hour <- as.factor(hour(garden1$Time))
garden2$Day <- day(garden2$DateTime)
garden2$Hour <- as.factor(hour(garden2$DateTime))

#######Average Sensors by Day and Hour and consolidate to new rows
Stats <- function(sensor, data){
    data %>%
    group_by(Day, Hour) %>%
    summarize(mean({{sensor}}))
}


#getting mean of the hourly sensor readings
Soil.Temp <- Stats(Soil.Temp, garden1)
Light <- Stats(Light, garden1)
Soil.Moisture <- Stats(Soil.Moisture, garden2)
Humidity <- Stats(Humidity, garden2)
Air.Temp <- Stats(Air.Temp, garden2)

#join new columns
garden <- left_join(Soil.Temp, Light, by = c("Day", "Hour"))
garden <- left_join(garden, Soil.Moisture, by = c("Day", "Hour"))
garden <- left_join(garden, Humidity, by = c("Day", "Hour"))
garden <- left_join(garden, Air.Temp, by = c("Day", "Hour"))
glimpse(gar)
#rename columns
gar <- garden %>%
  rename(c("Soil.Temp.C" = 'mean(Soil.Temp)', "Light" = 'mean(Light)', "Soil.Moisture" = 'mean(Soil.Moisture)',
                "Humidity" = 'mean(Humidity)', "Air.Temp C" = 'mean(Air.Temp)'))

write.csv(garden, "clean_garden_project.csv")
################################################################

gar <- read.csv("clean_garden_project.csv")
gar <- as_tibble(gar)
glimpse(gar)
summary(gar)


###################################################
#light levels
##### 0-30 DARK
##### 30 - 349 CLOUDY
####  350 - 549 PARTLY SUNNY
###   550+ sUNNY
glimpse(gar)

#create and name categories for light levels
gar$Light_Levels = cut(gar$Light,c(-1,30.1,349.1,549.1, 1024.1))
levels(gar$Light_Levels) = c("Dark","Cloudy","Part.Sun", "Sunny")


########################################
#soil moisture levels
#### 540 - 930
#### 540 - 650 dry
#### 650 - 775 moist
#### 775 - 875 wet
#### 875 - 920 saturated



#create and name categories for soil moisture levels
gar$Soil_Moisture_Levels = cut(gar$Soil.Moisture,c(540,650,775,875,1000))
levels(gar$Soil_Moisture_Levels) = c("Dry", "Moist", "Wet","Saturated")

#create a week cateogry
gar$Week = cut(gar$Day, c(5.9,12.1,19.1,40.1))
levels(gar$Week) = c("Week 1", "Week 2", "Week 3")

glimpse(gar)
summary(gar)


write.csv(gar, "garden_project_final.csv")

