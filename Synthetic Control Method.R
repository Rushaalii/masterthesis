library(raster)
library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(readr)
library(data.table)
library(Synth)
library(tidyr)
library(knitr)
library(kableExtra)
library(xtable)
library(Synth)
library(openxlsx)


# Remove all objects from the workspace
rm(list = ls()) 

setwd("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs")
# Reading the compressed CSV file directly into R without explicitly unzipping

synth_data2 <- read_csv("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/50Synthetic Control.csv.gz")

#Add new variables
synth_data2 <- synth_data2 %>%
  mutate(married = if_else(MARST %in% c(1, 2), 1, 0))


geographicdata2012 <- read.xlsx("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/New York City/2012-2021 PUMAS Cities.xlsx", sheet = 1)
geographicdata2012$PUMA <- sub("^0", "", geographicdata2012$PUMA)
geographicdata2005 <- read.xlsx("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/New York City/2005-2011 PUMAS Citites.xlsx", sheet = 1)
geographicdata2005$PUMA <- sub("^0", "", geographicdata2005$PUMA)

geographicdata2012$X13 <- NULL

#Geograpical Areas (PUMAS)
pumas2005nyc <- geographicdata2005$PUMA[geographicdata2005$Place.Name == "New York city"]
pumas2012nyc <- geographicdata2012$PUMA[geographicdata2012$Place.Name == "New York city"]

#pumas2005largecities <- geographicdata2005$PUMA[geographicdata2005$Percent.PUMA.Population > 0.75]
#pumas2012largecities <- geographicdata2012$PUMA[geographicdata2012$Percent.PUMA.Population > 0.75]

#pumas2005rest_of_us <- geographicdata2005$PUMA[geographicdata2005$Place.Name != "New York city"]
#pumas2012rest_of_us <- geographicdata2005$PUMA[geographicdata2012$Place.Name != "New York city"]

donorpoolcities <- c("New York city", "Los Angeles city", "Chicago city", "Houston city", "Philadelphia city", "Phoenix city", "San Antonio city", "San Diego city", "Dallas city", "San Jose city", "Indianapolis city (balance)", "San Francisco city", "Columbus city", "Charlotte city", "Detroit city", "El Paso city", "Memphis city", "Seattle city", "Nashville-Davidson metropolitan government (balance)", "Denver city", "Louisville/Jefferson County metro government (balance)", "Portland city", "Las Vegas city", "Albuquerque city", "Tucson city", "Fresno city", "Sacramento city", "Long Beach city", "Kansas City city", "Mesa city", "Virginia Beach city", "Colorado Springs city", "Omaha city", "Raleigh city", "Cleveland city", "Oakland city", "Minneapolis city", "Wichita city", "San Juan zona urbana", "Arlington city", "Bakersfield city", "Urban Honolulu CDP", "Anaheim city", "Tampa city", "Aurora city", "Santa Ana city", "St. Louis city", "Pittsburgh city", "Corpus Christi city", "Riverside city")
cities2005donorpool <- geographicdata2005[geographicdata2005$Place.Name %in% donorpoolcities,]
cities2012donorpool <- geographicdata2012[geographicdata2012$Place.Name %in% donorpoolcities,]
pumas2005donorpool <- cities2005donorpool$PUMA[cities2005donorpool$Percent.PUMA.Population > 0.75]
pumas2012donorpool <- cities2012donorpool$PUMA[cities2012donorpool$Percent.PUMA.Population > 0.75]


nyc <- synth_data2[(synth_data2$PUMA %in% pumas2005nyc & synth_data2$YEAR < 2012) | (synth_data2$PUMA %in% pumas2012nyc & synth_data2$YEAR >=2012),]
#largecities <- synth_data2[(synth_data2$PUMA %in% pumas2005largecities & synth_data2$YEAR < 2012) | (synth_data2$PUMA %in% pumas2012largecities & synth_data2$YEAR >= 2012),]
#rest_of_us <- synth_data2[(synth_data2$PUMA %in% pumas2005rest_of_us & synth_data2$YEAR < 2012) | (synth_data2$PUMA %in% pumas2012rest_of_us & synth_data2$YEAR >= 2012),]
donorpool <- synth_data2[(synth_data2$PUMA %in% pumas2005donorpool & synth_data2$YEAR < 2012) | (synth_data2$PUMA %in% pumas2012donorpool & synth_data2$YEAR >= 2012),]


nyc$SPMPOV <- NULL
donorpool$SPMPOV <- NULL


#Labor Force Participation Rate
#NYC
part_rate_women_nyc <- nyc %>%
  group_by(YEAR) %>%
  summarise(ParticipationRate = sum(LABFORCE == 1) / sum(LABFORCE %in% c(1, 2))) %>%
  ungroup()  # Remove the grouping structure

# Join this back to the original dataframe
nyc <- nyc %>%
  left_join(part_rate_women_nyc, by = "YEAR")

#Labor Force Participation Rate
#Donorpool
donorpool$PUMA <- as.character(donorpool$PUMA)
cities2005donorpool$PUMA <- as.character(cities2005donorpool$PUMA)

#Add Cities 
donorpool <- bind_rows(
  donorpool %>% 
    filter(YEAR <= 2011) %>%
    left_join(select(cities2005donorpool, PUMA, Place.Name), by = "PUMA", relationship = "many-to-many"),
  
  donorpool %>% 
    filter(YEAR >= 2012) %>%
    left_join(select(cities2012donorpool, PUMA, Place.Name), by = "PUMA", relationship = "many-to-many")
)


#Labor Force Participation Rate
part_rate_women_donorpool <- donorpool %>%
  group_by(YEAR,Place.Name) %>%
  summarise(ParticipationRate = sum(LABFORCE == 1) / sum(LABFORCE %in% c(1, 2))) %>%
  ungroup()  # Remove the grouping structure

# Join this back to the original dataframe
donorpool <- merge(donorpool, part_rate_women_donorpool, by = c("YEAR","Place.Name"),all.x = TRUE)


#Restrict sample to women with children under age of 5
donorpool_women5 <- donorpool[donorpool$NCHLT5 > 0,]

#Labor Force Participation Rate
part_rate_women5_donorpool <- donorpool_women5 %>%
  group_by(YEAR,Place.Name) %>%
  summarise(ParticipationRate5 = sum(LABFORCE == 1) / sum(LABFORCE %in% c(1, 2))) %>%
  ungroup()  # Remove the grouping structure

donorpool_women5 <- merge(donorpool_women5, part_rate_women5_donorpool, by = c("YEAR","Place.Name"),all.x = TRUE)

  


#Run regressions
variables <- c("NCHILD","NCHLT5","AGE","EDUC","UHRSWORK","INCTOT","INCWAGE")

model1 <- lm(ParticipationRate ~ NCHILD + NCHLT5 + AGE + EDUC, data = donorpool_women5)
summary(model1)


model2 <- glm(ParticipationRate ~ married + SEX + RACE, data = donorpool)
summary(model2)


#Create necessary variables 
#Percentage of Mothers married
donorpool <- donorpool %>%
  group_by(city) %>%
  mutate(marriedmother = mean(married, na.rm = TRUE),
         ) %>%
  ungroup() 


donorpool <- donorpool %>%
  group_by(city) %>%
  mutate(
    marriedmother = mean(married, na.rm = TRUE),
    highschooldropout = sum(EDUC == c(1,2,3,4,5,6,7)/sum(EDUC), na.rm = TRUE),  
    highschool = sum(EDUC == 8/sum(EDUC), na.rm = TRUE), 
    college = sum(EDUC >= 9 /sum(EDUC), na.rm = TRUE), 
    age = mean(AGE, na.rm = TRUE),
    white = sum(RACE==1)/sum(RACE!=1),
    black = sum(RACE==2)/sum(RACE!=2),
    other = sum(RACE== c(3,4,5,6,7,8,9))/sum(RACE!=2)
    part_rate = sum()
  ) %>%
  ungroup()


synth_data_women[synth_data$]
