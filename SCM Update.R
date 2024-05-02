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

synth_data <- read_csv("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/50SyntheticControlWomen.csv")

#Add new variables (add poverty!!!)
synth_data$married <- ifelse (synth_data$MARST <= 2, 1, 0)
synth_data$white <- ifelse (synth_data$RACE == 1, 1, 0)
synth_data$black <- ifelse (synth_data$RACE == 2, 1, 0)
synth_data$hispanic <- ifelse (synth_data$HISPAN != 0,1,0)


geographicdata2012 <- read.xlsx("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/New York City/2012-2021 PUMAS Cities.xlsx", sheet = 1)
geographicdata2012$PUMA <- sub("^0", "", geographicdata2012$PUMA)
geographicdata2005 <- read.xlsx("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/New York City/2005-2011 PUMAS Citites.xlsx", sheet = 1)
geographicdata2005$PUMA <- sub("^0", "", geographicdata2005$PUMA)

geographicdata2012$X13 <- NULL

#Geograpical Areas (PUMAS)
donorpoolcities <- c("New York city", "Los Angeles city", "Chicago city", "Houston city", "Philadelphia city", "Phoenix city", "San Antonio city", "San Diego city", "Dallas city", "San Jose city", "Indianapolis city (balance)", "San Francisco city", "Columbus city", "Charlotte city", "Detroit city", "El Paso city", "Memphis city", "Seattle city", "Nashville-Davidson metropolitan government (balance)", "Denver city", "Louisville/Jefferson County metro government (balance)", "Portland city", "Las Vegas city", "Albuquerque city", "Tucson city", "Fresno city", "Sacramento city", "Long Beach city", "Kansas City city", "Mesa city", "Virginia Beach city", "Colorado Springs city", "Omaha city", "Raleigh city", "Cleveland city", "Oakland city", "Minneapolis city", "Wichita city", "San Juan zona urbana", "Arlington city", "Bakersfield city", "Urban Honolulu CDP", "Anaheim city", "Tampa city", "Aurora city", "Santa Ana city", "St. Louis city", "Pittsburgh city", "Corpus Christi city", "Riverside city")
cities2005donorpool <- geographicdata2005[geographicdata2005$Place.Name %in% donorpoolcities,]
cities2012donorpool <- geographicdata2012[geographicdata2012$Place.Name %in% donorpoolcities,]
pumas2005donorpool <- cities2005donorpool$PUMA[cities2005donorpool$Percent.PUMA.Population > 0.75]
pumas2012donorpool <- cities2012donorpool$PUMA[cities2012donorpool$Percent.PUMA.Population > 0.75]

donorpool <- synth_data[(synth_data$PUMA %in% pumas2005donorpool & synth_data$YEAR < 2012) | (synth_data$PUMA %in% pumas2012donorpool & synth_data$YEAR >= 2012),]


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
  group_by(YEAR, Place.Name) %>%
  summarise(ParticipationRate = sum(LABFORCE == 1) / sum(LABFORCE %in% c(1, 2))) %>%
  ungroup()  # Remove the grouping structure

# Join this back to the original dataframe
donorpool <- merge(donorpool, part_rate_women_donorpool, by = c("YEAR","Place.Name"),all.x = TRUE)


#Restrict sample to women with children under age of 5
donorpool_women5 <- donorpool[donorpool$NCHLT5 > 0,]

#Labor Force Participation Rate/ Employment Rate 
part_rate_women5_donorpool <- donorpool_women5 %>%
  group_by(YEAR, Place.Name) %>%
  summarise(
    ParticipationRate5 = sum(LABFORCE == 1) / sum(LABFORCE %in% c(1, 2)),
    EmploymentRate5 = sum(EMPSTAT == 1) / sum(EMPSTAT %in% c(1, 2)),
    .groups = 'drop' 
  )

donorpool_women5 <- merge(donorpool_women5, part_rate_women5_donorpool, by = c("YEAR","Place.Name"),all.x = TRUE)


#Run regressions for donorpool with women <5
model1 <- lm(ParticipationRate5 ~ NCHILD + NCHLT5 + AGE + EDUC + married + white + black + hispanic, data = donorpool_women5)
summary(model1)

#Create necessary variables: 
#Shares by cities 

#Share married
#Average age
#Average education (maybe adjust and create binary variables)
#Share white mothers
#Share black mothers
#Share hispanic mothers


synthcontrol_donorpool <- donorpool_women5 %>%
  group_by(Place.Name, YEAR) %>%
  summarise(
    married = mean(married, na.rm = TRUE),
    white = mean(white, na.rm = TRUE),
    black = mean(black, na.rm = TRUE),
    hispanic = mean(hispanic, na.rm = TRUE),
    education = mean(EDUC, na.rm = TRUE),
    age = mean(AGE, na.rm = TRUE),
    part_rate5 = mean(ParticipationRate5, na.rm = TRUE),
    emp_rate5 = mean(EmploymentRate5, na.rm = TRUE),
    hours = mean(UHRSWORK, na.rm = TRUE),
    .groups = "drop"  
  )

#Include lags
#2010
part_rate5_2010 <- synthcontrol_donorpool %>%
  filter(YEAR == 2010) %>%
  select(Place.Name, part_rate5_2010 = part_rate5)

# Merging the 2010 values back to the original dataframe
synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate5_2010, by = "Place.Name")


#2007 values and creating a new dataframe to merge back
part_rate5_2007 <- synthcontrol_donorpool %>%
  filter(YEAR == 2007) %>%
  select(Place.Name, part_rate5_2007 = part_rate5)  # Renaming the column for clarity

# Merging the 2010 values back to the original dataframe
synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate5_2007, by = "Place.Name")


# Now, convert this factor to integers
synthcontrol_donorpool$city <- factor(synthcontrol_donorpool$Place.Name)
synthcontrol_donorpool$city <- as.numeric(synthcontrol_donorpool$city)

#Change format of data frame (necessary for command dataprep)
synthcontrol_donorpool <- as.data.frame(synthcontrol_donorpool)

#For now (STILL HAVE TO ADAPT, WHY IS THAT???)
cities_to_remove <- c(5, 7, 19, 22, 24, 35, 37, 40)
synthcontrol_donorpool <- synthcontrol_donorpool[!synthcontrol_donorpool$city %in% cities_to_remove, ]


# Correct way to specify controls
controls_identifier <- setdiff(unique(synthcontrol_donorpool$city), 23)

#New York City is no. 23
# Corrected dataprep function
# Setting up the dataprep function correctly
dataprep_out <-
  dataprep(
    foo = synthcontrol_donorpool,
    predictors = c("age","married","white","black","hispanic","education","part_rate5_2007","part_rate5_2010"),
    predictors.op = "mean",
    dependent = "part_rate5",
    unit.variable = "city",
    time.variable = "YEAR",
    treatment.identifier = 23,
    controls.identifier = controls_identifier,
    time.predictors.prior = 2006:2013,
    time.optimize.ssr = 2006:2013,
    unit.names.variable = "Place.Name",  
    time.plot = 2006:2019
  )

synth_out <- synth(dataprep_out)

path.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 1990,
          Ylab = "Labor Force Participation",
          Xlab = "Year",
          Legend = c("NYC", "Synthetic NYC"),
          Main = "NYC vs Synthetic NYC")

gaps.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 1990,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between labor force participation in NYC and its synthetic version")


synth.tables <- synth.tab(dataprep.res = dataprep_out,
                          synth.res    = synth_out
)


synth.tables$tab.pred

#Composition weights predictor variables
predictor.composition <-synth.tables$tab.v

#Composition weigths Donorpool
donorpool.composition <- synth.tables$tab.w










