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
library(openxlsx)

# Remove all objects from the workspace
rm(list = ls()) 

### SCRIPT from Til -> donorpool & LFP 

setwd("/Users/rabea/Desktop/BSE/Master project/Analysis /")

synth_data <- read_csv("/Users/rabea/Desktop/BSE/Master project/Analysis /50Synthetic Control_cleaned.csv")

geographicdata2012 <- read.xlsx("/Users/rabea/Desktop/BSE/Master project/Analysis /2012-2021 PUMAS Cities.xlsx", sheet = 1)
geographicdata2012$PUMA <- sub("^0", "", geographicdata2012$PUMA)
geographicdata2005 <- read.xlsx("/Users/rabea/Desktop/BSE/Master project/Analysis /2005-2011 PUMAS Citites.xlsx", sheet = 1)
geographicdata2005$PUMA <- sub("^0", "", geographicdata2005$PUMA)

geographicdata2012$X13 <- NULL

#Geograpical Areas (PUMAS)
donorpoolcities <- c("New York city", "Los Angeles city", "Chicago city", "Houston city", "Philadelphia city", "Phoenix city", "San Antonio city", "San Diego city", "Dallas city", "San Jose city", "Indianapolis city (balance)", "San Francisco city", "Columbus city", "Charlotte city", "Detroit city", "El Paso city", "Memphis city", "Seattle city", "Nashville-Davidson metropolitan government (balance)", "Denver city", "Louisville/Jefferson County metro government (balance)", "Portland city", "Las Vegas city", "Albuquerque city", "Tucson city", "Fresno city", "Sacramento city", "Long Beach city", "Kansas City city", "Mesa city", "Virginia Beach city", "Colorado Springs city", "Omaha city", "Raleigh city", "Cleveland city", "Oakland city", "Minneapolis city", "Wichita city", "San Juan zona urbana", "Arlington city", "Bakersfield city", "Urban Honolulu CDP", "Anaheim city", "Tampa city", "Aurora city", "Santa Ana city", "St. Louis city", "Pittsburgh city", "Corpus Christi city", "Riverside city")
cities2005donorpool <- geographicdata2005[geographicdata2005$Place.Name %in% donorpoolcities,]
cities2012donorpool <- geographicdata2012[geographicdata2012$Place.Name %in% donorpoolcities,]
pumas2005donorpool <- cities2005donorpool$PUMA[cities2005donorpool$Percent.PUMA.Population > 0.75]
pumas2012donorpool <- cities2012donorpool$PUMA[cities2012donorpool$Percent.PUMA.Population > 0.75]

donorpool <- synth_data[(synth_data$PUMA %in% pumas2005donorpool & synth_data$YEAR < 2012) | (synth_data$PUMA %in% pumas2012donorpool & synth_data$YEAR >= 2012),]


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







########### Script for Synthetic Diff in Diff ############

# Create treatment indicator
donorpool_women5 <- donorpool_women5 %>%
  mutate(Treated = ifelse(Place.Name == "New York city" & YEAR >= 2014, 1, 0))

# Create dataset for SDID
selected_variables <- donorpool_women5[, c("Place.Name", "YEAR", "ParticipationRate5", "Treated")]
data_SDID <- data.frame(selected_variables)

# Testing for balanced data set
complete_cases <- data_SDID %>%
 group_by(Place.Name, YEAR) %>%
 summarise(n = n(), .groups = 'drop')

# Creating a balanced set of unit-time combinations
complete_panel <- expand.grid(Place.Name = unique(data_SDID$Place.Name), YEAR = unique(data_SDID$YEAR))

# Merging with the original data
data_SDID_balanced <- merge(complete_panel, data_SDID, by = c("Place.Name", "YEAR"), all = TRUE)

# Checking for the missing combinations (cities)
rows_with_na <- rowSums(is.na(data_SDID_balanced)) > 0
data_with_na <- data_SDID_balanced[rows_with_na, ]

places_to_remove <- c("Arlington city", "Corpus Christi city", "Oakland city",
                       "San Juan zona urbana", "Seattle city", "Virginia Beach city",
                       "Charlotte city", "Colorado Springs city",
                       "Louisville/Jefferson County metro government (balance)",
                       "Nashville-Davidson metropolitan government (balance)")

data_SDID_filtered <- data_SDID_balanced[!data_SDID_balanced$Place.Name %in% places_to_remove, ]

data_SDID_unique <- distinct(data_SDID_filtered)

# Synthetic Diff in Diff
devtools::install_github("synth-inference/synthdid")

library(synthdid) # loads the "synthdid" package

setup = panel.matrices(data_SDID_unique) # converts data set from panel to matrix format required by synthdid estimators

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)


