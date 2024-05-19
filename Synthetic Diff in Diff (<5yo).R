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

### SCRIPT for donorpool & LFP 

setwd("/Users/rabea/Desktop/BSE/Master project/Analysis /")

synth_data <- read_csv("/Users/rabea/Desktop/BSE/Master project/Analysis /usa_00036.csv.gz")

# Restrict until 2019
synth_data$YEAR <- as.numeric(as.character(synth_data$YEAR))

synth_data <- synth_data[synth_data$YEAR < 2020,]

# Create unique/state-independent PUMA
synth_data$STATEFIP <- sprintf("%02d", synth_data$STATEFIP)
synth_data$PUMA <- sprintf("%05d", synth_data$PUMA)
synth_data$UPUMA <- paste0(synth_data$STATEFIP,"_", synth_data$PUMA)

#Add new variables
synth_data$laborforce <- ifelse(synth_data$LABFORCE == 2, 1, ifelse(synth_data$LABFORCE == 1, 0, NA))
synth_data$employment <- ifelse(synth_data$EMPSTAT == 1, 1, ifelse(synth_data$EMPSTAT %in% c(2, 3), 0, NA))
synth_data$fulltime <- ifelse(synth_data$UHRSWORK >= 30, 1, 0)
synth_data$no.children <- ifelse(synth_data$NCHILD >= 4, 4, synth_data$NCHILD)
synth_data$race <- ifelse(synth_data$RACE >= 3, 3, synth_data$RACE)
synth_data$married <- ifelse(synth_data$MARST <= 2, 1, 0)
synth_data$white <- ifelse(synth_data$RACE == 1, 1, 0)
synth_data$black <- ifelse(synth_data$RACE == 2, 1, 0)
synth_data$INCWAGE[synth_data$INCWAGE == 999999] <- NA

synth_data$hsdropout <- ifelse(synth_data$EDUCD <= 061, 1, 0)
synth_data$hsgraduate <- ifelse(synth_data$EDUCD >= 062 & synth_data$EDUCD < 065, 1, 0)
synth_data$somecollege <- ifelse(synth_data$EDUCD >= 065 & synth_data$EDUCD <= 100, 1, 0)
synth_data$min_bachelor <- ifelse(synth_data$EDUCD > 100, 1, 0)

# Adding the poverty variable
synth_data$poverty <- ifelse(synth_data$POVERTY == 1, 1, 0)



################################################################################
# Creating Donor Pool
################################################################################


# Data Loading and Cleaning
geographicdata2005 <- read.xlsx("2005-2011 PUMAS Citites.xlsx", sheet = 1)
geographicdata2005$UPUMA <- paste0(geographicdata2005$FIPS.State.Code, "_", geographicdata2005$PUMA)

geographicdata2012 <- read.xlsx("2012-2021 PUMAS Cities.xlsx", sheet = 1)
geographicdata2012$Place.Name <- ifelse(geographicdata2012$Place.Name == "Nashville-Davidson metropolitan government (balance)", "Nashville-Davidson (balance)", geographicdata2012$Place.Name)
geographicdata2012$X13 <- NULL
geographicdata2012$UPUMA <- paste0(geographicdata2012$FIPS.State.Code, "_", geographicdata2012$PUMA)

# Keeping only cities with >500'000 population and >75% PUMA zone coverage
cities2005donorpool <- subset(geographicdata2005, geographicdata2005$Place.2000.Population > 500000 & geographicdata2005$Percent.PUMA.Population > 75)
cities2012donorpool <- subset(geographicdata2012, geographicdata2012$Place.2010.Population > 500000 & geographicdata2012$Percent.PUMA.Population > 75)

# Keeping only cities in 2012 that had >500'000 population in year 2000 (questionable?) 
metropolitan_cities <- unique(cities2005donorpool$Place.Name)
cities2012donorpool <- subset(cities2012donorpool, Place.Name %in% metropolitan_cities)

# Removing cities with major pre-K policies
major_preK <- c("Washington city", "Baltimore city", "Jacksonville city", "Oklahoma City city", "Milwaukee city", "Fort Worth city", "Austin city", "Boston city")
cities2005donorpool <- subset(cities2005donorpool, !(Place.Name %in% major_preK))
cities2012donorpool <- subset(cities2012donorpool, !(Place.Name %in% major_preK))

# Extract PUMA codes
upumas2005donorpool <- cities2005donorpool$UPUMA
upumas2012donorpool <- cities2012donorpool$UPUMA
donorpool <- synth_data[(synth_data$UPUMA %in% upumas2005donorpool & synth_data$YEAR < 2012) | (synth_data$UPUMA %in% upumas2012donorpool & synth_data$YEAR >= 2012),]

# Donorpool
# donorpool$UPUMA <- as.character(donorpool$UPUMA)
# cities2005donorpool$UPUMA <- as.character(cities2005donorpool$UPUMA)

#Add Cities 
donorpool <- bind_rows(
  donorpool %>% 
    filter(YEAR <= 2011) %>%
    left_join(dplyr::select(cities2005donorpool, UPUMA, Place.Name), by = "UPUMA"),
  
  donorpool %>% 
    filter(YEAR >= 2012) %>%
    left_join(dplyr::select(cities2012donorpool, UPUMA, Place.Name), by = "UPUMA")
)

#Restrict to women with children below 5
donorpool[donorpool$NCHLT5 > 0,]


################################################################################
# Labour Force Participation Rate
################################################################################

#Run regressions for donorpool with women <5
# model1 <- lm(laborforce ~ NCHILD + NCHLT5 + AGE + married + white + black + hsdropout + hsgraduate + somecollege + min_bachelor, data = donorpool)
# summary(model1)

#Create necessary variables: 
#Shares by cities 

synthcontrol_donorpool <- donorpool %>%
  group_by(Place.Name, YEAR) %>%
  summarise(
    married = weighted.mean(married, w = PERWT, na.rm = TRUE),
    white = weighted.mean(white, w = PERWT, na.rm = TRUE),
    black = weighted.mean(black, w = PERWT, na.rm = TRUE),
    age = weighted.mean(AGE, w = PERWT, na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime, w = PERWT, na.rm = TRUE),
    no_children = weighted.mean(no.children, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  )


########### Script for Synthetic Diff in Diff ############

# Create treatment indicator
synthcontrol_donorpool <- synthcontrol_donorpool %>%
  mutate(Treated = ifelse(Place.Name == "New York city" & YEAR >= 2014, 1, 0))

# Create dataset for SDID
selected_variables <- synthcontrol_donorpool[, c("Place.Name", "YEAR", "part_rate", "Treated")]
data_SDID <- data.frame(selected_variables)

# # Testing for balanced data set
# complete_cases <- data_SDID %>%
#  group_by(Place.Name, YEAR) %>%
#  summarise(n = n(), .groups = 'drop')
# 
# # Creating a balanced set of unit-time combinations
# complete_panel <- expand.grid(Place.Name = unique(data_SDID$Place.Name), YEAR = unique(data_SDID$YEAR))
# 
# # Merging with the original data
# data_SDID_balanced <- merge(complete_panel, data_SDID, by = c("Place.Name", "YEAR"), all = TRUE)
# 
# # Checking for the missing combinations (cities)
# rows_with_na <- rowSums(is.na(data_SDID_balanced)) > 0
# data_with_na <- data_SDID_balanced[rows_with_na, ]
# 
# places_to_remove <- c("Charlotte city")
# 
# data_SDID_filtered <- data_SDID_balanced[!data_SDID_balanced$Place.Name %in% places_to_remove, ]
# data_SDID_unique <- distinct(data_SDID_filtered)

# Synthetic Diff in Diff
devtools::install_github("synth-inference/synthdid")

library(synthdid) # loads the "synthdid" package

setup = panel.matrices(data_SDID) # converts data set from panel to matrix format required by synthdid estimators

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)


