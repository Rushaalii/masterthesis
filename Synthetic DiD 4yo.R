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

data <- read_csv("/Users/rabea/Desktop/BSE/Master project/Analysis /100SyntheticControl_usefor_4yo.csv.gz")


#Drop duplicates!!!
data <- data %>%
  distinct(SERIAL, YEAR, .keep_all = TRUE)

#Drop years 2020-2022
data <- data[data$YEAR < 2020,]

#1. Include all possibly eligible children: Include all 3y.o (born in 4th quarter), 4y.o., 5y.o.
data <- data[data$AGE %in% c(3,4,5),]
data <- data[!(data$AGE == 3 & data$BIRTHQTR %in% c(1,2,3)),]

#2. Remove children currently enrolled in kindergarten
data <- data[data$EDUCD != 012,]

# Create unique/state-independent PUMA
data$STATEFIP <- sprintf("%02d", data$STATEFIP)
data$PUMA <- sprintf("%05d", data$PUMA)
data$UPUMA <- paste0(data$STATEFIP,"_", data$PUMA)


data$laborforce <- ifelse(data$LABFORCE_MOM == 2, 1, ifelse(data$LABFORCE_MOM == 1, 0, NA))
data$employment <- ifelse(data$EMPSTAT_MOM == 1, 1, ifelse(data$EMPSTAT_MOM == c(2,3), 0, NA))
data$fulltime <- ifelse(data$UHRSWORK_MOM >= 30, 1, 0)
data$no.children <- ifelse(data$NCHILD_MOM >= 4, 4, data$NCHILD_MOM)
data$race <- ifelse(data$RACE_MOM >= 3, 3, data$RACE_MOM)
data$married <- ifelse (data$MARST_MOM <= 2, 1, 0)
data$white <- ifelse (data$RACE_MOM == 1, 1, 0)
data$black <- ifelse (data$RACE_MOM == 2, 1, 0)
data$INCWAGE_MOM[data$INCWAGE_MOM == 999999] <- NA

data$hsdropout = ifelse(data$EDUCD_MOM <= 061, 1,0)
data$hsgraduate = ifelse(data$EDUCD_MOM <= 064, 1,0)
data$somecollege = ifelse(data$EDUCD_MOM <= 100, 1,0)
data$min_bachelor = ifelse(data$EDUCD_MOM > 100, 1,0)


#Income brackets: Below poverty treshold, 100-300% of poverty treshold, 300% or more of poverty treshold (Household Income!)
data <- data %>% mutate(
  hhincome = case_when(
    POVERTY_MOM <= 100 ~ 1,
    POVERTY_MOM <= 500 ~ 2,
    TRUE ~ 3
  )
)

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
donorpool <- data[(data$UPUMA %in% upumas2005donorpool & data$YEAR < 2012) | (data$UPUMA %in% upumas2012donorpool & data$YEAR >= 2012),]

#Add Cities 
donorpool <- bind_rows(
  donorpool %>% 
    filter(YEAR <= 2011) %>%
    left_join(dplyr::select(cities2005donorpool, UPUMA, Place.Name), by = "UPUMA"),
  
  donorpool %>% 
    filter(YEAR >= 2012) %>%
    left_join(dplyr::select(cities2012donorpool, UPUMA, Place.Name), by = "UPUMA")
)


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
  #filter(married == 0,na.rm = TRUE) %>% #Delete/Change
  summarise(
    married = weighted.mean(married, , w = PERWT, na.rm = TRUE),
    white = weighted.mean(white, w = PERWT,na.rm = TRUE),
    black = weighted.mean(black, w = PERWT,na.rm = TRUE),
    #hispanic = weighted.mean(hispanic, na.rm = TRUE),
    share_hsdropout = weighted.mean(hsdropout, w = PERWT,na.rm = TRUE),
    share_hsgraduate = weighted.mean(hsgraduate, w = PERWT,na.rm = TRUE),
    share_somecollege = weighted.mean(somecollege, w = PERWT,na.rm = TRUE),
    share_min_bachelor = weighted.mean(min_bachelor, w = PERWT,na.rm = TRUE),
    age = weighted.mean(AGE_MOM, w = PERWT, na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT,na.rm = TRUE),
    emp_rate = weighted.mean(employment, w = PERWT,na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime, w = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT[INCWAGE_MOM > 0], na.rm = TRUE),
    no_children = weighted.mean(no.children, w = PERWT, na.rm = TRUE),
    .groups = "drop"  
  )


########### Script for Synthetic Diff in Diff ############

# Create treatment indicator
synthcontrol_donorpool <- synthcontrol_donorpool %>%
  mutate(Treated = ifelse(Place.Name == "New York city" & YEAR >= 2013, 1, 0))

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

