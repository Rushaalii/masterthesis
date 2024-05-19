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


setwd("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs")
# Reading the compressed CSV file directly into R without explicitly unzipping
#Only downloaded data until 2019 due to impact of COVID

data <- read_csv("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/100SyntheticControl_reduced.csv.gz")

################################################################################
###Statistics Women with children at age 4###
################################################################################

#Drop duplicates!!!
data <- data %>%
  distinct(SERIAL, YEAR, .keep_all = TRUE)

#1. Include all possibly eligible children: Include all 3y.o (born in 4th quarter), 4y.o., 5y.o.
data <- data[data$AGE %in% c(3,4,5,7),] #7 is control
data <- data[!(data$AGE == 3 & data$BIRTHQTR %in% c(1,2,3)),]
data <- data[data$YEAR < 2020,]

#2. Remove children currently enrolled in kindergarten
data <- data[data$EDUCD != 012,]

data$treatment <- ifelse(data$AGE == 7,0,1)

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

data <- data%>% mutate(
  education = case_when(
    EDUCD_MOM <= 061 ~ 1,
    EDUCD_MOM <= 064 ~ 1,
    EDUCD_MOM <= 100 ~ 1,
    TRUE ~ 3
  )
)

# Data Loading and Cleaning
geographicdata2012 <- read.xlsx("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/New York City/2012-2021 PUMAS Cities.xlsx", sheet = 1)
geographicdata2012$PUMA <- sub("^0", "", geographicdata2012$PUMA)
geographicdata2012$Place.Name <- ifelse(geographicdata2012$Place.Name == "Nashville-Davidson metropolitan government (balance)", "Nashville-Davidson (balance)", geographicdata2012$Place.Name)
geographicdata2012$X13 <- NULL
geographicdata2005 <- read.xlsx("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/New York City/2005-2011 PUMAS Citites.xlsx", sheet = 1)
geographicdata2005$PUMA <- sub("^0", "", geographicdata2005$PUMA)

# Keeping only cities with >500'000 population and >75% PUMA zone coverage
cities2005donorpool <- subset(geographicdata2005, geographicdata2005$Place.2000.Population > 500000 & geographicdata2005$Percent.PUMA.Population > 75)
cities2012donorpool <- subset(geographicdata2012, geographicdata2012$Place.2010.Population > 500000 & geographicdata2012$Percent.PUMA.Population > 75)

# Keeping only cities in 2012 that had >500'000 population in year 2000.
metropolitan_cities <- unique(cities2005donorpool$Place.Name)
cities2012donorpool <- subset(cities2012donorpool, Place.Name %in% metropolitan_cities)

# Removing cities with major pre-K policies
major_preK <- c("Washington city", "Baltimore city", "Jacksonville city", "Oklahoma City city", "Milwaukee city", "Fort Worth city", "Austin city", "Boston city")
cities2005donorpool <- subset(cities2005donorpool, !(Place.Name %in% major_preK))
cities2012donorpool <- subset(cities2012donorpool, !(Place.Name %in% major_preK))

# Extract PUMA codes
pumas2005donorpool <- cities2005donorpool$PUMA
pumas2012donorpool <- cities2012donorpool$PUMA
donorpool <- data[(data$PUMA %in% pumas2005donorpool & data$YEAR < 2012) | (data$PUMA %in% pumas2012donorpool & data$YEAR >= 2012),]

#Donorpool
donorpool$PUMA <- as.character(donorpool$PUMA)
cities2005donorpool$PUMA <- as.character(cities2005donorpool$PUMA)

#Add Cities 
donorpool <- bind_rows(
  donorpool %>% 
    filter(YEAR <= 2011) %>%
    left_join(dplyr::select(cities2005donorpool, PUMA, Place.Name), by = "PUMA", relationship = "many-to-many"),
  
  donorpool %>% 
    filter(YEAR >= 2012) %>%
    left_join(dplyr::select(cities2012donorpool, PUMA, Place.Name), by = "PUMA", relationship = "many-to-many")
)

donorpool$nyc <- ifelse(donorpool$Place.Name == "New York city",1,0)

nyc <- donorpool[donorpool$Place.Name == "New York city",]
not_nyc <- donorpool[!(donorpool$Place.Name == "New York city"),]


not_nyc$region <- "Not NYC"
nyc$region <- "NYC"

combined_data <- rbind(nyc, not_nyc)



#General NYC vs Non NYC
combined_data <- combined_data %>%
  filter(combined_data$treatment == 1) %>%
  group_by(YEAR, region) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT_MOM, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT_MOM, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )


#NYC by elegibility
nyc_elegebility <- nyc %>%
  group_by(YEAR,treatment) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT_MOM, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT_MOM, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )
nyc_elegebility$treatment <- as.factor(nyc_elegebility$treatment)

not_nyc_elegebility <- not_nyc %>%
  group_by(YEAR,treatment) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT_MOM, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT_MOM, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )
not_nyc_elegebility$treatment <- as.factor(not_nyc_elegebility$treatment)


#Differences-in-Differences#
### Only Difference between elegible and non-elegible WOMEN in NYC vs not NYC (Donorpool) ###
# Create a wide format data frame
nyc_differences <- nyc_elegebility %>%
  pivot_wider(
    names_from = treatment,  # Create columns based on child5
    values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
    names_prefix = "child4_"
  )

# Compute differences
nyc_differences <- nyc_differences %>%
  mutate(
    emp_rate_diff = emp_rate_child4_1 - emp_rate_child4_0,
    hours_diff = hours_child4_1 - hours_child4_0,
    fulltime_diff = fulltime_child4_1 - fulltime_child4_0,
    part_rate_diff = part_rate_child4_1 - part_rate_child4_0,
    income_diff = income_child4_1 - income_child4_0,
    dataset = "NYC"
  )


not_nyc_differences <- not_nyc_elegebility %>%
  pivot_wider(
    names_from = treatment,  # Create columns based on child5
    values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
    names_prefix = "child4_"
  )

# Compute differences
not_nyc_differences <- not_nyc_differences %>%
  mutate(
    emp_rate_diff = emp_rate_child4_1 - emp_rate_child4_0,
    hours_diff = hours_child4_1 - hours_child4_0,
    fulltime_diff = fulltime_child4_1 - fulltime_child4_0,
    part_rate_diff = part_rate_child4_1 - part_rate_child4_0,
    income_diff = income_child4_1 - income_child4_0,
    dataset = "Average Donorpool"
  )

differences_nyc_vs_nonnyc <- bind_rows(nyc_differences, not_nyc_differences)



#By race
nyc_race <- nyc %>%
  filter(treatment == 1) %>%
  group_by(YEAR,race) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT_MOM, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT_MOM, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

donorpool_race <- donorpool %>%
  filter(treatment == 1) %>%
  group_by(YEAR,race,nyc) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT_MOM, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT_MOM, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )


#By education
nyc_education <- nyc %>%
  filter(treatment == 1) %>%
  group_by(YEAR, education) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT_MOM, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT_MOM, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

donorpool_education <- donorpool %>%
  filter(treatment == 1) %>%
  group_by(YEAR, education, nyc) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT_MOM, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT_MOM, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

#By income
nyc_hhincome <- nyc %>%
  filter(treatment == 1) %>%
  group_by(YEAR, hhincome) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT_MOM, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT_MOM, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )


donorpool_hhincome <- donorpool %>%
  filter(treatment == 1) %>%
  group_by(YEAR, hhincome, nyc) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT_MOM, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT_MOM, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

###############################
### Create individual plots ###
###############################

###NYC vs non-NYC in general###
# Plot participation rate for both NYC and Non-NYC data
plot_part_rate <- ggplot(data = combined_data) +
  geom_line(aes(x = YEAR, y = part_rate, color = region)) +  # Use 'region' to differentiate the lines
  labs(x = "Year", y = "", color = "Region") +
  ggtitle("Participation Rate of Mothers with children of age 4") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
print(plot_part_rate)

plot_emp_rate <- ggplot(data = combined_data) +
  geom_line(aes(x = YEAR, y = emp_rate,color= region)) +
  labs(x = "Year", y = "") +
  ggtitle("Employment Rate of Mothers with children of age 4") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
plot(plot_emp_rate)

plot_inc <- ggplot(data = combined_data) +
  geom_line(aes(x = YEAR, y = income, color = region)) +
  labs(x = "Year", y = "") +
  ggtitle("Wage Income of Employed Mothers with Children of age 4") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
plot(plot_inc)

plot_hours <- ggplot(data = combined_data) +
  geom_line(aes(x = YEAR, y = hours, color = region)) +
  labs(x = "Year", y = "") +
  ggtitle("Hours Worked of Employed Mothers with Children of age 4") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
plot(plot_hours)

plot_fulltime <- ggplot(data = combined_data) +
  geom_line(aes(x = YEAR, y = fulltime, color = region)) +
  labs(x = "Year", y = "") +
  ggtitle("Fulltime Share of Employed Mothers with Children of age 4") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
plot(plot_fulltime)

grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)


###NYC by elegibility###
# Plot participation rate for both NYC and Non-NYC data
plot_part_rate <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = part_rate, color = treatment)) +  # Use 'region' to differentiate the lines
  labs(x = "Year", y = "") +
  ggtitle("Participation Rate of Mothers with children of age 4 vs age 7") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
print(plot_part_rate)

plot_emp_rate <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = emp_rate,color= treatment)) +
  labs(x = "Year", y = "") +
  ggtitle("Employment Rate of Mothers with children of age 4 vs age 7") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
plot(plot_emp_rate)

plot_inc <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = income, color = treatment)) +
  labs(x = "Year", y = "") +
  ggtitle("Wage Income of Employed Mothers with Children of age 4 vs age 7") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
plot(plot_inc)

plot_hours <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = hours, color = treatment)) +
  labs(x = "Year", y = "") +
  ggtitle("Hours Worked of Employed Mothers with Children of age 4 vs age 7") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
plot(plot_hours)

plot_fulltime <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = fulltime, color = treatment)) +
  labs(x = "Year", y = "") +
  ggtitle("Fulltime Share of Employed Mothers with Children of age 4 vs age 7") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
plot(plot_fulltime)

grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)


###Differences NYC vs Not NYC###
plot_part_rate <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = part_rate_diff, color = dataset)) +  # Use 'region' to differentiate the lines
  labs(x = "Year", y = "Participation Rate") +
  ggtitle("Participation Difference Mothers 4y.o. vs 7y.o.") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(differences_nyc_vs_nonnyc$YEAR), max(differences_nyc_vs_nonnyc$YEAR), by = 1))
print(plot_part_rate)

plot_emp_rate <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = emp_rate_diff, color = dataset)) +
  labs(x = "Year", y = "") +
  ggtitle("Employment Difference Mothers 4y.o. vs 7y.o.") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(differences_nyc_vs_nonnyc$YEAR), max(differences_nyc_vs_nonnyc$YEAR), by = 1))
plot(plot_emp_rate)

plot_inc <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = income_diff, color = dataset)) +
  labs(x = "Year", y = "") +
  ggtitle("Wage Difference Employed Mothers 4y.o. vs 7y.o.") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(differences_nyc_vs_nonnyc$YEAR), max(differences_nyc_vs_nonnyc$YEAR), by = 1))
plot(plot_inc)

plot_hours <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = hours_diff, color = dataset)) +
  labs(x = "Year", y = "") +
  ggtitle("Hours Worked of Employed Mothers 4y.o. vs 7y.o.") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(differences_nyc_vs_nonnyc$YEAR), max(differences_nyc_vs_nonnyc$YEAR), by = 1))
plot(plot_hours)

plot_fulltime <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = fulltime_diff, color = dataset)) +
  labs(x = "Year", y = "") +
  ggtitle("Fulltime Share of Employed Mothers 4y.o. vs 7y.o.") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(differences_nyc_vs_nonnyc$YEAR), max(differences_nyc_vs_nonnyc$YEAR), by = 1))
plot(plot_fulltime)

grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)










###Race within NYC###
nyc_race$race <- factor(nyc_race$race, levels = c(1, 2, 3), labels = c("White", "Black", "Other"))

plot_part_rate_race <- ggplot(data = nyc_race) +
  geom_line(aes(x = YEAR, y = part_rate, color = race)) +
  labs(x = "Year", y = "", color = "race") +
  ggtitle("Labor Force Participation Rate of Mothers with Children < 5 y.o. by Race") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_race$YEAR), max(nyc_race$YEAR), by = 1))
plot(plot_part_rate_race)

plot_emp_rate_race <- ggplot(data = nyc_race) +
  geom_line(aes(x = YEAR, y = emp_rate, color = race)) +
  labs(x = "Year", y = "", color = "race") +
  ggtitle("Employment Rate of Mothers with Children < 5 y.o. by Race") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_race$YEAR), max(nyc_race$YEAR), by = 1))
plot(plot_emp_rate_race)

plot_inc_race <- ggplot(data = nyc_race) +
  geom_line(aes(x = YEAR, y = income, color = race)) +
  labs(x = "Year", y = "", color = "race") +
  ggtitle("Income of Mothers with Children < 5 y.o. by Race") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_race$YEAR), max(nyc_race$YEAR), by = 1))
plot(plot_inc_race)

plot_hours_race <- ggplot(data = nyc_race) +
  geom_line(aes(x = YEAR, y = hours, color = race)) +
  labs(x = "Year", y = "", color = "race") +
  ggtitle("Hours worked by Mothers with Children < 5 y.o. by Race") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_race$YEAR), max(nyc_race$YEAR), by = 1))
plot(plot_hours_race)

plot_fulltime_race <- ggplot(data = nyc_race) +
  geom_line(aes(x = YEAR, y = fulltime, color = race)) +
  labs(x = "Year", y = "", color = "race") +
  ggtitle("Fulltime Share of Mothers with Children < 5 y.o. by Race") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_race$YEAR), max(nyc_race$YEAR), by = 1))
plot(plot_fulltime_race)

grid.arrange(plot_part_rate_race, plot_emp_rate_race, plot_inc_race, plot_hours_race,ncol=2)


###Race with NYC vs. Donorpool###
donorpool_race$race <- factor(donorpool_race$race, levels = c(1, 2, 3), labels = c("White", "Black", "Other"))


#
#
#



###Education within NYC###
nyc_education$education <- factor(nyc_education$education, levels = c(1, 2, 3), labels = c("Highschool Dropout", "Highschool Graduate", "College"))

plot_part_rate_education <- ggplot(data = nyc_education) +
  geom_line(aes(x = YEAR, y = part_rate, color = education)) +
  labs(x = "Year", y = "", color = "education") +
  ggtitle("Participation Rate of Women with Children < 5 y.o. by Education Level") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_part_rate_education)

plot_emp_rate_education <- ggplot(data = nyc_education) +
  geom_line(aes(x = YEAR, y = emp_rate, color = education)) +
  labs(x = "Year", y = "", color = "education") +
  ggtitle("Employment Rate of Women with Children < 5 y.o. by Education Level") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_emp_rate_education)


plot_inc_education <- ggplot(data = nyc_education) +
  geom_line(aes(x = YEAR, y = income, color = education)) +
  labs(x = "Year", y = "", color = "education") +
  ggtitle("Income of Women with Children < 5 y.o. by Education Level") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_inc_education)


plot_hours_education <- ggplot(data = nyc_education) +
  geom_line(aes(x = YEAR, y = hours, color = education)) +
  labs(x = "Year", y = "", color = "education") +
  ggtitle("Hours worked by Women with Children < 5 y.o. by Education Level") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_hours_education)

plot_fulltime_education <- ggplot(data = nyc_education) +
  geom_line(aes(x = YEAR, y = fulltime, color = education)) +
  labs(x = "Year", y = "", color = "education") +
  ggtitle("Fulltime Share of Women with Children < 5 y.o. by Education Level") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_fulltime_education)

grid.arrange(plot_part_rate_education, plot_emp_rate_education, plot_inc_education, plot_hours_education,ncol=2)



###Education NYC vs donorpool###
donorpool_education$education <- factor(donorpool_education$education, levels = c(1, 2, 3), labels = c("Highschool Dropout", "Highschool Graduate", "College"))

plot_part_rate_education <- ggplot(data = donorpool_education) +
  geom_line(aes(x = YEAR, y = part_rate, color = education)) +
  facet_wrap(~ nyc) +  # Separate plots based on 'nyc'
  labs(x = "Year", y = "Participation Rate", color = "Education") +
  ggtitle("Participation Rate of Women with Children < 5 y.o. by Education Level and NYC Status") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(donorpool_education$YEAR), max(donorpool_education$YEAR), by = 1))

plot(plot_part_rate_education)

plot_emp_rate_education <- ggplot(data = donorpool_education) +
  geom_line(aes(x = YEAR, y = emp_rate, color = education)) +
  facet_wrap(~ nyc) +  # Separate plots based on 'nyc'
  labs(x = "Year", y = "Employment Rate", color = "Education") +
  ggtitle("Employment Rate of Women with Children < 5 y.o. by Education Level and NYC Status") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(donorpool_education$YEAR), max(donorpool_education$YEAR), by = 1))

plot(plot_emp_rate_education)


plot_inc_education <- ggplot(data = donorpool_education) +
  geom_line(aes(x = YEAR, y = income, color = education)) +
  facet_wrap(~ nyc) +  # Separate plots based on 'nyc'
  labs(x = "Year", y = "Income", color = "Education") +
  ggtitle("Income of Women with Children < 5 y.o. by Education Level and NYC Status") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(donorpool_education$YEAR), max(donorpool_education$YEAR), by = 1))

plot(plot_inc_education)


plot_hours_education <- ggplot(data = donorpool_education) +
  geom_line(aes(x = YEAR, y = hours, color = education)) +
  facet_wrap(~ nyc) +  # Separate plots based on 'nyc'
  labs(x = "Year", y = "Hours Worked", color = "Education") +
  ggtitle("Hours Worked of Women with Children < 5 y.o. by Education Level and NYC Status") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(donorpool_education$YEAR), max(donorpool_education$YEAR), by = 1))

plot(plot_hours_education)

plot_fulltime_education <- ggplot(data = donorpool_education) +
  geom_line(aes(x = YEAR, y = fulltime, color = education)) +
  facet_wrap(~ nyc) +  # Separate plots based on 'nyc'
  labs(x = "Year", y = "Employment Rate", color = "Education") +
  ggtitle("Fulltime Share of Women with Children < 5 y.o. by Education Level and NYC Status") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(donorpool_education$YEAR), max(donorpool_education$YEAR), by = 1))

plot(plot_fulltime_education)

grid.arrange(plot_part_rate_education, plot_emp_rate_education, plot_inc_education, plot_hours_education,ncol=2)


###Income Brackets within NYC###
nyc_hhincome$hhincome <- factor(nyc_hhincome$hhincome, levels = c(1, 2, 3), labels = c("Below Poverty Treshold", "100%-300% of Poverty Treshold", "More than 300% of Poverty Treshold"))

plot_part_rate_hhincome<- ggplot(data = nyc_hhincome) +
  geom_line(aes(x = YEAR, y = part_rate, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  ggtitle("Participation Rate of Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_part_rate_hhincome)

plot_emp_rate_hhincome<- ggplot(data = nyc_hhincome) +
  geom_line(aes(x = YEAR, y = emp_rate, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  ggtitle("Employment Rate of Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_emp_rate_hhincome)


plot_inc_hhincome<- ggplot(data = nyc_hhincome) +
  geom_line(aes(x = YEAR, y = income, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  ggtitle("Income of Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_inc_hhincome)


plot_hours_hhincome<- ggplot(data = nyc_hhincome) +
  geom_line(aes(x = YEAR, y = hours, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  ggtitle("Hours worked by Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_hours_hhincome)


plot_fulltime_hhincome<- ggplot(data = nyc_hhincome) +
  geom_line(aes(x = YEAR, y = fulltime, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  ggtitle("Fulltime Share of Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_fulltime_hhincome)


grid.arrange(plot_part_rate_hhincome, plot_emp_rate_hhincome, plot_inc_hhincome, plot_hours_hhincome,ncol=2)


###Income Brackets NYC vs not NYC###
donorpool_hhincome$hhincome <- factor(donorpool_hhincome$hhincome, levels = c(1, 2, 3), labels = c("Below Poverty Treshold", "100%-300% of Poverty Treshold", "More than 300% of Poverty Treshold"))

plot_part_rate_hhincome<- ggplot(data = donorpool_hhincome) +
  geom_line(aes(x = YEAR, y = part_rate, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  facet_wrap(~ nyc) +  # Separate plots based on 'nyc'
  ggtitle("Participation Rate of Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(donorpool_hhincome$YEAR), max(donorpool_hhincome$YEAR), by = 1))
plot(plot_part_rate_hhincome)

plot_emp_rate_hhincome<- ggplot(data = donorpool_hhincome) +
  geom_line(aes(x = YEAR, y = emp_rate, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  facet_wrap(~ nyc) +  # Separate plots based on 'nyc'
  ggtitle("Employment Rate of Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(donorpool_hhincome$YEAR), max(donorpool_hhincome$YEAR), by = 1))
plot(plot_emp_rate_hhincome)


plot_inc_hhincome<- ggplot(data = donorpool_hhincome) +
  geom_line(aes(x = YEAR, y = income, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  facet_wrap(~ nyc) +  # Separate plots based on 'nyc'
  ggtitle("Income of Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(donorpool_hhincome$YEAR), max(donorpool_hhincome$YEAR), by = 1))

plot(plot_inc_hhincome)


plot_hours_hhincome<- ggplot(data = donorpool_hhincome) +
  geom_line(aes(x = YEAR, y = hours, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  facet_wrap(~ nyc) +  # Separate plots based on 'nyc'
  ggtitle("Hours of Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(donorpool_hhincome$YEAR), max(donorpool_hhincome$YEAR), by = 1))

plot(plot_hours_hhincome)


plot_fulltime_hhincome<- ggplot(data = nyc_hhincome) +
  geom_line(aes(x = YEAR, y = fulltime, color = hhincome)) +
  labs(x = "Year", y = "", color = "Household Income") +
  ggtitle("Fulltime Share of Women with Children < 5 y.o. by Household Income") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(nyc_education$YEAR), max(nyc_education$YEAR), by = 1))
plot(plot_fulltime_hhincome)


grid.arrange(plot_part_rate_hhincome, plot_emp_rate_hhincome, plot_inc_hhincome, plot_hours_hhincome, plot_fulltime_hhincome,ncol=2)


