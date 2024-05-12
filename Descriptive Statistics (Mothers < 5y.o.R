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

################################################################################
###Statistics Women with children under age of 5###
################################################################################

data <- read_csv("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/100SyntheticControl_usefor_5yo.csv.gz")
data <- data[data$YEAR < 2020,]

#DataCleaning 
# data <- data[!is.na(data$EMPSTAT), ] #Delete NAs
# data <- data[!is.na(data$LABFORCE), ] #Delete NAs
# data <- data[data$INCWAGE != 999999,]


#Add additional variables
data$laborforce <- ifelse(data$LABFORCE == 2, 1, ifelse(data$LABFORCE == 1, 0, NA))
data$employment <- ifelse(data$EMPSTAT == 1, 1, ifelse(data$EMPSTAT == c(2,3), 0, NA))
data$fulltime <- ifelse(data$UHRSWORK >= 30, 1, 0)
data$no.children <- ifelse(data$NCHILD >= 4, 4, data$NCHILD)
data$race <- ifelse(data$RACE >= 3, 3, data$RACE)
data$married <- ifelse (data$MARST <= 2, 1, 0)
data$white <- ifelse (data$RACE == 1, 1, 0)
data$black <- ifelse (data$RACE == 2, 1, 0)
data$INCWAGE[data$INCWAGE == 999999] <- NA
data$child5 <- ifelse (data$NCHLT5 >0,1,0)

data$hsdropout = ifelse(data$EDUCD <= 061, 1,0)
data$hsgraduate = ifelse(data$EDUCD <= 064, 1,0)
data$somecollege = ifelse(data$EDUCD <= 100, 1,0)
data$min_bachelor = ifelse(data$EDUCD > 100, 1,0)


#Income brackets: Below poverty treshold, 100-300% of poverty treshold, 300% or more of poverty treshold (Household Income!)
data <- data %>% mutate(
  hhincome = case_when(
    POVERTY <= 100 ~ 1,
    POVERTY <= 500 ~ 2,
    TRUE ~ 3
  )
)

data <- data%>% mutate(
  education = case_when(
    EDUCD <= 061 ~ 1,
    EDUCD <= 064 ~ 1,
    EDUCD <= 100 ~ 1,
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
cities2005donorpool <- subset(geographicdata2005, geographicdata2005$Place.2000.Population > 500000 & geographicdata2005$Percent.PUMA.Population > 0.75)
cities2012donorpool <- subset(geographicdata2012, geographicdata2012$Place.2010.Population > 500000 & geographicdata2012$Percent.PUMA.Population > 0.75)

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
combined_data_5 <- combined_data[combined_data$child5 == 1, ]


#General NYC vs Non NYC
combined_data <- combined_data %>%
  group_by(YEAR, region) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], w = PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

combined_data_5 <- combined_data_5 %>%
  group_by(YEAR, region) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], w = PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )



#NYC by elegibility
nyc_elegebility <- nyc %>%
  group_by(YEAR,child5) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], w = PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )
nyc_elegebility$child5 <- as.factor(nyc_elegebility$child5)

not_nyc_elegebility <- not_nyc %>%
  group_by(YEAR,child5) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], w = PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )
not_nyc_elegebility$child5 <- as.factor(not_nyc_elegebility$child5)


#Differences-in-Differences#
### Only Difference between elegible and non-elegible WOMEN in NYC vs not NYC (Donorpool) ###
# Create a wide format data frame
nyc_differences <- nyc_elegebility %>%
  pivot_wider(
    names_from = child5,  # Create columns based on child5
    values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
    names_prefix = "child5_"
  )

# Compute differences
nyc_differences <- nyc_differences %>%
  mutate(
    emp_rate_diff = emp_rate_child5_1 - emp_rate_child5_0,
    hours_diff = hours_child5_1 - hours_child5_0,
    fulltime_diff = fulltime_child5_1 - fulltime_child5_0,
    part_rate_diff = part_rate_child5_1 - part_rate_child5_0,
    income_diff = income_child5_1 - income_child5_0,
    dataset = "NYC"
  )


not_nyc_differences <- not_nyc_elegebility %>%
  pivot_wider(
    names_from = child5,  # Create columns based on child5
    values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
    names_prefix = "child5_"
  )

# Compute differences
not_nyc_differences <- not_nyc_differences %>%
  mutate(
    emp_rate_diff = emp_rate_child5_1 - emp_rate_child5_0,
    hours_diff = hours_child5_1 - hours_child5_0,
    fulltime_diff = fulltime_child5_1 - fulltime_child5_0,
    part_rate_diff = part_rate_child5_1 - part_rate_child5_0,
    income_diff = income_child5_1 - income_child5_0,
    dataset = "Average Donorpool"
  )

differences_nyc_vs_nonnyc <- bind_rows(nyc_differences, not_nyc_differences)



#By race
nyc_race <- nyc %>%
  filter(child5 == 1) %>%
  group_by(YEAR,race) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], wt = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, wt = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

donorpool_race <- donorpool %>%
  filter(child5 == 1) %>%
  group_by(YEAR,race,nyc) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], wt = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, wt = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )


#By education
nyc_education <- nyc %>%
  filter(child5 == 1) %>%
  group_by(YEAR, education) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], wt = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, wt = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

donorpool_education <- donorpool %>%
  filter(child5 == 1) %>%
  group_by(YEAR, education, nyc) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], wt = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, wt = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

#By income
nyc_hhincome <- nyc %>%
  filter(child5 == 1) %>%
  group_by(YEAR, hhincome) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], wt = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, wt = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )


donorpool_hhincome <- donorpool %>%
  filter(child5 == 1) %>%
  group_by(YEAR, hhincome, nyc) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT == 1], wt = PERWT[EMPSTAT == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, wt = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], PERWT[INCWAGE > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

###############################
### Create individual plots ###
###############################

###NYC vs non-NYC in general###
# Plot participation rate for both NYC and Non-NYC data
plot_part_rate <- ggplot(data = combined_data_5) +
  geom_line(aes(x = YEAR, y = part_rate, color = region)) +  # Use 'region' to differentiate the lines
  labs(x = "Year", y = "", color = "Region") +
  ggtitle("Participation Rate of Mothers with children < 5yo (NYC vs. Non-NYC)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
print(plot_part_rate)

plot_emp_rate <- ggplot(data = combined_data_5) +
  geom_line(aes(x = YEAR, y = emp_rate,color= region)) +
  labs(x = "Year", y = "") +
  ggtitle("Employment Rate of Mothers with children < 5yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
plot(plot_emp_rate)

plot_inc <- ggplot(data = combined_data_5) +
  geom_line(aes(x = YEAR, y = income, color = region)) +
  labs(x = "Year", y = "") +
  ggtitle("Wage Income of Employed Mothers with Children < 5yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
plot(plot_inc)

plot_hours <- ggplot(data = combined_data_5) +
  geom_line(aes(x = YEAR, y = hours, color = region)) +
  labs(x = "Year", y = "") +
  ggtitle("Hours Worked of Employed Mothers with Children < 5yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
plot(plot_hours)

plot_fulltime <- ggplot(data = combined_data_5) +
  geom_line(aes(x = YEAR, y = fulltime, color = region)) +
  labs(x = "Year", y = "") +
  ggtitle("Fulltime Share of Employed Mothers with Children < 5yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(combined_data$YEAR), max(combined_data$YEAR), by = 1))
plot(plot_fulltime)

grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)


###NYC by elegibility###
# Plot participation rate for both NYC and Non-NYC data
plot_part_rate <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = part_rate, color = child5)) +  # Use 'region' to differentiate the lines
  labs(x = "Year", y = "") +
  ggtitle("Participation Rate of Mothers with children < 5yo in NYC") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
print(plot_part_rate)

plot_emp_rate <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = emp_rate,color= child5)) +
  labs(x = "Year", y = "") +
  ggtitle("Employment Rate of Mothers with children < 5yo in NYC") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
plot(plot_emp_rate)

plot_inc <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = income, color = child5)) +
  labs(x = "Year", y = "") +
  ggtitle("Wage Income of Employed Mothers with Children < 5yo in NYC") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
plot(plot_inc)

plot_hours <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = hours, color = child5)) +
  labs(x = "Year", y = "") +
  ggtitle("Hours Worked of Employed Mothers with Children < 5yo in NYC") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
plot(plot_hours)

plot_fulltime <- ggplot(data = nyc_elegebility) +
  geom_line(aes(x = YEAR, y = fulltime, color = child5)) +
  labs(x = "Year", y = "") +
  ggtitle("Fulltime Share of Employed Mothers with Children < 5yo in NYC") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_elegebility$YEAR), max(nyc_elegebility$YEAR), by = 1))
plot(plot_fulltime)

grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)


###Differences NYC vs Not NYC###
plot_part_rate <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = part_rate_diff, color = dataset)) +  # Use 'region' to differentiate the lines
  labs(x = "Year", y = "Participation Rate") +
  ggtitle("Participation Difference Mothers vs Mothers with children < 5yo (NYC vs. Non-NYC)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(differences_nyc_vs_nonnyc$YEAR), max(differences_nyc_vs_nonnyc$YEAR), by = 1))
print(plot_part_rate)

plot_emp_rate <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = emp_rate_diff, color = dataset)) +
  labs(x = "Year", y = "") +
  ggtitle("Employment Difference of Mothers vs Mothers with children < 5yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(differences_nyc_vs_nonnyc$YEAR), max(differences_nyc_vs_nonnyc$YEAR), by = 1))
plot(plot_emp_rate)

plot_inc <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = income_diff, color = dataset)) +
  labs(x = "Year", y = "") +
  ggtitle("Wage Difference Employed Mothers vs Mothers with Children < 5yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(differences_nyc_vs_nonnyc$YEAR), max(differences_nyc_vs_nonnyc$YEAR), by = 1))
plot(plot_inc)

plot_hours <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = hours_diff, color = dataset)) +
  labs(x = "Year", y = "") +
  ggtitle("Hours Difference Employed Mothers vs Mothers with Children < 5yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(differences_nyc_vs_nonnyc$YEAR), max(differences_nyc_vs_nonnyc$YEAR), by = 1))
plot(plot_hours)

plot_fulltime <- ggplot(data = differences_nyc_vs_nonnyc) +
  geom_line(aes(x = YEAR, y = fulltime_diff, color = dataset)) +
  labs(x = "Year", y = "") +
  ggtitle("Fulltime Difference Employed Mothers vs Mothers with Children < 5yo") +
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



























# 
# ##########################################################################################
# ###Statistics Women with children under age of 5 vs Women without children under age 5 ###
# ##########################################################################################
# 
# rm(list = ls()) 
# data <- read_csv("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/50Synthetic Control.csv.gz")
# 
# #Data Cleaning
# # data <- data[!is.na(data$EMPSTAT), ] #Delete NAs
# # data <- data[!is.na(data$LABFORCE), ] #Delete NAs
# # data <- data[data$INCWAGE != 999999,]
# 
# # Data Loading and Cleaning
# geographicdata2012 <- read.xlsx("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/New York City/2012-2021 PUMAS Cities.xlsx", sheet = 1)
# geographicdata2012$PUMA <- sub("^0", "", geographicdata2012$PUMA)
# geographicdata2012$Place.Name <- ifelse(geographicdata2012$Place.Name == "Nashville-Davidson metropolitan government (balance)", "Nashville-Davidson (balance)", geographicdata2012$Place.Name)
# geographicdata2012$X13 <- NULL
# geographicdata2005 <- read.xlsx("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs/New York City/2005-2011 PUMAS Citites.xlsx", sheet = 1)
# geographicdata2005$PUMA <- sub("^0", "", geographicdata2005$PUMA)
# 
# # Keeping only cities with >500'000 population and >75% PUMA zone coverage
# cities2005donorpool <- subset(geographicdata2005, geographicdata2005$Place.2000.Population > 500000 & geographicdata2005$Percent.PUMA.Population > 0.75)
# cities2012donorpool <- subset(geographicdata2012, geographicdata2012$Place.2010.Population > 500000 & geographicdata2012$Percent.PUMA.Population > 0.75)
# 
# # Keeping only cities in 2012 that had >500'000 population in year 2000.
# metropolitan_cities <- unique(cities2005donorpool$Place.Name)
# cities2012donorpool <- subset(cities2012donorpool, Place.Name %in% metropolitan_cities)
# 
# # Removing cities with major pre-K policies
# major_preK <- c("Washington city", "Baltimore city", "Jacksonville city", "Oklahoma City city", "Milwaukee city", "Fort Worth city", "Austin city", "Boston city")
# cities2005donorpool <- subset(cities2005donorpool, !(Place.Name %in% major_preK))
# cities2012donorpool <- subset(cities2012donorpool, !(Place.Name %in% major_preK))
# 
# # Extract PUMA codes
# pumas2005donorpool <- cities2005donorpool$PUMA
# pumas2012donorpool <- cities2012donorpool$PUMA
# donorpool <- data[(data$PUMA %in% pumas2005donorpool & data$YEAR < 2012) | (data$PUMA %in% pumas2012donorpool & data$YEAR >= 2012),]
# 
# 
# #Donorpool
# donorpool$PUMA <- as.character(donorpool$PUMA)
# cities2005donorpool$PUMA <- as.character(cities2005donorpool$PUMA)
# 
# #Add Cities 
# donorpool <- bind_rows(
#   donorpool %>% 
#     filter(YEAR <= 2011) %>%
#     left_join(dplyr::select(cities2005donorpool, PUMA, Place.Name), by = "PUMA", relationship = "many-to-many"),
#   
#   donorpool %>% 
#     filter(YEAR >= 2012) %>%
#     left_join(dplyr::select(cities2012donorpool, PUMA, Place.Name), by = "PUMA", relationship = "many-to-many")
# )
# 
# 
# 
# #Add additional variables
# donorpool$fulltime <- ifelse(donorpool$UHRSWORK >= 30, 1, 0)
# donorpool$no.children <- ifelse(donorpool$NCHILD >= 4, 4, donorpool$NCHILD)
# donorpool$race <- ifelse(donorpool$RACE >= 3, 3, donorpool$RACE)
# donorpool$married <- ifelse (donorpool$MARST <= 2, 1, 0)
# donorpool$white <- ifelse (donorpool$RACE == 1, 1, 0)
# donorpool$black <- ifelse (donorpool$RACE == 2, 1, 0)
# donorpool$hispanic <- ifelse (donorpool$HISPAN != 0,1,0)
# # donorpool$employed <- ifelse (donorpool$EMPSTAT == 1, 1, 0)
# # donorpool$laborforce <- ifelse (donorpool$LABFORCE == 2,1,0)
# 
# #Education: 1: Highschool dropout, 2: Highschool Graduate, 3: College
# #data$education <- ifelse(data$EDUCD <= 061, 1,
# #ifelse(data$EDUC <= 064, 2, 3))
# donorpool <- donorpool %>% mutate(
#   education = case_when(
#     EDUCD <= 061 ~ 1,
#     EDUCD <= 064 ~ 2,
#     TRUE ~ 3
#   )
# )
# 
# #Income brackets: Below poverty treshold, 100-300% of poverty treshold, 300% or more of poverty treshold (Household Income!)
# # donorpool <- donorpool %>% mutate(
# #   hhincome = case_when(
# #     POVERTY <= 100 ~ 1,
# #     POVERTY <= 300 ~ 2,
# #     TRUE ~ 3
# #   )
# # )
# 
# 
# nyc <- donorpool[donorpool$Place.Name == "New York city",]
# not_nyc <- donorpool[!(donorpool$Place.Name == "New York city"),]
# 
# not_nyc$region <- "Not NYC"
# nyc$region <- "NYC"
# 
# 
# not_nyc$child5 <- ifelse(not_nyc$NCHLT5 > 0, 1,0)
# nyc$child5 <- ifelse(nyc$NCHLT5 > 0, 1,0)
# 
# 
# ### Differences between elegible and non-elegible WOMEN in NYC ###
# 
# nyc_differences <- nyc %>%
#   group_by(child5,YEAR) %>%
#   summarise(
#     emp_rate = sum(EMPSTAT == 1) / sum(EMPSTAT %in% c(1, 2)),
#     hours = mean(UHRSWORK[EMPSTAT == 1]),
#     fulltime = sum(EMPSTAT == 1 & fulltime == 1) / sum(EMPSTAT == 1 & (fulltime == 0 | fulltime == 1)),
#     part_rate = sum(LABFORCE == 2) / sum(LABFORCE %in% c(1, 2)),
#     income =  mean(INCWAGE[INCWAGE > 0])
#   )
# 
# nyc_differences$child5 <- factor(nyc_differences$child5, levels = c(0, 1))
# 
# not_nyc_differences <- not_nyc %>%
#   group_by(child5, YEAR) %>%
#   summarise(
#     emp_rate = sum(EMPSTAT == 1) / sum(EMPSTAT %in% c(1, 2)),
#     hours = mean(UHRSWORK[EMPSTAT == 1]),
#     fulltime = sum(EMPSTAT == 1 & fulltime == 1) / sum(EMPSTAT == 1 & (fulltime == 0 | fulltime == 1)),
#     part_rate = sum(LABFORCE == 2) / sum(LABFORCE %in% c(1, 2)),
#     income =  mean(INCWAGE[INCWAGE > 0])
#   )
# 
# not_nyc_differences$child5 <- factor(not_nyc_differences$child5, levels = c(0, 1))
# 
# 
# plot_part_rate <- ggplot(data = nyc_differences) +
#   geom_line(aes(x = YEAR, y = part_rate, color = child5)) +  # Use 'region' to differentiate the lines
#   labs(x = "Year", y = "Participation Rate", color = "Region") +
#   ggtitle("Participation Rate of Women vs Mothers with children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences$YEAR), max(nyc_differences$YEAR), by = 1))
# print(plot_part_rate)
# 
# plot_emp_rate <- ggplot(data = nyc_differences) +
#   geom_line(aes(x = YEAR, y = emp_rate, color= child5)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Employment Rate of Women vs Mothers with children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences$YEAR), max(nyc_differences$YEAR), by = 1))
# plot(plot_emp_rate)
# 
# plot_inc <- ggplot(data = nyc_differences) +
#   geom_line(aes(x = YEAR, y = income, color = child5)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Wage Income of Employed Women vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences$YEAR), max(nyc_differences$YEAR), by = 1))
# plot(plot_inc)
# 
# plot_hours <- ggplot(data = nyc_differences) +
#   geom_line(aes(x = YEAR, y = hours, color = child5)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Hours Worked of Employed Women vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences$YEAR), max(nyc_differences$YEAR), by = 1))
# plot(plot_hours)
# 
# plot_fulltime <- ggplot(data = nyc_differences) +
#   geom_line(aes(x = YEAR, y = fulltime, color = child5)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Fulltime Share of Employed Women vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences$YEAR), max(nyc_differences$YEAR), by = 1))
# plot(plot_fulltime)
# 
# 
# grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)
# 
# 
# ### Differences between elegible and non-elegible MOTHERS in NYC ###
# 
# nyc_differences_mothers <- nyc %>%
#   filter(NCHILD > 0) %>%
#   group_by(YEAR, child5) %>%
#   summarise(
#     emp_rate = sum(EMPSTAT == 1) / sum(EMPSTAT %in% c(1, 2)),
#     hours = mean(UHRSWORK[EMPSTAT == 1]),
#     fulltime = sum(EMPSTAT == 1 & fulltime == 1) / sum(EMPSTAT == 1 & (fulltime == 0 | fulltime == 1)),
#     part_rate = sum(LABFORCE == 2) / sum(LABFORCE %in% c(1, 2)),
#     income =  mean(INCWAGE[INCWAGE > 0])
#   )
# 
# nyc_differences_mothers$child5 <- factor(nyc_differences_mothers$child5, levels = c(0, 1))
# 
# 
# not_nyc_differences_mothers <- not_nyc %>%
#   filter(NCHILD > 0) %>%
#   group_by(YEAR, child5) %>%
#   summarise(
#     emp_rate = sum(EMPSTAT == 1) / sum(EMPSTAT %in% c(1, 2)),
#     hours = mean(UHRSWORK[EMPSTAT == 1]),
#     fulltime = sum(EMPSTAT == 1 & fulltime == 1) / sum(EMPSTAT == 1 & (fulltime == 0 | fulltime == 1)),
#     part_rate = sum(LABFORCE == 2) / sum(LABFORCE %in% c(1, 2)),
#     income =  mean(INCWAGE[INCWAGE > 0])
#   )
# 
# not_nyc_differences_mothers$child5 <- factor(not_nyc_differences_mothers$child5, levels = c(0, 1))
# 
# plot_part_rate <- ggplot(data = nyc_differences_mothers) +
#   geom_line(aes(x = YEAR, y = part_rate, color = child5)) +  # Use 'region' to differentiate the lines
#   labs(x = "Year", y = "Participation Rate") +
#   ggtitle("Participation Rate of Mothers without vs Mothers with children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_mothers$YEAR), max(nyc_differences_mothers$YEAR), by = 1))
# print(plot_part_rate)
# 
# plot_emp_rate <- ggplot(data = nyc_differences_mothers) +
#   geom_line(aes(x = YEAR, y = emp_rate,color= child5)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Employment Rate of Mothers without vs Mothers with children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_mothers$YEAR), max(nyc_differences_mothers$YEAR), by = 1))
# plot(plot_emp_rate)
# 
# plot_inc <- ggplot(data = nyc_differences_mothers) +
#   geom_line(aes(x = YEAR, y = income, color = child5)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Wage Income of Employed Mothers without vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_mothers$YEAR), max(nyc_differences_mothers$YEAR), by = 1))
# plot(plot_inc)
# 
# plot_hours <- ggplot(data = nyc_differences_mothers) +
#   geom_line(aes(x = YEAR, y = hours, color = child5)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Hours Worked of Employed Mothers without vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_mothers$YEAR), max(nyc_differences_mothers$YEAR), by = 1))
# plot(plot_hours)
# 
# plot_fulltime <- ggplot(data = nyc_differences_mothers) +
#   geom_line(aes(x = YEAR, y = fulltime, color = child5)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Fulltime Share of Employed Mothers without vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_mothers$YEAR), max(nyc_differences_mothers$YEAR), by = 1))
# plot(plot_fulltime)
# 
# grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)
# 
# #############################
# #Differences-in-Differences#
# #############################
# 
# 
# ### Only Difference between elegible and non-elegible WOMEN in NYC vs not NYC (Donorpool) ###
# # Create a wide format data frame
# nyc_differences_wide1 <- nyc_differences %>%
#   pivot_wider(
#     names_from = child5,  # Create columns based on child5
#     values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
#     names_prefix = "child5_"
#   )
# 
# # Compute differences
# nyc_differences_wide1 <- nyc_differences_wide1 %>%
#   mutate(
#     emp_rate_diff = emp_rate_child5_1 - emp_rate_child5_0,
#     hours_diff = hours_child5_1 - hours_child5_0,
#     fulltime_diff = fulltime_child5_1 - fulltime_child5_0,
#     part_rate_diff = part_rate_child5_1 - part_rate_child5_0,
#     income_diff = income_child5_1 - income_child5_0,
#     dataset = "NYC"
#   )
# 
# 
# not_nyc_differences_wide1 <- not_nyc_differences %>%
#   pivot_wider(
#     names_from = child5,  # Create columns based on child5
#     values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
#     names_prefix = "child5_"
#   )
# 
# # Compute differences
# not_nyc_differences_wide1 <- not_nyc_differences_wide1 %>%
#   mutate(
#     emp_rate_diff = emp_rate_child5_1 - emp_rate_child5_0,
#     hours_diff = hours_child5_1 - hours_child5_0,
#     fulltime_diff = fulltime_child5_1 - fulltime_child5_0,
#     part_rate_diff = part_rate_child5_1 - part_rate_child5_0,
#     income_diff = income_child5_1 - income_child5_0,
#     dataset = "not NYC"
#   )
# 
# NYC_vs_nonNYC <- bind_rows(nyc_differences_wide1, not_nyc_differences_wide1)
# 
# plot_part_rate <- ggplot(data = NYC_vs_nonNYC) +
#   geom_line(aes(x = YEAR, y = part_rate_diff, color = dataset)) +  # Use 'region' to differentiate the lines
#   labs(x = "Year", y = "Participation Rate", color = "Region") +
#   ggtitle("Participation Difference Women vs Mothers with children < 5yo (NYC vs. Non-NYC)") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(NYC_vs_nonNYC$YEAR), max(NYC_vs_nonNYC$YEAR), by = 1))
# print(plot_part_rate)
# 
# plot_emp_rate <- ggplot(data = NYC_vs_nonNYC) +
#   geom_line(aes(x = YEAR, y = emp_rate_diff, color = dataset)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Employment Difference of Women vs Mothers with children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(NYC_vs_nonNYC$YEAR), max(NYC_vs_nonNYC$YEAR), by = 1))
# plot(plot_emp_rate)
# 
# plot_inc <- ggplot(data = NYC_vs_nonNYC) +
#   geom_line(aes(x = YEAR, y = income_diff, color = dataset)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Wage Difference Employed Women vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(NYC_vs_nonNYC$YEAR), max(NYC_vs_nonNYC$YEAR), by = 1))
# plot(plot_inc)
# 
# plot_hours <- ggplot(data = NYC_vs_nonNYC) +
#   geom_line(aes(x = YEAR, y = hours_diff, color = dataset)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Hours Worked of Employed Women vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(NYC_vs_nonNYC$YEAR), max(NYC_vs_nonNYC$YEAR), by = 1))
# plot(plot_hours)
# 
# plot_fulltime <- ggplot(data = NYC_vs_nonNYC) +
#   geom_line(aes(x = YEAR, y = fulltime_diff)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Fulltime Share of Employed Women vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(NYC_vs_nonNYC$YEAR), max(NYC_vs_nonNYC$YEAR), by = 1))
# plot(plot_fulltime)
# 
# grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)
# 
# ### Only Difference between elegible and non-elegible MOTHERS in NYC ###
# 
# # Create a wide format data frame
# nyc_differences_wide <- nyc_differences_mothers %>%
#   pivot_wider(
#     names_from = child5,  # Create columns based on child5
#     values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
#     names_prefix = "child5_"
#   )
# 
# # Compute differences
# nyc_differences_wide <- nyc_differences_wide %>%
#   mutate(
#     emp_rate_diff = emp_rate_child5_1 - emp_rate_child5_0,
#     hours_diff = hours_child5_1 - hours_child5_0,
#     fulltime_diff = fulltime_child5_1 - fulltime_child5_0,
#     part_rate_diff = part_rate_child5_1 - part_rate_child5_0,
#     income_diff = income_child5_1 - income_child5_0,
#     dataset = "NYC"
#   )
# 
# not_nyc_differences_wide <- not_nyc_differences_mothers %>%
#   pivot_wider(
#     names_from = child5,  # Create columns based on child5
#     values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
#     names_prefix = "child5_"
#   )
# 
# 
# # Compute differences
# not_nyc_differences_wide <- not_nyc_differences_wide %>%
#   mutate(
#     emp_rate_diff = emp_rate_child5_1 - emp_rate_child5_0,
#     hours_diff = hours_child5_1 - hours_child5_0,
#     fulltime_diff = fulltime_child5_1 - fulltime_child5_0,
#     part_rate_diff = part_rate_child5_1 - part_rate_child5_0,
#     income_diff = income_child5_1 - income_child5_0,
#     dataset = "not NYC"
#   )
# 
# NYC_vs_nonNYC_mothers <- bind_rows(nyc_differences_wide, not_nyc_differences_wide)
# 
# 
# plot_part_rate <- ggplot(data = NYC_vs_nonNYC_mothers) +
#   geom_line(aes(x = YEAR, y = part_rate_diff, color = dataset)) +  # Use 'region' to differentiate the lines
#   labs(x = "Year", y = "Participation Rate", color = "Region") +
#   ggtitle("Participation Difference Mothers without vs Mothers with children < 5yo (NYC vs. Non-NYC)") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_wide$YEAR), max(nyc_differences_wide$YEAR), by = 1))
# print(plot_part_rate)
# 
# plot_emp_rate <- ggplot(data = NYC_vs_nonNYC_mothers) +
#   geom_line(aes(x = YEAR, y = emp_rate_diff, color = dataset)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Employment Difference Mothers without vs Mothers with children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_wide$YEAR), max(nyc_differences_wide$YEAR), by = 1))
# plot(plot_emp_rate)
# 
# plot_inc <- ggplot(data = NYC_vs_nonNYC_mothers) +
#   geom_line(aes(x = YEAR, y = income_diff, color = dataset)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Income Difference Employed Mothers without vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_wide$YEAR), max(nyc_differences_wide$YEAR), by = 1))
# plot(plot_inc)
# 
# plot_hours <- ggplot(data = NYC_vs_nonNYC_mothers) +
#   geom_line(aes(x = YEAR, y = hours_diff, color = dataset)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Hours Difference Employed Mothers without vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_wide$YEAR), max(nyc_differences_wide$YEAR), by = 1))
# plot(plot_hours)
# 
# plot_fulltime <- ggplot(data = NYC_vs_nonNYC_mothers) +
#   geom_line(aes(x = YEAR, y = fulltime_diff, color = dataset)) +
#   labs(x = "Year", y = "") +
#   ggtitle("Fulltime Share of Employed Mothers without vs Mothers with Children < 5yo") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(nyc_differences_wide$YEAR), max(nyc_differences_wide$YEAR), by = 1))
# plot(plot_fulltime)
# 
# 
# grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)
# 
# 
# 
# 
# 
# 
# 
# # Create a wide format data frame
# nyc_differences_wide1 <- nyc_differences %>%
#   pivot_wider(
#     names_from = child5,  # Create columns based on child5
#     values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
#     names_prefix = "child5_"
#   )
# 
# # Compute differences
# nyc_differences_wide1 <- nyc_differences_wide1 %>%
#   mutate(
#     emp_rate_diff = emp_rate_child5_1 - emp_rate_child5_0,
#     hours_diff = hours_child5_1 - hours_child5_0,
#     fulltime_diff = fulltime_child5_1 - fulltime_child5_0,
#     part_rate_diff = part_rate_child5_1 - part_rate_child5_0,
#     income_diff = income_child5_1 - income_child5_0,
#     dataset = "NYC"
#   )
# 
# 
# not_nyc_differences_wide1 <- not_nyc_differences %>%
#   pivot_wider(
#     names_from = child5,  # Create columns based on child5
#     values_from = c(emp_rate, hours, fulltime, part_rate, income),  # Extract the required metrics
#     names_prefix = "child5_"
#   )
# 
# # Compute differences
# not_nyc_differences_wide1 <- not_nyc_differences_wide1 %>%
#   mutate(
#     emp_rate_diff = emp_rate_child5_1 - emp_rate_child5_0,
#     hours_diff = hours_child5_1 - hours_child5_0,
#     fulltime_diff = fulltime_child5_1 - fulltime_child5_0,
#     part_rate_diff = part_rate_child5_1 - part_rate_child5_0,
#     income_diff = income_child5_1 - income_child5_0,
#     dataset = "not NYC"
#   )
# 
# NYC_vs_nonNYC <- bind_rows(nyc_differences_wide1, not_nyc_differences_wide1)
# 
# 
# 
# 
# 
