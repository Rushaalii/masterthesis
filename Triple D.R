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
library(broom)
setwd("/Users/nic/Desktop/Thesis Data")
rm(list=ls())
data <- read_csv("100SyntheticControl_usefor_4yo.csv.gz")

#Restrict until 2019
data$YEAR <- as.numeric(as.character(data$YEAR))

data <- data[data$YEAR < 2020,]

# Create unique/state-independent PUMA
data$STATEFIP <- sprintf("%02d", data$STATEFIP)
data$PUMA <- sprintf("%05d", data$PUMA)
data$UPUMA <- paste0(data$STATEFIP,"_", data$PUMA)

data$laborforce <- ifelse(data$LABFORCE_MOM == 2, 1, ifelse(data$LABFORCE_MOM == 1, 0, NA))
data$employment <- ifelse(data$EMPSTAT == 1, 1, ifelse(data$EMPSTAT %in% c(2,3), 0, NA))
data$child5 <- ifelse(data$NCHLT5 > 0, 1, 0)
data <- data[data$AGE %in% c(3,4,7),] #7 is control
data <- data[!(data$AGE == 3 & data$BIRTHQTR %in% c(1,2,3)),]
data <- data[data$EDUCD != 012,]

data$treatment <- ifelse(data$AGE == 7,0,1)



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

# Keeping only cities with >75% PUMA zone coverage
cities2005donorpool <- subset(geographicdata2005, geographicdata2005$Percent.PUMA.Population > 75)
cities2012donorpool <- subset(geographicdata2012, geographicdata2012$Percent.PUMA.Population > 75)

# Removing cities with major pre-K policies
major_preK <- c("Washington city", "Baltimore city", "Jacksonville city", "Oklahoma City city", "Milwaukee city", "Fort Worth city", "Austin city", "Boston city")
cities2005donorpool <- subset(cities2005donorpool, !(Place.Name %in% major_preK))
cities2012donorpool <- subset(cities2012donorpool, !(Place.Name %in% major_preK))
# 
# # Keeping New York and Jersey City
# NY_OR_JERSEY <- c("New York city", "Jersey City city")
# cities2005donorpool <- subset(cities2005donorpool, Place.Name %in% NY_OR_JERSEY)
# cities2012donorpool <- subset(cities2012donorpool, Place.Name %in% NY_OR_JERSEY)

# Extract PUMA codes
upumas2005donorpool <- cities2005donorpool$UPUMA
upumas2012donorpool <- cities2012donorpool$UPUMA
donorpool <- data[(data$UPUMA %in% upumas2005donorpool & data$YEAR < 2012) | (data$UPUMA %in% upumas2012donorpool & data$YEAR >= 2012),]


donorpool <- bind_rows(
  donorpool %>% 
    filter(YEAR <= 2011) %>%
    left_join(dplyr::select(cities2005donorpool, UPUMA, Place.Name), by = "UPUMA"),
  
  donorpool %>% 
    filter(YEAR >= 2012) %>%
    left_join(dplyr::select(cities2012donorpool, UPUMA, Place.Name), by = "UPUMA")
)

NY_OR_JERSEY <- c("New York city", "Jersey City city")
donorpool <- subset(donorpool, Place.Name %in% NY_OR_JERSEY)

###########################################################################################################################
# DiD Model 4 year old children in NY (== 1) versus 7 yo children in NY (== 0) before 2014 (== 0) and after 2014 (== 1)
 ##########################################################################################################################


nyc_donorpool <- subset(donorpool, Place.Name %in% "New York city")
nyc_donorpool$time <- ifelse(nyc_donorpool$YEAR >= 2014, 1, 0)
nyc_donorpool$did <- nyc_donorpool$treatment * nyc_donorpool$time

# Calculate participation rate by year and eligibility
nyc_summary <- nyc_donorpool %>%
  group_by(YEAR, treatment, time, did) %>%
  summarise(
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    .groups = 'drop'
  )

# Fit the difference-in-differences model using lm
did_model_nyc <- lm(part_rate ~ treatment + time + did, data = nyc_summary)

# Display the summary of the model
summary(did_model_nyc)

did_results_nyc <- tidy(did_model_nyc)
print(did_results_nyc)

# Extracting and interpreting coefficients
did_coef_nyc <- did_results_nyc %>% filter(term == "did")
print(did_coef_nyc)

# Visualizing results 
plot_did_nyc <- ggplot(data = nyc_summary, aes(x = YEAR, y = part_rate, color = as.factor(treatment))) +
  geom_line() +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "blue") + # Line indicating treatment start
  annotate("text", x = 2012, y = max(nyc_summary$part_rate), label = "Pre-treatment", hjust = 1, color = "blue") + # Pre-treatment label
  annotate("text", x = 2016, y = max(nyc_summary$part_rate), label = "Post-treatment", hjust = 0, color = "blue") + # Post-treatment label
  labs(x = "Year", y = "Participation Rate", color = "Eligible (Child = 4)") +
  ggtitle("Labor Force Participation Rate in NYC: Mothers with 4yo vs Mothers with 7yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_summary$YEAR), max(nyc_summary$YEAR), by = 1))

print(plot_did_nyc)
print(head(nyc_summary))


##########################################################################################################################
# DiD 4 year old children Jersey (== 0) versus New York (== 1) before 2014 (== 0) and after 2014 (== 1)
##########################################################################################################################


nyc_jersey_donorpool <- subset(donorpool, treatment == 1) #only 4 year olds + 3 year olds born in 3rd quarter
nyc_jersey_donorpool$treatment <- ifelse(nyc_jersey_donorpool$Place.Name == "Jersey City city", 0, 1) #replace treatment == 1 with == 0 if in Jersey City city
nyc_jersey_donorpool$time <- ifelse(nyc_jersey_donorpool$YEAR >= 2014, 1, 0)
nyc_jersey_donorpool$did <- nyc_jersey_donorpool$treatment * nyc_jersey_donorpool$time

# Calculate participation rate by year and eligibility
yjd_summary <- nyc_jersey_donorpool %>%
  group_by(YEAR, treatment, time, did) %>%
  summarise(
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    .groups = 'drop'
  )

# Fit the difference-in-differences model using lm
did_model_yjd<- lm(part_rate ~ treatment + time + did, data = yjd_summary)

# Display the summary of the model
summary(did_model_yjd)

did_results_yjd <- tidy(did_model_yjd)
print(did_results_yjd)

# Extracting and interpreting coefficients
did_coef_yjd <- did_results_yjd %>% filter(term == "did")
print(did_coef_yjd)

# Visualizing results 
plot_did_yjd <- ggplot(data = yjd_summary, aes(x = YEAR, y = part_rate, color = as.factor(treatment))) +
  geom_line() +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "blue") + # Line indicating treatment start
  annotate("text", x = 2012, y = max(nyc_summary$part_rate), label = "Pre-treatment", hjust = 1, color = "blue") + # Pre-treatment label
  annotate("text", x = 2016, y = max(nyc_summary$part_rate), label = "Post-treatment", hjust = 0, color = "blue") + # Post-treatment label
  labs(x = "Year", y = "Participation Rate", color = "Eligible (Child = 4)") +
  ggtitle("LBPR Mothers of 4yo New York compared to Jersey City") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(yjd_summary$YEAR), max(yjd_summary$YEAR), by = 1))

print(plot_did_yjd)
print(head(yjd_summary))


##########################################################################################################################
# Triple Difference Model
##########################################################################################################################

jersey_donorpool <- donorpool

jersey_donorpool$time <- ifelse(jersey_donorpool$YEAR >= 2014, 1, 0)
jersey_donorpool$city <- ifelse(jersey_donorpool$Place.Name == "New York city", 1, 0)
jersey_donorpool$treatment_time <- jersey_donorpool$treatment * jersey_donorpool$time
jersey_donorpool$treatment_city <- jersey_donorpool$treatment * jersey_donorpool$city
jersey_donorpool$city_time <- jersey_donorpool$city * jersey_donorpool$time
jersey_donorpool$DDD <- jersey_donorpool$city * jersey_donorpool$time * jersey_donorpool$treatment


# Calculate participation rate by year and eligibility
jersey_summary <- jersey_donorpool %>%
  group_by(Place.Name, YEAR, treatment, time, city, treatment_time, treatment_city, city_time, DDD) %>%
  summarise(
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    .groups = 'drop'
  )

triple_diff_model <- lm(part_rate ~ DDD, data = jersey_summary)

summary(triple_diff_model)


