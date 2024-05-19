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


# Remove all objects from the workspace
rm(list=ls())
setwd("/Users/tilpommer/Documents/BSE/Term 3/Master project/Results")

data <- read_csv("/Users/tilpommer/Documents/BSE/Term 3/Master project/Results/100SyntheticControl_usefor_below5yo.csv.gz")

#Restrict until 2019
data$YEAR <- as.numeric(as.character(synth_data$YEAR))

data <- data[data$YEAR < 2020,]

# Create unique/state-independent PUMA
data$STATEFIP <- sprintf("%02d", data$STATEFIP)
data$PUMA <- sprintf("%05d", data$PUMA)
data$UPUMA <- paste0(data$STATEFIP,"_", data$PUMA)

data$laborforce <- ifelse(data$LABFORCE == 2, 1, ifelse(data$LABFORCE == 1, 0, NA))
data$employment <- ifelse(data$EMPSTAT == 1, 1, ifelse(data$EMPSTAT %in% c(2,3), 0, NA))
data$child5 <- ifelse(data$NCHLT5 > 0, 1, 0)


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
cities2005donorpool <- subset(geographicdata2005, geographicdata2005$Place.2000.Population > 500000 & geographicdata2005$Percent.PUMA.Population > 0.75)
cities2012donorpool <- subset(geographicdata2012, geographicdata2012$Place.2010.Population > 500000 & geographicdata2012$Percent.PUMA.Population > 0.75)

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

# Ensure data has the necessary columns and convert YEAR to numeric if needed
nyc <- donorpool %>%
  filter(Place.Name == "New York city") %>%
  mutate(
    eligible = ifelse(child5 == 1, 1, 0),
    post = ifelse(YEAR >= 2014, 1, 0),
    treat_post = eligible * post
  )

# Calculate participation rate by year and eligibility
nyc_summary <- nyc %>%
  group_by(YEAR, eligible, post, treat_post) %>%
  summarise(
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    .groups = 'drop'
  )

# Fitting the Difference-in-Differences model
did_model_nyc <- lm(part_rate ~ eligible + post + treat_post, data = nyc_summary)

# Summarizing the results
did_results_nyc <- tidy(did_model_nyc)
print(did_results_nyc)

# Extracting and interpreting coefficients
did_coef_nyc <- did_results_nyc %>% filter(term == "treat_post")
print(did_coef_nyc)

# Visualizing results 
plot_did_nyc <- ggplot(data = nyc_summary, aes(x = YEAR, y = part_rate, color = as.factor(eligible))) +
  geom_line() +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "blue") + # Line indicating treatment start
  annotate("text", x = 2012, y = max(nyc_summary$part_rate), label = "Pre-treatment", hjust = 1, color = "blue") + # Pre-treatment label
  annotate("text", x = 2016, y = max(nyc_summary$part_rate), label = "Post-treatment", hjust = 0, color = "blue") + # Post-treatment label
  labs(x = "Year", y = "Participation Rate", color = "Eligible (Child < 5)") +
  ggtitle("Labor Force Participation Rate in NYC: Eligible vs Non-Eligible Mothers") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc_summary$YEAR), max(nyc_summary$YEAR), by = 1))

print(plot_did_nyc)
print(head(nyc_summary))

