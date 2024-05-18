# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(openxlsx)

# Remove all objects from the workspace
rm(list = ls()) 

# Set working directory
setwd("C:/Users/ru/Downloads")

# Load data
data <- read_csv("C:/Users/ru/Downloads/100SyntheticControl_usefor_below5yo.csv.gz")
data <- data[data$YEAR < 2020,]

# Data Cleaning 
data <- data[!is.na(data$EMPSTAT) & !is.na(data$LABFORCE) & data$INCWAGE != 999999,]

# Add additional variables
data$laborforce <- ifelse(data$LABFORCE == 2, 1, ifelse(data$LABFORCE == 1, 0, NA))
data$employment <- ifelse(data$EMPSTAT == 1, 1, ifelse(data$EMPSTAT %in% c(2,3), 0, NA))
data$child5 <- ifelse(data$NCHLT5 > 0, 1, 0)

# Load geographic data
geographicdata2012 <- read.xlsx("C:/Users/rusha/Downloads/2012-2021 PUMAS Cities.xlsx", sheet = 1)
geographicdata2012$PUMA <- sub("^0", "", geographicdata2012$PUMA)
geographicdata2005 <- read.xlsx("C:/Users/rusha/Downloads/2005-2011 PUMAS Citites.xlsx", sheet = 1)
geographicdata2005$PUMA <- sub("^0", "", geographicdata2005$PUMA)

# Filter cities with >500'000 population and >75% PUMA zone coverage
cities2005donorpool <- subset(geographicdata2005, Place.2000.Population > 500000 & Percent.PUMA.Population > 0.75)
cities2012donorpool <- subset(geographicdata2012, Place.2010.Population > 500000 & Percent.PUMA.Population > 0.75)

# Keep cities in 2012 that had >500'000 population in 2000
metropolitan_cities <- unique(cities2005donorpool$Place.Name)
cities2012donorpool <- subset(cities2012donorpool, Place.Name %in% metropolitan_cities)

# Remove cities with major pre-K policies
major_preK <- c("Washington city", "Baltimore city", "Jacksonville city", "Oklahoma City city", "Milwaukee city", "Fort Worth city", "Austin city", "Boston city")
cities2005donorpool <- subset(cities2005donorpool, !(Place.Name %in% major_preK))
cities2012donorpool <- subset(cities2012donorpool, !(Place.Name %in% major_preK))

# Extract PUMA codes and filter donor pool
pumas2005donorpool <- cities2005donorpool$PUMA
pumas2012donorpool <- cities2012donorpool$PUMA
donorpool <- data[(data$PUMA %in% pumas2005donorpool & data$YEAR < 2012) | (data$PUMA %in% pumas2012donorpool & data$YEAR >= 2012),]

# Add city names to donor pool
donorpool$PUMA <- as.character(donorpool$PUMA)
cities2005donorpool$PUMA <- as.character(cities2005donorpool$PUMA)

donorpool <- bind_rows(
  donorpool %>% 
    filter(YEAR <= 2011) %>%
    left_join(dplyr::select(cities2005donorpool, PUMA, Place.Name), by = "PUMA"),
  
  donorpool %>% 
    filter(YEAR >= 2012) %>%
    left_join(dplyr::select(cities2012donorpool, PUMA, Place.Name), by = "PUMA")
)

donorpool$nyc <- ifelse(donorpool$Place.Name == "New York city", 1, 0)

# Filter NYC data
nyc <- donorpool[donorpool$Place.Name == "New York city",]


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
    part_rate = sum(LABFORCE == 2) / sum(LABFORCE %in% c(1, 2)),
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

