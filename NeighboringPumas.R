library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
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
library(tigris)
library(tidycensus)
library(stringr)
library(ipumsr)
library(survey)
library(tidyverse)
library(broom)

rm(list = ls()) 

# Set options for tigris to use sf class and cache downloads
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Download PUMA data for New York State
ny_pumas <- pumas(state = "NY", year = 2019, cb = TRUE)
ny_pumas <- st_transform(ny_pumas, crs = 3857)  # Transform for accurate distance measurements


# Filter NYC PUMAs based on known codes
nyc_puma_codes <- c("03701", "03702", "03703", "03704", "03705", "03706", "03707", "03708", "03709", "03710",
                    "03801", "03802", "03803", "03804", "03805", "03806", "03807", "03808", "03809", "03810",
                    "03901", "03902", "03903", "04001", "04002", "04003", "04004", "04005", "04006", "04007",
                    "04008", "04009", "04010", "04011", "04012", "04013", "04014", "04015", "04016", "04017",
                    "04018", "04101", "04102", "04103", "04104", "04105", "04106", "04107", "04108", "04109",
                    "04110", "04111", "04112", "04113", "04114")

nyc_pumas <- ny_pumas %>% filter(PUMACE10 %in% nyc_puma_codes)

# Create a 10 km buffer around NYC PUMAs
nyc_pumas_buffered <- st_buffer(nyc_pumas, dist = 30000)

# Find PUMAs that intersect the buffered area using st_intersects
intersection_matrix <- st_intersects(nyc_pumas_buffered, ny_pumas)
surrounding_pumas_indices <- apply(intersection_matrix, 2, any)  # Check for any TRUE in each column
surrounding_pumas <- ny_pumas[surrounding_pumas_indices, ]

# Exclude NYC PUMAs from the results
surrounding_pumas <- surrounding_pumas[!surrounding_pumas$PUMACE10 %in% nyc_puma_codes, ]

# Plot results
ggplot() +
  geom_sf(data = ny_pumas, fill = "white", color = "black") +
  geom_sf(data = nyc_pumas, fill = "blue") +
  geom_sf(data = surrounding_pumas, fill = "red") +
  theme_minimal() +
  ggtitle("NYC PUMAs and Surrounding Areas within 10 km")


pumas_surrounding <- surrounding_pumas$PUMACE10
nyc_pumas <- nyc_pumas$PUMACE10


#################################################
### Elegible Mothers (4 y.o. children sample) ###
#################################################
setwd("/Users/tilpommer/Documents/BSE/Term 3/Master project/Synthetic Control/Preschool: PreK Programs")
data <- read_csv("/Users/tilpommer/Documents/BSE/Term 3/Master project/Results/100SyntheticControl_usefor_4yo.csv.gz")

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
data$married <- ifelse (data$MARST_MOM == 1, 1, 0) #Only married where partner is present!
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

################################################################################
# Diff-in-Diff 4yo 
################################################################################

donorpool <- data[(data$PUMA %in% pumas_surrounding) | (data$PUMA %in% nyc_pumas),]
donorpool <- donorpool[donorpool$AGE != 7,]

donorpool$treatment <- ifelse(donorpool$PUMA %in% nyc_pumas,1,0)
donorpool$time <- ifelse(donorpool$YEAR >= 2014, 1, 0)
donorpool$did <- donorpool$treatment * donorpool$time



# Calculate participation rate by year and eligibility
summary <- donorpool %>%
  group_by(YEAR, did, time, treatment) %>%
  summarise(
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    .groups = 'drop'
  )


# Fit the difference-in-differences model using lm
did_model <- lm(part_rate ~ treatment + time + did, data = summary)

# Display the summary of the model
summary(did_model)

did_results <- tidy(did_model)
print(did_results_nyc)

# Extracting and interpreting coefficients
did_coef <- did_results %>% filter(term == "did")
print(did_coef)

# Visualizing results 
plot_did <- ggplot(data = summary, aes(x = YEAR, y = part_rate, color = as.factor(treatment))) +
  geom_line() +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "blue") + # Line indicating treatment start
  annotate("text", x = 2012, y = max(summary$part_rate), label = "Pre-treatment", hjust = 1, color = "blue") + # Pre-treatment label
  annotate("text", x = 2016, y = max(summary$part_rate), label = "Post-treatment", hjust = 0, color = "blue") + # Post-treatment label
  labs(x = "Year", y = "Participation Rate", color = "NYC vs Surrounding Areas") +
  ggtitle("Labor Force Participation of elegible Mothers: NYC vs Surrounding Areas") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(summary$YEAR), max(summary$YEAR), by = 1))

print(plot_did)
print(head(summary))





