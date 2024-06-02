rm(list = ls())

library(dplyr)
library(ggplot2)
library(readr)
library(openxlsx)

setwd("C:/Users/ru/Downloads")

data <- read_csv("C:/Users/ru/Downloads/100SyntheticControl_usefor_4yo.csv.gz.csv.gz")

# Data preparation
data <- data %>%
  distinct(SERIAL, YEAR, .keep_all = TRUE) %>%
  filter(AGE %in% c(3, 4, 5, 7), 
         !(AGE == 3 & BIRTHQTR %in% c(1, 2, 3)), 
         YEAR < 2020, 
         EDUCD != 12)

data$STATEFIP <- sprintf("%02d", data$STATEFIP)
data$PUMA <- sprintf("%05d", data$PUMA)
data$UPUMA <- paste0(data$STATEFIP,"_", data$PUMA)


# Create unique/state-independent PUMA
data <- data %>%
  mutate(
    treatment = ifelse(AGE == 4, 0, 1),
    laborforce = ifelse(LABFORCE_MOM == 2, 1, ifelse(LABFORCE_MOM == 1, 0, NA)),
    employment = ifelse(EMPSTAT_MOM == 1, 1, ifelse(EMPSTAT_MOM %in% c(2, 3), 0, NA)),
    fulltime = ifelse(UHRSWORK_MOM >= 30, 1, 0),
    no_children = ifelse(NCHILD_MOM >= 4, 4, NCHILD_MOM),
    race = ifelse(RACE_MOM >= 3, 3, RACE_MOM),
    married = ifelse(MARST_MOM <= 2, 1, 0),
    white = ifelse(RACE_MOM == 1, 1, 0),
    black = ifelse(RACE_MOM == 2, 1, 0),
    INCWAGE_MOM = ifelse(INCWAGE_MOM == 999999, NA, INCWAGE_MOM),
    highschool = ifelse(EDUCD_MOM <= 61, 1, ifelse(EDUCD_MOM <= 64, 1, 0)),
    somecollege = ifelse(EDUCD_MOM %in% c(65, 71, 81), 1, 0),
    min_bachelor = ifelse(EDUCD_MOM > 100, 1, 0)
  )

data <- data %>%
  mutate(
    hhincome = case_when(
      POVERTY_MOM <= 100 ~ 1,
      POVERTY_MOM <= 300 ~ 2,
      TRUE ~ 3
    )
  )

data <- data %>%
  mutate(
    education = case_when(
      EDUCD_MOM <= 61 ~ 1,
      EDUCD_MOM <= 100 ~ 1,
      TRUE ~ 3
    )
  )

# Plot 1: Labour Force Participation over time for New York and Non-New York
data_ny <- data %>% filter(STATEFIP == "36")
data_non_ny <- data %>% filter(STATEFIP != "36")

lfp_ny <- data_ny %>%
  group_by(YEAR) %>%
  summarise(lfp_rate = mean(laborforce, na.rm = TRUE))

lfp_non_ny <- data_non_ny %>%
  group_by(YEAR) %>%
  summarise(lfp_rate = mean(laborforce, na.rm = TRUE))

lfp_data <- lfp_ny %>%
  mutate(Location = "New York") %>%
  bind_rows(lfp_non_ny %>% mutate(Location = "Non-New York"))

ggplot(lfp_data, aes(x = YEAR, y = lfp_rate, color = Location)) +
  geom_line() +
  labs(title = "Labour Force Participation Over Time",
       x = "Year",
       y = "Labour Force Participation Rate",
       color = "Location") +
  theme_minimal()

# Plot 2: Labour Force Participation of mothers with 4yo children in New York versus Non-New York
data_4yo <- data %>% filter(AGE == 4)

lfp_4yo_ny <- data_4yo %>%
  filter(STATEFIP == "36") %>%
  group_by(YEAR) %>%
  summarise(lfp_rate = mean(laborforce, na.rm = TRUE))

lfp_4yo_non_ny <- data_4yo %>%
  filter(STATEFIP != "36") %>%
  group_by(YEAR) %>%
  summarise(lfp_rate = mean(laborforce, na.rm = TRUE))

lfp_4yo_data <- lfp_4yo_ny %>%
  mutate(Location = "New York") %>%
  bind_rows(lfp_4yo_non_ny %>% mutate(Location = "Non-New York"))

ggplot(lfp_4yo_data, aes(x = YEAR, y = lfp_rate, color = Location)) +
  geom_line() +
  labs(title = "Labour Force Participation of Mothers with 4yo Children",
       x = "Year",
       y = "Labour Force Participation Rate",
       color = "Location") +
  theme_minimal()

# Plot 3: Differentiating between married and unmarried mothers in New york vs Non-New York
lfp_4yo_married <- data_4yo %>%
  group_by(YEAR, STATEFIP, married) %>%
  summarise(lfp_rate = mean(laborforce, na.rm = TRUE)) %>%
  mutate(Status = ifelse(married == 1, "Married", "Unmarried"))

lfp_4yo_married_ny <- lfp_4yo_married %>% filter(STATEFIP == "36")
lfp_4yo_married_non_ny <- lfp_4yo_married %>% filter(STATEFIP != "36")

lfp_4yo_married_data <- lfp_4yo_married_ny %>%
  mutate(Location = "New York") %>%
  bind_rows(lfp_4yo_married_non_ny %>% mutate(Location = "Non-New York"))

ggplot(lfp_4yo_married_data, aes(x = YEAR, y = lfp_rate, color = interaction(Status, Location))) +
  geom_line() +
  labs(title = "Labour Force Participation of Mothers with 4yo Children (Married vs. Unmarried)",
       x = "Year",
       y = "Labour Force Participation Rate",
       color = "Status and Location") +
  theme_minimal()

# Plot 4: Differentiating between below and above the poverty threshold in New York vs Non-New York
lfp_4yo_poverty <- data_4yo %>%
  group_by(YEAR, STATEFIP, hhincome) %>%
  summarise(lfp_rate = mean(laborforce, na.rm = TRUE)) %>%
  mutate(Status = case_when(
    hhincome == 1 ~ "Below Poverty",
    hhincome == 2 ~ "100-300% Poverty",
    hhincome == 3 ~ "Above 300% Poverty"
  ))

lfp_4yo_poverty_ny <- lfp_4yo_poverty %>% filter(STATEFIP == "36")
lfp_4yo_poverty_non_ny <- lfp_4yo_poverty %>% filter(STATEFIP != "36")

lfp_4yo_poverty_data <- lfp_4yo_poverty_ny %>%
  mutate(Location = "New York") %>%
  bind_rows(lfp_4yo_poverty_non_ny %>% mutate(Location = "Non-New York"))

ggplot(lfp_4yo_poverty_data, aes(x = YEAR, y = lfp_rate, color = interaction(Status, Location))) +
  geom_line() +
  labs(title = "Labour Force Participation of Mothers with 4yo Children (By Poverty Status)",
       x = "Year",
       y = "Labour Force Participation Rate",
       color = "Poverty Status and Location") +
  theme_minimal()
  
#OR
threshold <- 100 # Replace with the actual threshold if different

lfp_4yo_poverty <- data_4yo %>%
  group_by(YEAR, STATEFIP, hhincome) %>%
  summarise(lfp_rate = mean(laborforce, na.rm = TRUE)) %>%
  mutate(Status = ifelse(hhincome == 1, "Below Poverty", "Above Poverty"))

lfp_4yo_poverty_ny <- lfp_4yo_poverty %>% filter(STATEFIP == "36")
lfp_4yo_poverty_non_ny <- lfp_4yo_poverty %>% filter(STATEFIP != "36")

lfp_4yo_poverty_data <- lfp_4yo_poverty_ny %>%
  mutate(Location = "New York") %>%
  bind_rows(lfp_4yo_poverty_non_ny %>% mutate(Location = "Non-New York"))

ggplot(lfp_4yo_poverty_data, aes(x = YEAR, y = lfp_rate, color = interaction(Status, Location))) +
  geom_line() +
  labs(title = "Labour Force Participation of Mothers with 4yo Children (Below vs. Above Poverty Threshold)",
       x = "Year",
       y = "Labour Force Participation Rate",
       color = "Poverty Status and Location") +
  theme_minimal()

