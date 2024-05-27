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
library(sf)
library(SCtools)
library(parallel)
library(MSCMT)
library(broom)

#Sample Construction
rm(list = ls()) 

setwd("/Users/nic/Desktop/Thesis Data")


#################################################
### Elegible Mothers (4 y.o. children sample) ###
#################################################
data <- read_csv("100SyntheticControl_usefor_4yo.csv.gz")

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
data$hsgraduate <- ifelse(data$EDUCD_MOM == 63, 1,
                          ifelse(data$EDUC_MOM == 64, 1, 0))
data$somecollege = ifelse(data$EDUCD_MOM == 062, 1,
                          ifelse(data$EDUC_MOM == 063, 1,
                                 ifelse(data$EDUC_MOM == 064, 1, 0)))
data$somecollege = ifelse(data$EDUCD_MOM == 065, 1,
                          ifelse(data$EDUC_MOM == 071, 1,
                                 ifelse(data$EDUC_MOM == 081, 1, 0)))
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


donorpool$nyc <- ifelse(donorpool$Place.Name == "New York city",1,0)

nyc <- donorpool[donorpool$Place.Name == "New York city",]
not_nyc <- donorpool[!(donorpool$Place.Name == "New York city"),]


###Checks###
table(donorpool$YEAR, donorpool$Place.Name)

################################################################################
#Descriptive Comparison NYC vs all other cities in Donorpool#
################################################################################
# 
# # Create the survey design object, assuming 'weights' is the name of the weighting variable
# design <- svydesign(ids = ~1, data = donorpool, weights = ~PERWT)
# 
# # Calculate summaries for each group
# cities <- svyby(~EMPSTAT_MOM + UHRSWORK_MOM + fulltime + LABFORCE_MOM + INCWAGE_MOM, 
#                 ~YEAR + Place.Name, 
#                 design, 
#                 FUN = function(subdata) {
#                   data.frame(
#                     emp_rate = svymean(as.integer(EMPSTAT_MOM == 1), subdata, na.rm = TRUE),
#                     hours = svymean(UHRSWORK_MOM[EMPSTAT_MOM == 1], subdata, na.rm = TRUE),
#                     fulltime = svyratio(~(EMPSTAT_MOM == 1 & fulltime == 1), ~(EMPSTAT_MOM == 1 & (fulltime == 0 | fulltime == 1)), subdata, na.rm = TRUE),
#                     part_rate = svyratio(~(LABFORCE_MOM == 2), ~(LABFORCE_MOM %in% c(1, 2)), subdata, na.rm = TRUE),
#                     income = svymean(INCWAGE_MOM[INCWAGE_MOM > 0], subdata, na.rm = TRUE)
#                   )
#                 },
#                 na.rm = TRUE,
#                 deff = FALSE)
# 
# # To format the results as a data frame for easier use:
# cities <- as.data.frame(cities)




# cities <- donorpool %>%
#   group_by(YEAR, Place.Name) %>%
#   summarise(
#     emp_rate = mean(employment, na.rm = TRUE),
#     hours = mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], na.rm = TRUE),
#     fulltime = sum(EMPSTAT_MOM == 1 & fulltime == 1, na.rm = TRUE) / 
#       sum(EMPSTAT_MOM == 1 & (fulltime == 0 | fulltime == 1), na.rm = TRUE),
#     part_rate = sum(LABFORCE_MOM == 2, na.rm = TRUE) / 
#       sum(LABFORCE_MOM %in% c(1, 2), na.rm = TRUE),
#     income = mean(INCWAGE_MOM[INCWAGE_MOM > 0], na.rm = TRUE),
#     .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise 
#   )


cities <- donorpool %>%
  group_by(YEAR, Place.Name) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w =PERWT[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

cities <- cities %>%
  mutate(Region = "Singe Region")


cities_not_nyc <- not_nyc %>%
  group_by(YEAR) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w =PERWT[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )
cities_not_nyc <- cities_not_nyc %>%
  mutate(Region = "Average Donorpool Cities")


cities_nyc <- nyc %>%
  group_by(YEAR) %>%
  summarise(
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK_MOM[EMPSTAT_MOM == 1], w =PERWT[EMPSTAT_MOM == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime[EMPSTAT_MOM == 1], w = PERWT[EMPSTAT_MOM == 1], na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE_MOM[INCWAGE_MOM > 0], w = PERWT[INCWAGE_MOM > 0], na.rm = TRUE),
    .groups = 'drop'  # If using dplyr >= 1.0.0, drop grouping by default after summarise
  )

cities_nyc <- cities_nyc %>%
  mutate(Region = "NYC")

cities <- bind_rows(cities_nyc, cities_not_nyc)

#Plot graphs
plot_part_rate <- ggplot(data = cities) +
  geom_line(aes(x = YEAR, y = part_rate, color = Region)) +  
  labs(x = "Year", y = "Participation Rate", color = "Region") +
  ggtitle("Participation Rate of Mothers with children = 4yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(cities$YEAR), max(cities$YEAR), by = 1))
print(plot_part_rate)

plot_emp_rate <- ggplot(data = cities) +
  geom_line(aes(x = YEAR, y = emp_rate, color = Region)) +
  labs(x = "Year", y = "") +
  ggtitle("Employment Rate of Mothers with children = 4yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(cities$YEAR), max(cities$YEAR), by = 1))
plot(plot_emp_rate)

plot_inc <- ggplot(data = cities) +
  geom_line(aes(x = YEAR, y = income, color = Region)) +
  labs(x = "Year", y = "") +
  ggtitle("Wage Income of Employed Mothers with Children = 4yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc$YEAR), max(nyc$YEAR), by = 1))
plot(plot_inc)

plot_hours <- ggplot(data = cities) +
  geom_line(aes(x = YEAR, y = hours, color = Region)) +
  labs(x = "Year", y = "") +
  ggtitle("Hours Worked of Employed Mothers with Children = 4yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(nyc$YEAR), max(nyc$YEAR), by = 1))
plot(plot_hours)

plot_fulltime <- ggplot(data = cities) +
  geom_line(aes(x = YEAR, y = fulltime, color = Region)) +
  labs(x = "Year", y = "") +
  ggtitle("Fulltime Share of Employed Mothers with Children = 4yo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(cities$YEAR), max(cities$YEAR), by = 1))
plot(plot_fulltime)

grid.arrange(plot_part_rate, plot_emp_rate, plot_inc, plot_hours,ncol=2)


########################################
#Synthetic Control for eligible Women###
########################################

#Run regressions for donorpool add hispanic
model1 <- lm(laborforce ~ NCHILD_MOM + NCHLT5_MOM + AGE_MOM + hsdropout + hsgraduate + somecollege + min_bachelor + married + white + black , data = donorpool)
summary(model1)

#Create necessary variables: 
synthcontrol_donorpool <- donorpool %>%
  group_by(Place.Name, YEAR) %>%
  summarise(
    married = weighted.mean(married , w = PERWT, na.rm = TRUE),
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


###Include lags and averages
#2013
part_rate_2013 <- synthcontrol_donorpool %>%
  filter(YEAR == 2013) %>%
  dplyr::select(Place.Name, part_rate_2013 = part_rate)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate_2013, by = "Place.Name")

hours_2013 <- synthcontrol_donorpool %>%
  filter(YEAR == 2013) %>%
  dplyr::select(Place.Name, hours_2013 = hours)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(hours_2013, by = "Place.Name")

income_2013 <- synthcontrol_donorpool %>%
  filter(YEAR == 2013) %>%
  dplyr::select(Place.Name, income_2013 = income)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(income_2013, by = "Place.Name")


#2012
part_rate_2012 <- synthcontrol_donorpool %>%
  filter(YEAR == 2012) %>%
  dplyr::select(Place.Name, part_rate_2012 = part_rate)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate_2012, by = "Place.Name")

hours_2012 <- synthcontrol_donorpool %>%
  filter(YEAR == 2012) %>%
  dplyr::select(Place.Name, hours_2012 = hours)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(hours_2012, by = "Place.Name")

income_2012 <- synthcontrol_donorpool %>%
  filter(YEAR == 2012) %>%
  dplyr::select(Place.Name, income_2012 = income)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(income_2012, by = "Place.Name")


#2010
part_rate_2010 <- synthcontrol_donorpool %>%
  filter(YEAR == 2010) %>%
  dplyr::select(Place.Name, part_rate_2010 = part_rate)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate_2010, by = "Place.Name")

hours_2010 <- synthcontrol_donorpool %>%
  filter(YEAR == 2010) %>%
  dplyr::select(Place.Name, hours_2010 = hours)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(hours_2010, by = "Place.Name")

income_2010 <- synthcontrol_donorpool %>%
  filter(YEAR == 2010) %>%
  dplyr::select(Place.Name, income_2010 = income)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(income_2010, by = "Place.Name")


#2009
part_rate_2009 <- synthcontrol_donorpool %>%
  filter(YEAR == 2009) %>%
  dplyr::select(Place.Name, part_rate_2009 = part_rate)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate_2009, by = "Place.Name")

hours_2009 <- synthcontrol_donorpool %>%
  filter(YEAR == 2009) %>%
  dplyr::select(Place.Name, hours_2009 = hours)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(hours_2009, by = "Place.Name")

income_2009 <- synthcontrol_donorpool %>%
  filter(YEAR == 2009) %>%
  dplyr::select(Place.Name, income_2009 = income)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(income_2009, by = "Place.Name")


#2007 values and creating a new dataframe to merge back
part_rate_2007 <- synthcontrol_donorpool %>%
  filter(YEAR == 2007) %>%
  dplyr::select(Place.Name, part_rate_2007 = part_rate)  # Renaming the column for clarity

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate_2007, by = "Place.Name")

hours_2007 <- synthcontrol_donorpool %>%
  filter(YEAR == 2007) %>%
  dplyr::select(Place.Name, hours_2007 = hours)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(hours_2007, by = "Place.Name")

income_2007 <- synthcontrol_donorpool %>%
  filter(YEAR == 2007) %>%
  dplyr::select(Place.Name, income_2007 = income)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(income_2007, by = "Place.Name")


#Include average labor force participation rate (pre-intervention)
part_rate_average <- synthcontrol_donorpool %>%
  filter(YEAR <= 2013) %>%
  group_by(Place.Name) %>%
  summarise(part_rate_average = mean(part_rate, na.rm = TRUE))

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate_average, by = "Place.Name")

hours_average <- synthcontrol_donorpool %>%
  filter(YEAR <= 2013) %>%
  group_by(Place.Name) %>%
  summarise(hours_average = mean(hours, na.rm = TRUE))

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(hours_average, by = "Place.Name")

income_average <- synthcontrol_donorpool %>%
  filter(YEAR <= 2013) %>%
  group_by(Place.Name) %>%
  summarise(income_average = mean(income, na.rm = TRUE))

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(income_average, by = "Place.Name")



# Now, convert this factor to integers
synthcontrol_donorpool$city <- factor(synthcontrol_donorpool$Place.Name)
synthcontrol_donorpool$city <- as.numeric(synthcontrol_donorpool$city)

#Change format of data frame (necessary for command dataprep)
synthcontrol_donorpool <- as.data.frame(synthcontrol_donorpool)

# Correct way to specify controls
controls_identifier <- setdiff(unique(synthcontrol_donorpool$city), 13)

#New York City is no. 13
#Labor Force Participation
dataprep.out <-
  dataprep(
    foo = synthcontrol_donorpool,
    predictors = c("age", "married", "white", "black", "share_hsgraduate", "share_hsdropout", "share_somecollege", "share_min_bachelor", "no_children", "part_rate_2007", "part_rate_2010", "part_rate_2013","part_rate_average"), #"part_rate_2007","part_rate_2010","part_rate_2012"#add "hispanic"
    predictors.op = "mean",
    dependent = "part_rate",
    unit.variable = "city",
    time.variable = "YEAR",
    treatment.identifier = 13,
    controls.identifier = controls_identifier,
    time.predictors.prior = 2006:2013,
    time.optimize.ssr = 2006:2013,
    unit.names.variable = "Place.Name",  
    time.plot = 2006:2019
  )

synth.out <- synth(dataprep.out)
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res    = synth.out
)

gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
gaps[1:14, 1]
gaps

synth.tables$tab.w
synth.tables$tab.v
synth.tables$tab.pred

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1990,
          Ylab = "Labor Force Participation",
          Xlab = "Year",
          Legend = c("NYC", "Synthetic NYC"),
          Main = "Mothers of 4y.o. (NYC vs Synthetic NYC)")
abline(v=2014,lty="dotted",lwd=2)

gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1990,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between labor force participation in NYC and its synthetic version")
abline(v=2014,lty="dotted",lwd=2)


######################################################
### DiD to get estimates of synthetic Control Method###
######################################################

control <- as.data.frame(dataprep.out$Y0plot %*% synth.out$solution.w)
names(control)[1] <- "part_rate"
control$YEAR <- 2006:2019
control$eligible <- 0
control$post <- ifelse(control$YEAR >= 2014, 1, 0)
control$treat_post <- control$eligible * control$post

nyc <- synthcontrol_donorpool %>%
  filter(city == 13) %>%
  select(YEAR, part_rate) %>%
  mutate(eligible = 1, post = ifelse(YEAR >= 2014, 1, 0), treat_post = eligible * post)

did <- bind_rows(control, nyc)
row.names(did) <- NULL

# Dummies == 1, otherwise 0, for each post-treatment year respectively.
for (year in 2014:2019) {
  did[[paste0("post_", year)]] <- ifelse(did$YEAR == year & did$post == 1, 1, 0)
}

# Dummies == 1, otherwise 0, for rollout and full effect periods respectively.
did <- did %>%
  mutate(rollout = ifelse(YEAR %in% c(2014, 2015, 2016), 1, 0))
did <- did %>%
  mutate(established = ifelse(YEAR %in% c(2017, 2018, 2019), 1, 0))
did <- did %>%
  mutate(rollout_established = rollout * established)

# DiD estimation.
did_model <- lm(part_rate ~ eligible + post + treat_post, data = did)
did_results <- broom::tidy(did_model)
print(did_results)
did_coef <- did_results %>% filter(term == "treat_post")
print(did_coef)

######################################################
# Dynamic SCM Model
######################################################

dynamic_did_model <- lm(part_rate ~ eligible * (post_2014 + post_2015 + post_2016 + post_2017 + post_2018 + post_2019), data = did)

dynamic_did_results <- tidy(dynamic_did_model)
print(dynamic_did_results)
dynamic_did_coef <- dynamic_did_results %>% filter(grepl("eligible:post_", term))
print(dynamic_did_coef)

######################################################
# Dynamic DiD coefficients calculated for rollout period and established period. 
######################################################

dynamic_2period <- lm(part_rate ~ eligible * (rollout + established), data = did)

dynamic_2period_results <- tidy(dynamic_2period)
print(dynamic_2period_results)

######################################################
# Plots
######################################################

plot_dynamic_did <- ggplot(data = dynamic_did_coef, aes(x = as.numeric(gsub("post_", "", term)), y = estimate)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  labs(x = "Year", y = "Dynamic DiD Estimate", title = "Dynamic DiD Estimates for Labor Force Participation") +
  theme_minimal()

# Visualize the results
plot_did <- ggplot(data = did, aes(x = YEAR, y = part_rate, color = as.factor(eligible))) +
  geom_line() +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "blue") + 
  annotate("text", x = 2011, y = max(did$part_rate), label = "Pre-treatment", hjust = 1, color = "blue") + 
  annotate("text", x = 2016, y = max(did$part_rate), label = "Post-treatment", hjust = 0, color = "blue") + 
  labs(x = "Year", y = "Participation Rate", color = "NYC") +
  ggtitle("Labor Force Participation Mothers with 4yo (NYC vs Donorpool)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(did$YEAR), max(did$YEAR), by = 1))

print(plot_did)

