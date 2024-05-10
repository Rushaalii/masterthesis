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


# Remove all objects from the workspace
rm(list=ls())
setwd("/Users/nic/Desktop/Thesis Data")

synth_data <- read_csv("50SyntheticControlWomen.csv")


#Add new variables (add poverty!!!)
synth_data$married <- ifelse (synth_data$MARST <= 2, 1, 0)
synth_data$white <- ifelse (synth_data$RACE == 1, 1, 0)
synth_data$black <- ifelse (synth_data$RACE == 2, 1, 0)
synth_data$hispanic <- ifelse (synth_data$HISPAN != 0,1,0)
synth_data$LABFORCE <- ifelse(synth_data$LABFORCE == 2, 1,
                             ifelse(synth_data$LABFORCE == 1, 0,
                                    ifelse(synth_data$LABFORCE == 0, NA, NA)))

################################################################################
# Donor Pool
################################################################################

# Data Loading and Cleaning
geographicdata2012 <- read.xlsx("2012-2021 PUMAS Cities.xlsx", sheet = 1)
geographicdata2012$PUMA <- sub("^0", "", geographicdata2012$PUMA)
geographicdata2012$Place.Name <- ifelse(geographicdata2012$Place.Name == "Nashville-Davidson metropolitan government (balance)", "Nashville-Davidson (balance)", geographicdata2012$Place.Name)
geographicdata2012$X13 <- NULL
geographicdata2005 <- read.xlsx("2005-2011 PUMAS Citites.xlsx", sheet = 1)
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
donorpool <- synth_data[(synth_data$PUMA %in% pumas2005donorpool & synth_data$YEAR < 2012) | (synth_data$PUMA %in% pumas2012donorpool & synth_data$YEAR >= 2012),]

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

# Exclude women without children
donorpool <- donorpool[donorpool$NCHILD > 0, ]

################################################################################
# Labour Force Participation Rate
################################################################################

# Difference LFPR eligible and non-eligible women
diff_LBPR <- donorpool %>%
  group_by(YEAR, Place.Name) %>%
  summarise(
    ParticipationRate1 = sum(LABFORCE == 1, na.rm = TRUE) / sum(LABFORCE %in% c(1, 0), na.rm = TRUE),
    LBPR_eligible = sum(LABFORCE[NCHLT5 > 0] == 1, na.rm = TRUE) / sum(LABFORCE[NCHLT5 > 0] %in% c(1, 0), na.rm = TRUE),
    LBPR_ineligible = sum(LABFORCE[NCHLT5 == 0] == 1, na.rm = TRUE) / sum(LABFORCE[NCHLT5 == 0] %in% c(1, 0), na.rm = TRUE),
    LBPR_diff = LBPR_eligible - LBPR_ineligible) %>%
  ungroup()

donorpool <- merge(donorpool, diff_LBPR, by = c("YEAR", "Place.Name"))

donorpool$HighSchoolGrad <- ifelse(donorpool$EDUC == 6, 1,
                                  ifelse(donorpool$EDUC == 7, 1,
                                         ifelse(donorpool$EDUC == 8, 1,
                                                ifelse(donorpool$EDUC == 9, 1,
                                                       ifelse(donorpool$EDUC == 10, 1,
                                                              ifelse(donorpool$EDUC == 11, 1, 0))))))
donorpool$CollegeGrad <- ifelse(donorpool$EDUC == 10, 1,
                                ifelse(donorpool$EDUC == 11, 1, 0))

donorpool$SomeCollege <- ifelse(donorpool$EDUC == 7, 1,
                                  ifelse(donorpool$EDUC == 8, 1,
                                            ifelse(donorpool$EDUC == 9, 1, 0)))

# Identify predictors of LBPR_diff

donorpool_pre_interv <- donorpool[donorpool$YEAR < 2014, ]
donorpool_pre_interv_women5 <- donorpool_pre_interv[donorpool_pre_interv$NCHLT5 > 0, ]
donorpool_women5 <- donorpool[donorpool$NCHLT5 > 0, ]

model_1 <- lm(LABFORCE ~ NCHILD + NCHLT5 + AGE + HighSchoolGrad + CollegeGrad + SomeCollege + married + white + black + hispanic, data = donorpool)
summary(model_1)

model_2 <- lm(LABFORCE ~ NCHILD + NCHLT5 + AGE + HighSchoolGrad + CollegeGrad + SomeCollege + married + white + black + hispanic, data = donorpool_pre_interv)
summary(model_2)

model_3 <- lm(LABFORCE ~ NCHILD + NCHLT5 + AGE + HighSchoolGrad + CollegeGrad + SomeCollege + married + white + black + hispanic, data = donorpool_pre_interv_women5)
summary(model_3)


# Create Variables: 

synthcontrol_donorpool <- donorpool_women5 %>%
  group_by(Place.Name, YEAR) %>%
  summarise(
    married = mean(married, na.rm = TRUE),
    white = mean(white, na.rm = TRUE),
    black = mean(black, na.rm = TRUE),
    hispanic = mean(hispanic, na.rm = TRUE),
    education = mean(EDUC, na.rm = TRUE),
    age = mean(AGE, na.rm = TRUE),
    LBPR_diff = mean(LBPR_diff, na.rm = TRUE),
    hours = mean(UHRSWORK, na.rm = TRUE),
    nchild5 = mean(NCHLT5, na.rm = TRUE),
    nchild = mean(NCHILD, na.rm = TRUE),
    .groups = "drop"  
  )


#2012
diff_LBPR_2012 <- synthcontrol_donorpool %>%
  filter(YEAR == 2012) %>%
  dplyr::select(Place.Name, diff_LBPR_2012 = LBPR_diff)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(diff_LBPR_2012, by = "Place.Name")

#2010
diff_LBPR_2010 <- synthcontrol_donorpool %>%
  filter(YEAR == 2010) %>%
  dplyr::select(Place.Name, diff_LBPR_2010 = LBPR_diff)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(diff_LBPR_2010, by = "Place.Name")


#2007 values and creating a new dataframe to merge back
diff_LBPR_2007 <- synthcontrol_donorpool %>%
  filter(YEAR == 2007) %>%
  dplyr::select(Place.Name, diff_LBPR_2007 = LBPR_diff)  # Renaming the column for clarity

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(diff_LBPR_2007, by = "Place.Name")



# Now, convert this factor to integers
synthcontrol_donorpool$city <- factor(synthcontrol_donorpool$Place.Name)
synthcontrol_donorpool$city <- as.numeric(synthcontrol_donorpool$city)

#Change format of data frame (necessary for command dataprep)
synthcontrol_donorpool <- as.data.frame(synthcontrol_donorpool)

# For now (STILL HAVE TO ADAPT, WHY IS THAT???)
cities_to_remove <- c(1, 19)
synthcontrol_donorpool <- synthcontrol_donorpool[!synthcontrol_donorpool$city %in% cities_to_remove, ]

# Correct way to specify controls
controls_identifier <- setdiff(unique(synthcontrol_donorpool$city), 12)

#New York City is no. 12
# "diff_LBPR_2012", "diff_LBPR_2010", "diff_LBPR_2007",  Corrected dataprep function
# Setting up the dataprep function correctly
dataprep.out <-
  dataprep(
    foo = synthcontrol_donorpool,
    predictors = c("age","married","white","hispanic", "diff_LBPR_2012", "diff_LBPR_2010", "diff_LBPR_2007", "black","education", "nchild5", "nchild"),
    predictors.op = "mean",
    dependent = "LBPR_diff",
    unit.variable = "city",
    time.variable = "YEAR",
    treatment.identifier = 12,
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

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1990,
          Ylab = "Diff LBPR",
          Xlab = "Year",
          Legend = c("NYC", "Synthetic NYC"),
          Main = "NYC vs Synthetic NYC")

gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1990,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between labor force participation in NYC and its synthetic version")
