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


################################################################################
# Creating Donor Pool
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

# donorpoolcities <- c("New York city", "Los Angeles city", "Chicago city", "Houston city", "Philadelphia city", "Phoenix city", "San Antonio city", "San Diego city", "Dallas city", "San Jose city", "Indianapolis city (balance)", "San Francisco city", "Columbus city", "Charlotte city", "Detroit city", "El Paso city", "Memphis city", "Seattle city", "Nashville-Davidson metropolitan government (balance)", "Denver city", "Louisville/Jefferson County metro government (balance)", "Portland city", "Las Vegas city", "Albuquerque city", "Tucson city", "Fresno city", "Sacramento city", "Long Beach city", "Kansas City city", "Mesa city", "Virginia Beach city", "Colorado Springs city", "Omaha city", "Raleigh city", "Cleveland city", "Oakland city", "Minneapolis city", "Wichita city", "San Juan zona urbana", "Arlington city", "Bakersfield city", "Urban Honolulu CDP", "Anaheim city", "Tampa city", "Aurora city", "Santa Ana city", "St. Louis city", "Pittsburgh city", "Corpus Christi city", "Riverside city")
# cities2005donorpool <- geographicdata2005[geographicdata2005$Place.Name %in% donorpoolcities,]
# cities2012donorpool <- geographicdata2012[geographicdata2012$Place.Name %in% donorpoolcities,]
# pumas2005donorpool <- cities2005donorpool$PUMA[cities2005donorpool$Percent.PUMA.Population > 0.75]
# pumas2012donorpool <- cities2012donorpool$PUMA[cities2012donorpool$Percent.PUMA.Population > 0.75]
# donorpool <- synth_data[(synth_data$PUMA %in% pumas2005donorpool & synth_data$YEAR < 2012) | (synth_data$PUMA %in% pumas2012donorpool & synth_data$YEAR >= 2012),]


################################################################################
# Labour Force Participation Rate
################################################################################

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

#Labor Force Participation Rate
part_rate_women_donorpool <- donorpool %>%
  group_by(YEAR, Place.Name) %>%
  summarise(ParticipationRate = sum(LABFORCE == 2) / sum(LABFORCE %in% c(1, 2))) %>%
  ungroup()  # Remove the grouping structure

# Join this back to the original dataframe
donorpool <- merge(donorpool, part_rate_women_donorpool, by = c("YEAR","Place.Name"),all.x = TRUE)


#Restrict sample to women with children under age of 5
donorpool_women5 <- donorpool[donorpool$NCHLT5 > 0,]

#Labor Force Participation Rate/ Employment Rate 
part_rate_women5_donorpool <- donorpool_women5 %>%
  group_by(YEAR, Place.Name) %>%
  summarise(
    ParticipationRate5 = sum(LABFORCE == 2) / sum(LABFORCE %in% c(1, 2)),
    EmploymentRate5 = sum(EMPSTAT == 1) / sum(EMPSTAT %in% c(1, 2)),
    .groups = 'drop' 
  )

donorpool_women5 <- merge(donorpool_women5, part_rate_women5_donorpool, by = c("YEAR","Place.Name"),all.x = TRUE)


#Run regressions for donorpool with women <5
model1 <- lm(ParticipationRate5 ~ NCHILD + NCHLT5 + AGE + EDUC + married + white + black + hispanic, data = donorpool_women5)
summary(model1)

#Create necessary variables: 
#Shares by cities 

#Share married
#Average age
#Average education (maybe adjust and create binary variables)
#Share white mothers
#Share black mothers
#Share hispanic mothers


synthcontrol_donorpool <- donorpool_women5 %>%
  group_by(Place.Name, YEAR) %>%
  summarise(
    married = mean(married, na.rm = TRUE),
    white = mean(white, na.rm = TRUE),
    black = mean(black, na.rm = TRUE),
    hispanic = mean(hispanic, na.rm = TRUE),
    education = mean(EDUC, na.rm = TRUE),
    age = mean(AGE, na.rm = TRUE),
    part_rate5 = mean(ParticipationRate5, na.rm = TRUE),
    emp_rate5 = mean(EmploymentRate5, na.rm = TRUE),
    hours = mean(UHRSWORK, na.rm = TRUE),
    .groups = "drop"  
  )

#Include lags

#2012
part_rate5_2012 <- synthcontrol_donorpool %>%
  filter(YEAR == 2012) %>%
  dplyr::select(Place.Name, part_rate5_2012 = part_rate5)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate5_2012, by = "Place.Name")





#2010
part_rate5_2010 <- synthcontrol_donorpool %>%
  filter(YEAR == 2010) %>%
  dplyr::select(Place.Name, part_rate5_2010 = part_rate5)

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate5_2010, by = "Place.Name")


#2007 values and creating a new dataframe to merge back
part_rate5_2007 <- synthcontrol_donorpool %>%
  filter(YEAR == 2007) %>%
  dplyr::select(Place.Name, part_rate5_2007 = part_rate5)  # Renaming the column for clarity

synthcontrol_donorpool <- synthcontrol_donorpool %>%
  left_join(part_rate5_2007, by = "Place.Name")


# Now, convert this factor to integers
synthcontrol_donorpool$city <- factor(synthcontrol_donorpool$Place.Name)
synthcontrol_donorpool$city <- as.numeric(synthcontrol_donorpool$city)

#Change format of data frame (necessary for command dataprep)
synthcontrol_donorpool <- as.data.frame(synthcontrol_donorpool)

# For now (STILL HAVE TO ADAPT, WHY IS THAT???)
cities_to_remove <- c(1, 19)
synthcontrol_donorpool <- synthcontrol_donorpool[!synthcontrol_donorpool$city %in% cities_to_remove, ]

synthcontrol_donorpool$city <- factor(synthcontrol_donorpool$Place.Name)
synthcontrol_donorpool$city <- as.numeric(synthcontrol_donorpool$city)

# Correct way to specify controls
controls_identifier <- setdiff(unique(synthcontrol_donorpool$city), 12)

#New York City is no. 19
# Corrected dataprep function
# Setting up the dataprep function correctly
dataprep.out <-
  dataprep(
    foo = synthcontrol_donorpool,
    predictors = c("age","married","white","hispanic","black","education","part_rate5_2007","part_rate5_2010","part_rate5_2012"),
    predictors.op = "mean",
    dependent = "part_rate5",
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
          Ylab = "Labor Force Participation",
          Xlab = "Year",
          Legend = c("NYC", "Synthetic NYC"),
          Main = "NYC vs Synthetic NYC")

gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1990,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between labor force participation in NYC and its synthetic version")


### Robustness Checks ### 
#Backdating



#Leave-one-out











#Placebo Tests
#Random City
dataprep.out <-
  dataprep(foo = synthcontrol_donorpool,
           predictors = c("age","married","white","hispanic","black","education","part_rate5_2007") ,
           predictors.op = "mean" ,
           time.predictors.prior = 2006:2013 ,
           dependent = "part_rate5",
           unit.variable = "city",
           time.variable = "YEAR",
           treatment.identifier = 5, # Change the ID to other unit that did NOT receive the treatment
           controls.identifier = c(1:4,6:18,20:33),
           time.optimize.ssr = 2006:2013,
           time.plot = 2006:2018
  )

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS"
)

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1990,
          Ylab = "Labor Force Participation",
          Xlab = "Year",
          Legend = c("Random", "Synthetic Random"),
          Main = "Random City vs Synthetic random city")

gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1990,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between labor force participation in random city and its synthetic version")



##All Placebos###

store <- matrix(NA,length(2006:2019),33)

colnames(store) <- unique(synthcontrol_donorpool$city)

# run placebo test
for(iter in 2:33)
{
  dataprep.out <-
    dataprep(foo = synthcontrol_donorpool,
             predictors = c("age","married","white","hispanic","black","education","part_rate5_2007") ,
             predictors.op = "mean" ,
             time.predictors.prior = 2006:2013 ,
             dependent = "part_rate5",
             unit.variable = "city",
             time.variable = "YEAR",
             treatment.identifier = iter,
             controls.identifier = c(2:33)[-iter+1],
             time.optimize.ssr = 2006:2013,
             time.plot = 2006:2019
    )
  # run synth
  synth.out <- synth(
    data.prep.obj = dataprep.out,
    method = "BFGS"
  )
  
  # store gaps
  store[,iter-1] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}


# now do figure
data <- store
rownames(data) <- 2006:2019

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 2006:2019
gap.end.pre  <- which(rownames(data)=="2012")

#  MSPE Pre-Treatment
mse        <-             apply(data[ gap.start:gap.end.pre,]^2,2,mean)
nyc.mse <- as.numeric(mse[19])
# Exclude states with 5 times higher MSPE than basque
data <- data[,mse<4*nyc.mse]
Cex.set <- .75

# Plot
plot(years,data[gap.start:gap.end,which(colnames(data)=="19")],
     ylim=c(-0.15,0.15),xlab="YEAR",
     xlim=c(2006,2019),ylab="Gap in Labor Force Participation Rate",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# Add lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

## Add Basque Line
lines(years,data[gap.start:gap.end,which(colnames(data)=="19")],lwd=2,col="black")

# Add grid
abline(v=2014,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)
legend("bottomright",legend=c("New York City","Control Cities"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
arrows(2013,-1.5,2014,-1.5,col="black",length=.1)
text(2013,-1.5,"Introduction universal pre-k",cex=Cex.set)
abline(v=2006)
abline(v=2019)
abline(h=-2)
abline(h=2)