#devtools::install_github("synth-inference/synthdid")

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
library(synthdid)

# Remove all objects from the workspace
rm(list = ls()) 

### SCRIPT for donorpool & LFP 

setwd("/Users/tilpommer/Documents/BSE/Term 3/Master project/Results")

synth_data <- read_csv("/Users/tilpommer/Documents/BSE/Term 3/Master project/Results/100SyntheticControl_usefor_below5yo.csv.gz")

# Restrict until 2019
synth_data$YEAR <- as.numeric(as.character(synth_data$YEAR))

synth_data <- synth_data[synth_data$YEAR < 2020,]

# Create unique/state-independent PUMA
synth_data$STATEFIP <- sprintf("%02d", synth_data$STATEFIP)
synth_data$PUMA <- sprintf("%05d", synth_data$PUMA)
synth_data$UPUMA <- paste0(synth_data$STATEFIP,"_", synth_data$PUMA)

#Add new variables
synth_data$laborforce <- ifelse(synth_data$LABFORCE == 2, 1, ifelse(synth_data$LABFORCE == 1, 0, NA))
synth_data$employment <- ifelse(synth_data$EMPSTAT == 1, 1, ifelse(synth_data$EMPSTAT %in% c(2, 3), 0, NA))
synth_data$fulltime <- ifelse(synth_data$UHRSWORK >= 30, 1, 0)
synth_data$no.children <- ifelse(synth_data$NCHILD >= 4, 4, synth_data$NCHILD)
synth_data$race <- ifelse(synth_data$RACE >= 3, 3, synth_data$RACE)
synth_data$married <- ifelse(synth_data$MARST <= 2, 1, 0)
synth_data$white <- ifelse(synth_data$RACE == 1, 1, 0)
synth_data$black <- ifelse(synth_data$RACE == 2, 1, 0)
synth_data$INCWAGE[synth_data$INCWAGE == 999999] <- NA

synth_data$hsdropout <- ifelse(synth_data$EDUCD <= 061, 1, 0)
synth_data$hsgraduate <- ifelse(synth_data$EDUCD >= 062 & synth_data$EDUCD < 065, 1, 0)
synth_data$somecollege <- ifelse(synth_data$EDUCD >= 065 & synth_data$EDUCD <= 100, 1, 0)
synth_data$min_bachelor <- ifelse(synth_data$EDUCD > 100, 1, 0)

# Adding the poverty variable
synth_data$poverty <- ifelse(synth_data$POVERTY == 1, 1, 0)



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
donorpool <- synth_data[(synth_data$UPUMA %in% upumas2005donorpool & synth_data$YEAR < 2012) | (synth_data$UPUMA %in% upumas2012donorpool & synth_data$YEAR >= 2012),]

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

#Restrict to women with children below 5
donorpool[donorpool$NCHLT5 > 0,]


################################################################################
# Synthetic Donorpool#
################################################################################

#Run regressions for donorpool with women <5
# model1 <- lm(laborforce ~ NCHILD + NCHLT5 + AGE + married + white + black + hsdropout + hsgraduate + somecollege + min_bachelor, data = donorpool)
# summary(model1)

#Create necessary variables: 
#Shares by cities 
#Create necessary variables: 
#Shares by cities 
synthcontrol_donorpool <- donorpool %>%
  group_by(Place.Name, YEAR) %>%
  summarise(
    married = weighted.mean(married, w = PERWT, na.rm = TRUE),
    white = weighted.mean(white, w = PERWT, na.rm = TRUE),
    black = weighted.mean(black, w = PERWT, na.rm = TRUE),
    share_hsdropout = weighted.mean(hsdropout, w = PERWT, na.rm = TRUE),
    share_hsgraduate = weighted.mean(hsgraduate, w = PERWT, na.rm = TRUE),
    share_somecollege = weighted.mean(somecollege, w = PERWT, na.rm = TRUE),
    share_min_bachelor = weighted.mean(min_bachelor, w = PERWT, na.rm = TRUE),
    age = weighted.mean(AGE, w = PERWT, na.rm = TRUE),
    part_rate = weighted.mean(laborforce, w = PERWT, na.rm = TRUE),
    emp_rate = weighted.mean(employment, w = PERWT, na.rm = TRUE),
    hours = weighted.mean(UHRSWORK[EMPSTAT == 1], w = PERWT[EMPSTAT == 1], na.rm = TRUE),
    fulltime = weighted.mean(fulltime, w = PERWT, na.rm = TRUE),
    income = weighted.mean(INCWAGE[INCWAGE > 0], w = PERWT[INCWAGE > 0], na.rm = TRUE),
    no_children = weighted.mean(no.children, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  )

########### Script for Synthetic Diff in Diff ############

# Create treatment indicator
synthcontrol_donorpool <- synthcontrol_donorpool %>%
  mutate(Treated = ifelse(Place.Name == "New York city" & YEAR >= 2014, 1, 0))

###Labor Force Participation Rate###

# Create dataset for SDID (Labor Force Participation Rate)
selected_variables <- synthcontrol_donorpool[, c("Place.Name", "YEAR", "part_rate", "Treated")]
data_SDID <- data.frame(selected_variables)


# Synthetic Diff in Diff
setup = panel.matrices(data_SDID) # converts data set from panel to matrix format required by synthdid estimators

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
lower_bound <- tau.hat - 1.96 * se
upper_bound <- tau.hat + 1.96 * se
CI <- c(lower_bound, upper_bound)
plot(tau.hat)


#Inserting more lines of control cities
estimate = synthdid_estimate(setup$Y, setup$N0, setup$T0)

top.controls = synthdid_controls(estimate)[1:10, , drop=FALSE]
plot(estimate, spaghetti.units=rownames(top.controls))


California = c('Los Angeles city', 'San Diego city', 'San Francisco city')
spaghetti.matrices = rbind(colMeans(setup$Y[rownames(setup$Y) %in% California, ]),
                           colMeans(setup$Y[rownames(setup$Y) %in% rownames(top.controls), ]))
rownames(spaghetti.matrices) = c('California', 'Top-10 Control Average')
plot(estimate, spaghetti.matrices=list(spaghetti.matrices), spaghetti.line.alpha=.4)

###Control Unit Contribution Plot###
synthdid_units_plot(estimate, units = rownames(top.controls))


#Weights Cities over Years
setup$Y

#Number Control Units
setup$N0









### Complex Plot, combining different estimates ###

#Includes DID and Synthetic Control Estimate for Comparison

#Synthetic Control
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
    predictors = c("age","married","white","black","share_hsdropout","share_hsgraduate","share_somecollege","share_min_bachelor","no_children"), #"part_rate_2007","part_rate_2010","part_rate_2012"#add "hispanic"
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


estimate.sc <- dataprep.out$Y0plot %*% synth.out$solution.w




estimate.sc = sc_estimate(setup$Y, setup$N0, setup$T0)
with.overlay = function(est, s) { attr(est,'overlay') = s; est }
estimators = function(s) {
  estimator.list = list(with.overlay(estimate, s), estimate.sc, estimate)
  names(estimator.list)=c('synth. diff-in-diff', 'synth. control', '')
  estimator.list
}

plot.estimators = function(ests, alpha.multiplier) {
  p = synthdid_plot(ests, se.method='none',
                    alpha.multiplier=alpha.multiplier, facet=rep(1,length(ests)),
                    trajectory.linetype = 1, effect.curvature=-.4,
                    trajectory.alpha=.5, effect.alpha=.5, diagram.alpha=1)
  suppressMessages(p + scale_alpha(range=c(0,1), guide='none'))
  # scale alpha so alpha=0 means totally invisible, which is unusual but useful
  # for hiding our invisible estimate. We have to suppress a warning that 
  # we're overriding an extant alpha scale that's added in synthdid_plot 
}

# set up the box we zoom in on in plot 5
lambda = attr(estimate, 'weights')$lambda
time = as.integer(timesteps(setup$Y))
xbox.ind = c(which(lambda > .01)[1], setup$T0+4)
xbox = time[xbox.ind] + c(-.5,.5)
ybox = range(setup$Y[setup$N0+1, min(xbox.ind):(max(xbox.ind))]) + c(-4,4)


p1 = plot.estimators(estimators(0),   alpha.multiplier=c(1,.1,0))
p2 = plot.estimators(estimators(.75), alpha.multiplier=c(1,.1,0))
p3 = plot.estimators(estimators(1),   alpha.multiplier=c(1,.1,0))
p4 = plot.estimators(estimators(1),   alpha.multiplier=c(1, 1,0))
p4.zoom = p4 + coord_cartesian(xlim=xbox, ylim=ybox) + xlab('') + ylab('') +
  theme(axis.ticks.x= element_blank(), axis.text.x = element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(),
        legend.position='off')
p5 = p4 + annotation_custom(ggplotGrob(p4.zoom), xmin = 1968,   # location by
                            xmax = 1984.7,   ymin=2, ymax=95) + # trial and error
  geom_rect(aes(xmin=min(xbox), xmax=max(xbox),
                ymin=min(ybox), ymax=max(ybox)),
            color=alpha('black', .25), size=.3, fill=NA)


plot.theme = theme(legend.position=c(.9,.85), legend.direction='vertical',
                   legend.key=element_blank(), legend.background=element_blank())

p1 + plot.theme



###Employment###

# Create dataset for SDID (Labor Force Participation Rate)
selected_variables <- synthcontrol_donorpool[, c("Place.Name", "YEAR", "emp_rate", "Treated")]
data_SDID <- data.frame(selected_variables)

setup = panel.matrices(data_SDID) # converts data set from panel to matrix format required by synthdid estimators

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)


###Income###

# Create dataset for SDID (Labor Force Participation Rate)
selected_variables <- synthcontrol_donorpool[, c("Place.Name", "YEAR", "income", "Treated")]
data_SDID <- data.frame(selected_variables)

setup = panel.matrices(data_SDID) # converts data set from panel to matrix format required by synthdid estimators

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)



###Hours###

# Create dataset for SDID (Labor Force Participation Rate)
selected_variables <- synthcontrol_donorpool[, c("Place.Name", "YEAR", "hours", "Treated")]
data_SDID <- data.frame(selected_variables)

setup = panel.matrices(data_SDID) # converts data set from panel to matrix format required by synthdid estimators

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)
