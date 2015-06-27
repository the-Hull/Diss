## Script applying functions from tidy_data.R


# Apply Functions to get & clean data ---------------------------------------

# simlist <- list.simulations()
# save(simlist, file="./output//simulation_list.R")


# expdataFull <- get.data(simlist)
# save(expdataFull, file = "./output/expdataFull.Rda")


# Load Experiment Data ----------------------------------------------------

load("./output//simulation_list.R")
load("./output/expdataFull.Rda")

statsFull <- summary_stats(expdataFull)


# Join Simulation Labels to Data ------------------------------------------

simOverview <- read.csv("./report_data/SimOverview.csv", header=T)
simOverview$FD <- as.ordered(9 - simOverview$RM_No)
simConcise <- simOverview[ ,c(1,8,9)]
statsAov <- inner_join(statsFull, simConcise)

Cell0 <- subset(statsAov, CellCode=="Cell0" & ExpNo!=8)
Cell1 <- subset(statsAov, CellCode=="Cell1" & ExpNo!=8)

# aov1 <- aov(Median ~ FD * FGroup * Label_Abb , Cell0)


