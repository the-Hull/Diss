## Script applying functions from tidy_data.R


# Apply Functions to get & clean data ---------------------------------------

# simlist <- list.simulations()

# expdata <- get.data(simlist)




# Save individual experiments
#
# name <- gsub("2015.*", "", gsub(".*W/", "", unique(exp.initial$Folder)))
# save(file=paste0("./output/", name, ".R"), exp.initial)
#

load("./output//simulation_list.R")


# Load Experiment Data ----------------------------------------------------

expdata.list <- list.files("./output/", pattern = ".Rda", full.names = T)
sapply(expdata.list, load)
