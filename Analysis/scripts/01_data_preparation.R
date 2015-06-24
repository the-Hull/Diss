## Script applying functions from tidy_data.R


# Apply Functions to get & clean data ---------------------------------------

simlist <- list.simulations()
exp.initial <- get.data(simlist, expno=1)
exp.rm1_end <- get.data(simlist,expno=2)



# Save individual experiments

name <- gsub("2015.*", "", gsub(".*W/", "", unique(exp.initial$Folder)))
save(file=paste0("./output/", name, ".R"), exp.initial)


load("./output//simulation_list.R")
