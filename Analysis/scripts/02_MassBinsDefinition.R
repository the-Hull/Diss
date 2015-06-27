## This script creates a vector containing mass bins for the trait space cutting
## Experiment. It uses the MassBinsDefinitions file found in
## ./madingley/input/Model Setup/ecological definition files

massbins <- read.csv("~/GitHub/madingley-ecosystem-model-biodiversity-function/madingley-ecosystem-model-biodiversity-function/Madingley/input/Model setup/00_INI/Ecological Definition Files/MassBinDefinitions.csv", header=T)


# Creating sequence for chosen Massbins

above1000 <- rev((2^(0:10)))*1000
above100 <- rev(1:10)*100
above10 <- rev(1:10)*10
above001 <- rev((2^(0:9)))/100
below001 <- 0.01/(10^(1:4))

# create vector, add 0 as last lower bound, multiply with 1000 for g

newbins <- c(above1000, above100, above10, above001, below001, 0)

# Diagnostics
# plot(log10(newbins))
# plot(newbins)


# Replace massbins and write results to table

massbins$Mass.bin.lower.bound <- NULL
massbins <- massbins[-c(47:78),]
massbins$Mass.bin.lower.bound <- newbins
names(massbins) <- gsub("\\.", " ", names(massbins))


# write.csv(massbins,
#           file="./output/MassBinDefinitions.csv",
#           row.names=F)
#
