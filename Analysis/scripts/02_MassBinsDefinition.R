## This script creates a vector containing mass bins for the trait space cutting
## Experiment. It uses the MassBinsDefinitions file found in
## ./madingley/input/Model Setup/ecological definition files

massbins <- read.csv("~/GitHub/madingley-ecosystem-model-biodiversity-function/madingley-ecosystem-model-biodiversity-function/Madingley/input/Model setup/00_INI/Ecological Definition Files/MassBinDefinitions.csv", header=T)


# Creating sequence for chosen Massbins

above10 <- rev((2^(4:10)))
above1 <- 10:1
above001 <- sort((rep(1:9, 2) / c(rep(10,9), rep(100,9))), decreasing = T)
below001 <- rep(1, 4)/(10^(3:6))


# create vector, add 0 as last lower bound, multiply with 1000 for g

newbins <- c(above10, above1, above001, below001, 0)*1000

# Diagnostics
# plot(log10(newbins))
# plot(newbins)


# Replace massbins and write results to table

massbins$Mass.bin.lower.bound <- NULL
massbins <- massbins[-c(41:78),]
massbins$Mass.bin.lower.bound <- newbins
names(massbins) <- gsub("\\.", " ", names(massbins))


write.csv(massbins,
          file="./output/MassBinDefinitions.csv",
          row.names=F)

