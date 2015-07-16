## Script applying functions from tidy_data.R


# Apply Functions to get & clean data ---------------------------------------

# simlist <- list.simulations()
# save(simlist, file="./output//simulation_list.R")


# expdataFull <- get.data(simlist)
# save(expdataFull, file = "./output/expdataFull.Rda")


# Load Experiment Data ----------------------------------------------------

load("./output//simulation_list.R")
load("./output/expdataFull.Rda")

# statsFull <- summary_stats(expdataFull)
# save("statsFull", file="./output/statsFull_medianCI.Rda")

load("./output/statsFull_medianCI.Rda")

# Join Simulation Labels to Data ------------------------------------------

simOverview <- read.csv("./report_data/SimOverview.csv", header=T)
# simOverview$FD <- factor(9 - simOverview$RM_No)
simOverview$FD <- 9 - simOverview$RM_No


simConcise <- simOverview[ ,c(1,8,9,10,11,12:16)]

# simConcise$FD <- relevel(simConcise$FD, ref="9")
simConcise$weights <- c(3, rep(1,6),3)


simConcise$pEND <- relevel(simConcise$pEND, ref="C")
simConcise$pECTi <- relevel(simConcise$pECTi, ref="C")
simConcise$pECTs <- relevel(simConcise$pECTs, ref="C")

simConcise$pENDa <- as.factor(simConcise$pENDa)
simConcise$pECTia <- as.factor(simConcise$pECTia)
simConcise$pECTsa <- as.factor(simConcise$pECTsa)

simConcise$pENDa <- relevel(simConcise$pENDa, ref="1")
simConcise$pECTia <- relevel(simConcise$pECTia, ref="1")
simConcise$pECTsa <- relevel(simConcise$pECTsa, ref="1")

simConcise$Label_Abb <- relevel(simConcise$Label_Abb, ref="C(end) + C(ect)s + C(ect)i")


statsAov <- inner_join(statsFull, simConcise)

statsAov$TL <- statsAov$FGroup

statsAov$TL[statsAov$TL=="autotroph"] <- 1
statsAov$TL[statsAov$TL=="herbivore"] <- 2
statsAov$TL[statsAov$TL=="omnivore"] <- 3
statsAov$TL[statsAov$TL=="carnivore"] <- 4


#
# # Create DF for H:C ratios ------------------------------------------------
#
 load("./output/HCRatios.Rda")

#
# ## Subset H and C
# herbivores <- subset(expdataFull, FGroup=="herbivore" & ExpNo!=8)
# carnivores <- subset(expdataFull, FGroup=="carnivore" & ExpNo!=8)
#
# names(herbivores)[6] <- "H.MassDens"
# names(carnivores)[6] <- "C.MassDens"
#
#
# ## Join subsets together
# HC.df <- full_join(herbivores,
#                    carnivores,
#                    by=c("ExpNo", "SimNo", "TimeStep", "CellCode", "Folder")
#                    )
#
# # Tidy up
# HC.df <- HC.df[ ,-c(2,5,8)]
# HC.df$HC.Ratio <- HC.df$H.MassDens / HC.df$C.MassDens
# HC.df <- HC.df[ ,-c(4,6)]
#
# # Grouping:
# HC.tmp <- group_by(HC.df,
#                 ExpNo,
#                 CellCode,
#                 TimeStep)
#
#
# # Summary stats + bootstrapping for median CI
# nsample=10^4
# HC.summary <- droplevels(summarize(HC.tmp,
#                             Count=n(),
#                             Mean=mean(HC.Ratio),
#                             SD=sd(HC.Ratio),
#                             Min=min(HC.Ratio),
#                             Max=max(HC.Ratio),
#                             Median=median(HC.Ratio),
#                             medianLCI=quantile(apply(matrix(sample(HC.Ratio,
#                                                                    rep=TRUE,
#                                                                    nsample*length(HC.Ratio)),
#                                                             nrow=nsample),
#                                                      1,
#                                                      median),
#                                                0.025),
#                             medianUCI=quantile(apply(matrix(sample(HC.Ratio,
#                                                                    rep=TRUE,
#                                                                    nsample*length(HC.Ratio)),
#                                                             nrow=nsample),
#                                                      1,
#                                                      median),
#                                                0.975)
#
# ))
#
#
#

# Create DF for H:A Ratios ------------------------------------------------

load("./output/HARatios.Rda")
HA.summary <- inner_join(HA.summary, simConcise)


#
#
# ## Subset Herbivore and Autotroph
# herbivores <- subset(expdataFull, FGroup=="herbivore" & ExpNo!=8)
# autotrophs <- subset(expdataFull, FGroup=="autotroph" & ExpNo!=8)
#
# names(herbivores)[6] <- "H.MassDens"
# names(autotrophs)[6] <- "A.MassDens"
#
#
# ## Join subsets together
# HA.df <- full_join(herbivores,
#                    autotrophs,
#                    by=c("ExpNo", "SimNo", "TimeStep", "CellCode", "Folder")
# )
#
# # Tidy up
# HA.df <- HA.df[ ,-c(2,5,8)]
# HA.df$HA.Ratio <- HA.df$H.MassDens / HA.df$A.MassDens
# HA.df <- HA.df[ ,-c(4,6)]
#
# # Grouping:
# HA.tmp <- group_by(HA.df,
#                    ExpNo,
#                    CellCode,
#                    TimeStep)
#
#
# # Summary stats + bootstrapping for median CI
# nsample=10^4
# HA.summary <- droplevels(summarize(HA.tmp,
#                                    Count=n(),
#                                    Mean=mean(HA.Ratio),
#                                    SD=sd(HA.Ratio),
#                                    Min=min(HA.Ratio),
#                                    Max=max(HA.Ratio),
#                                    Median=median(HA.Ratio)
#                                    ,medianLCI=quantile(apply(matrix(sample(HA.Ratio,
#                                                                           rep=TRUE,
#                                                                           nsample*length(HA.Ratio)),
#                                                                    nrow=nsample),
#                                                             1,
#                                                             median),
#                                                       0.025),
#                                    medianUCI=quantile(apply(matrix(sample(HA.Ratio,
#                                                                           rep=TRUE,
#                                                                           nsample*length(HA.Ratio)),
#                                                                    nrow=nsample),
#                                                             1,
#                                                             median),
#                                                       0.975)

# ))





# Aut:Het Ratio -----------------------------------------------------------
#
# ## Subset Heterotroph and Autotroph
# dat <- expdataFull[,-2]
#
# herbivores <- subset(dat, FGroup=="herbivore" & ExpNo!=8)
# autotrophs <- subset(dat, FGroup=="autotroph" & ExpNo!=8)
# carnivores <- subset(dat, FGroup=="carnivore" & ExpNo!=8)
# omnivores <-  subset(dat, FGroup=="omnivore" & ExpNo!=8)
#
# names(herbivores)[5] <- "H.MassDens"
# names(autotrophs)[5] <- "A.MassDens"
# names(carnivores)[5] <- "C.MassDens"
# names(omnivores)[5] <- "O.MassDens"
#
# ## Join subsets together
# HA.df <- full_join(herbivores,
#                    autotrophs,
#                    by=c("ExpNo", "SimNo", "TimeStep", "CellCode")
# )
#
# # Tidy up
# HA.df <- HA.df[ ,-c(4,7)]
#
#
# # Join Carnivores
# HAC.df <- full_join(HA.df,
#                     carnivores,
#                     by=c("ExpNo", "SimNo", "TimeStep", "CellCode")
# )
# HAC.df <- HAC.df[,-7]
# # Join Omnivores
#
# HACO.df <- full_join(HAC.df,
#                      omnivores,
#                      by=c("ExpNo", "SimNo", "TimeStep", "CellCode")
# )
# HACO.df <- HACO.df[,-8]
#
# AutHet.df <- HACO.df[,c(1,2,3,5,6)]
# AutHet.df$Het.MassDens <- rowSums(HACO.df[,c(4,7,8)])
# AutHet.df$MassDens.Ratio <- AutHet.df$Het.MassDens / AutHet.df$A.MassDens
#
# AutHet.Ratio <- AutHet.df[ ,c(1,2,3,4,7)]
#
# rm(list=c("omnivores", "carnivores", "autotrophs", "herbivores",
#           "HA.df", "HAC.df", "HACO.df"))
#
#
#
# # Grouping:
# AutHet.temp <- group_by(AutHet.df,
#                         ExpNo,
#                         CellCode,
#                         TimeStep)
#
#
# # Summary stats + bootstrapping for median CI
# nsample=10^4
# AutHet.summary <- droplevels(summarize(AutHet.temp,
#                                        Count=n(),
#                                        Mean=mean(MassDens.Ratio),
#                                        SD=sd(MassDens.Ratio),
#                                        Min=min(MassDens.Ratio),
#                                        Max=max(MassDens.Ratio),
#                                        Median=median(MassDens.Ratio),
#                                        medianLCI=quantile(apply(matrix(sample(MassDens.Ratio,
#                                                                               rep=TRUE,
#                                                                               nsample*length(MassDens.Ratio)),
#                                                                        nrow=nsample),
#                                                                 1,
#                                                                 median),
#                                                           0.025),
#                                        medianUCI=quantile(apply(matrix(sample(MassDens.Ratio,
#                                                                               rep=TRUE,
#                                                                               nsample*length(MassDens.Ratio)),
#                                                                        nrow=nsample),
#                                                                 1,
#                                                                 median),
#                                                           0.975)
#
# ))
#
#
#
#
