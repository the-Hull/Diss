## Stat Modelling


# HA.summary <- inner_join(HA.summary, statsFull[statsFull$FGroup=="carnivore" & statsFull$ExpNo!=8 ,c("ExpNo","CellCode", "TimeStep", "Mean", "Median")], by=c("ExpNo", "TimeStep", "CellCode"))
# names(HA.summary)[5] <- "Mean"
# names(HA.summary)[9] <- "Median"
# names(HA.summary)[22] <- "carMean"
# names(HA.summary)[23] <- "carMedian"
#
#
# # By cell. Justification: different dynamics, model not well behaved in seas.
#
# # Subset for aseasonal (Cell1)


load("./output/HARatios_medianCI.Rda")
load("./output/simConcise.Rda")
HA.summary <- inner_join(HA.summary, simConcise)


HA.0 <- subset(HA.summary, CellCode=="Cell0" & TimeStep>=1080 & ExpNo!=8)
HA.0 <- droplevels(HA.0)
HA.0$FD <- as.factor(HA.0$FD)
HA.0$FD <- relevel(HA.0$FD, ref="9")
HA.0$FD <- factor(HA.0$FD, levels(HA.0$FD)[c(1,3,2)])
HA.0$ExpNo <- as.factor(HA.0$ExpNo)



# Test for differences between FD Levels ----------------------------------

FD.ref <- aov(log10(Median) ~ FD, data=HA.0, weights=weights)
#non-normal residuals.



## Using autotrophs BMD:

autotrophs <- subset(statsAov, FGroup=="autotroph" &
                           CellCode=="Cell0" &
                           TimeStep>=90*12 &
                           ExpNo!=8)
autotrophs$FD <- droplevels(autotrophs$FD)

kruskalmc(Median~ExpNo, data=autotrophs)


FD.auto.t <- aov(log10(Median) ~ FD, weights=weights, data=autotrophs)
TukeyHSD(FD.auto.t, "FD", ordered = F)
FD.auto <- aov(Median ~ FD, weights=weights, data=autotrophs)
# HA.1$FD <- relevel(HA.1$FD, ref="9")
# HA.1$pEND <- relevel(HA.1$pEND, ref="C")
# HA.1$pECTi <- relevel(HA.1$pECTi, ref="C")
# HA.1$pECTs <- relevel(HA.1$pECTs, ref="C")


# ANOVA:

# Set up Null Model
HA.mod0 <- aov(Median ~ 1, data = HA.0)


HA.mod1 <- aov(Median ~ FD, data=HA.0)
HA.mod2 <- aov(log(Median) ~ ExpNo, data=HA.0)
HA.mod3 <- aov(log(Median) ~ FD, data=HA.0)
HA.mod4 <- aov(log(Median) ~ FD + (ExpNo %in% FD), data=HA.0)
HA.mod5 <- lm(log(Median) ~ FD + ((pEND+pECTi+pECTs) %in% FD), data=HA.0)
HA.mod6 <- lm(log10(Median) ~ FD + FD*pECTia*pENDa, data=HA.0)
HA.mod7 <- lm(log10(Median) ~ FD + FD:(pECTia * pENDa) - FD:pECTia:pENDa, data = HA.0, weights = weights)
###
HA.mod8 <- lm(log10(Median) ~ FD * pECTia * pENDa - FD:pECTia:pENDa, data = HA.0, weights = weights)
interaction.plot(HA.0$FD, HA.0$pECTia, log10(HA.0$Median))
interaction.plot(HA.0$FD, HA.0$pENDa, log10(HA.0$Median))
interaction.plot(HA.0$FD, HA.0$pECTia, log10(HA.0$Median))



HA.mod81 <- lm(log10(Median) ~ FD * pECTia * pENDa - FD:pECTia:pENDa - FD:pENDa, data = HA.0, weights = weights)
###
HA.mod9 <- lm(log10(Median) ~ FD * pECTia, data = HA.0, weights=weights)

# Model with all factors of interest

HA.modF <- aov(Median ~ FD+pEND+pECTs+pECTi, data=HA.0)
summary.lm(HA.modF)

HA.lm <- lm(Median ~ FD + pEND + pECTs + pECTi, data=HA.0)
HA.lm2 <- lm(Median ~ FD + pEND, data = HA.0)
HA.lm3 <- lm(log(Median) ~ FD, data = HA.0)

summary(HA.lm)

