## Effects of Carnivore group Presence Absence

load("./output/statsDensAov")
load("./output/statsAov.Rda")

# Examplary for autotroph BMD:

auto.BMD <- subset(statsAov, FGroup=="autotroph" &
                         ExpNo!=8 &
                         TimeStep>=1080)
#

## Non-normal, unequal variances.
# for(i in 1:7){
#       print(paste("Expno", i))
#       print(shapiro.test(auto.BMD.0$Median[auto.BMD.0$ExpNo==i]))
# }
#
# for(i in 1:7){
#       print(paste("Expno", i))
#       print(shapiro.test(auto.BMD.1$Median[auto.BMD.0$ExpNo==i]))
# }
#
# x11()
# par(mfrow=c(1,3))
#
# pres <- c("0", "1")
# for(p in pres){
#       hist()
#       print(paste("PRES", p))
#       print(shapiro.test(auto.BMD.1$Median[auto.BMD.0$pECTia==p]))
# }
#



auto.BMD$FD <- droplevels(auto.BMD$FD)
auto.BMD$ExpNo <- as.factor(auto.BMD$ExpNo)
auto.BMD$ExpNo <- droplevels(auto.BMD$ExpNo)

auto.BMD.0 <- subset(auto.BMD, CellCode=="Cell0")
auto.BMD.1 <- subset(auto.BMD, CellCode=="Cell1")


## Model with FD and presence Absence of ECTi
BMD.FD <- lm(log10(Median) ~ FD, data=auto.BMD.0)
BMD0 <- lm(log10(Median) ~ FD * pECTia, data=auto.BMD.0)
BMD0.1 <- lm(log10(Median) ~ ExpNo, data=auto.BMD.0)
BMD0.1 <- aov(log10(Median) ~ ExpNo, data=auto.BMD.0)
BMD0.2 <- aov(log10(Median) ~ FD, data=auto.BMD.0, weights=weights)

BMD0.3 <- aov(log10(Median) ~ pECTia, data=auto.BMD.0)

BMDeff <- lm(log10(Median) ~ pECTia * pECTsa * pENDa - pECTia:pECTsa:pENDa, data=auto.BMD.0)
BMDeff2 <- update(BMDeff, .~. -pECTia:pENDa)

update


eff <- allEffects(BMDeff)
plot(t, "pECTia:pECTsa")
plot(allEffects(BMD0))
