load("./output/bootMedian_Avbm.Rda")
load("./output/simConcise.Rda")
require(dplyr)
require(xtable)

herbAV <- subset(bootMedian_AVBM$data, FGroup=="herbivore")
herbAV <- left_join(herbAV, simConcise)

FD_median_herbAV <- list(
      round(tapply(herbAV$Med, list(herbAV$FD, herbAV$CellCode), median), 3),
      round(tapply(herbAV$Med, list(herbAV$FD, herbAV$CellCode), median) -
            tapply(herbAV$medianLCI, list(herbAV$FD, herbAV$CellCode), median),3),
      round(tapply(herbAV$medianUCI, list(herbAV$FD, herbAV$CellCode), median) -
            tapply(herbAV$Med, list(herbAV$FD, herbAV$CellCode), median),3)
)


table <- data.frame(FGR = numeric(4),
                    Median = numeric(4),
                    LCI = numeric(4),
                    UCI = numeric(4),
                    Median1 = numeric(4),
                    LCI1 = numeric(4),
                    UCI1 = numeric(4))
table[,1] <- 9:6


FD <- lapply(FD_median_herbAV, function(x) x[c(1,4,3,2), ])
table[,2] <- FD[[1]][,1]
table[,3] <- FD[[2]][,1]
table[,4] <- FD[[3]][,1]

table[,5] <- FD[[1]][,2]
table[,6] <- FD[[2]][,2]
table[,7] <- FD[[3]][,2]



xtab <- xtable(x = table,
               caption = c("Median of herbivore average body mass [$kg\\cdot n^{-1}$] for both
                           systems at different levels of carnivore functional group richness
                           (FGR). LT and UT are the lower and upper tail of the 95~\\%
                           confidence interval from bootstrapping.",
                           "Median of average body mass at different levels of
                           function group richness"),
               label = "tab:chap:res:avbm"
               )

print(xtab,
      file = "../WriteUp/Dissertation/res/tables/medianAVBM_herb.tex",
      table.placement = "ht!",
      caption.placement = "top",
      tabular.environment = "tabular*",
      include.rownames = F,
      sanitize.colnames.function = function(x) paste0("\\textbf{", x, "}"),
      booktabs = T)

