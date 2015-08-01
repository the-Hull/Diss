load("./output/bootMedian_IND.Rda")
load("./output/simConcise.Rda")
require(dplyr)
require(xtable)

herbDENS <- subset(bootmedian_IND$data, FGroup=="herbivore")
herbDENS <- left_join(herbDENS, simConcise)

FD_median_herbDENS <- list(
      round(tapply(herbDENS$Med, list(herbDENS$FD, herbDENS$CellCode), median), 0),
      round(tapply(herbDENS$Med, list(herbDENS$FD, herbDENS$CellCode), median) -
                  tapply(herbDENS$medianLCI, list(herbDENS$FD, herbDENS$CellCode), median),0),
      round(tapply(herbDENS$medianUCI, list(herbDENS$FD, herbDENS$CellCode), median) -
                  tapply(herbDENS$Med, list(herbDENS$FD, herbDENS$CellCode), median),0)
)


table <- data.frame(FGR = numeric(4),
                    Median = numeric(4),
                    LCI = numeric(4),
                    UCI = numeric(4),
                    Median1 = numeric(4),
                    LCI1 = numeric(4),
                    UCI1 = numeric(4))
table[,1] <- 9:6


FD <- lapply(FD_median_herbDENS, function(x) x[c(1,4,3,2), ])
table[,2] <- FD[[1]][,1]
table[,3] <- FD[[2]][,1]
table[,4] <- FD[[3]][,1]

table[,5] <- FD[[1]][,2]
table[,6] <- FD[[2]][,2]
table[,7] <- FD[[3]][,2]



xtab <- xtable(x = table,
               caption = c("Median of herbivore abundance density [$n\\cdot km^{-2}$] for both
                           systems at different levels of carnivore functional group richness
                           (FGR). LT and UT are the lower and upper tail of the 95~\\%
                           confidence interval from bootstrapping.",
                           "Median of abundance density at different levels of
                           function group richness"),
               label = "tab:chap:res:avbm"
               )

print(xtab,
      file = "../WriteUp/Dissertation/res/tables/medianDENS_herb.tex",
      table.placement = "ht!",
      caption.placement = "top",
      tabular.environment = "tabular*",
      include.rownames = F,
      sanitize.colnames.function = function(x) paste0("\\textbf{", x, "}"),
      booktabs = T)

