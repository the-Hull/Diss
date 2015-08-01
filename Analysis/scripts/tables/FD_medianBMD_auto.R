## Create table for median FD + CI of autotroph body mass density
## FD_median_auto.rda is based on data from "./output/bootmedianCI_BMD.rda"

load("./output/FD_median_auto.rda")



require(dplyr)
require(xtable)




table <- data.frame(FGR = numeric(4),
           Median = numeric(4),
           LCI = numeric(4),
           UCI = numeric(4),
           Median1 = numeric(4),
           LCI1 = numeric(4),
           UCI1 = numeric(4))
table[,1] <- 9:6


FD <- lapply(FD_median_auto, function(x) x[c(1,4,3,2), ])
table[,2] <- round(FD[[1]][,1])
table[,3] <- round(FD[[2]][,1])
table[,4] <- round(FD[[3]][,1])

table[,5] <- round(FD[[1]][,2])
table[,6] <- round(FD[[2]][,2])
table[,7] <- round(FD[[3]][,2])


xtab <- xtable(x = table,
               caption = c("Median biomass density [$kg\\cdot km^{-2}$] for both
               systems at different levels of carnivore functional group richness
               (FGR). LT and UT are the lower and upper tail of the 95~\\%
               confidence interval from bootstrapping.",
                           "Median biomass density at different levels of
                           function group richness"),
               label = "tab:chap:res:median"
               )

print(xtab,
      file = "../WriteUp/Dissertation/res/tables/medianBMD.tex",
      table.placement = "ht!",
      caption.placement = "top",
      tabular.environment = "tabular*",
      include.rownames = F,
      sanitize.colnames.function = function(x) paste0("\\textbf{", x, "}"),
      booktabs = T)