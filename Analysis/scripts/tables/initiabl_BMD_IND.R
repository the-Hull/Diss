## Initial Biomass in system
require(xtable)
load("./output/statsDensAov.Rda")
load("./output/statsAov.Rda")

seed.dens <- statsDensAov[statsDensAov$TimeStep==1, ]
seed.bmd <- statsAov[statsAov$TimeStep==1, ]

bmd.mat <- with(seed.bmd, tapply(Median, list(FGroup, ExpNo, CellCode), sum))
dens.mat <- with(seed.dens, tapply(Median, list(FGroup, ExpNo, CellCode), sum))

xbmd <- xtable(cbind(Experiment = c(1:8, 1:8),
                     t(cbind(bmd.mat[c(1,3,4,2),,1],
                           bmd.mat[c(1,3,4,2),,2] ))),
               caption = "Initial ($t = 1$) biomass density [$kg\\cdot km^{-2}$] for all experiments.",
               label = "tab:app:initialBMD")

print(xbmd,file = "../WriteUp/Dissertation/app/initial_BMD.tex",
             tabular.environment = "tabular*",
             caption.placement = "top",
             booktabs = T,
             add.to.row = list(list(8), "[1ex]"),
             include.rownames = F)
