## Table for autotroph ExpNo

load("./output/KruskalW/BMD_env.Rda")
auto.BMD <- do.call(cbind,
                  mget(ls(pattern=paste0("autotroph_ExpNo"),
                          envir = BMD),
                       envir = BMD))

names(auto.BMD) <- sub("[[:alpha:]]*_[[:alpha:]]*_", "", names(auto.BMD))
names(auto.BMD) <- gsub("_", ".", names(auto.BMD))

xauto <- xtable(auto.BMD,
                label = "tab:chap:res:dyn:autoBMD",
                caption = c("Results of \\textit{post-hoc} Kruskal-Wallis multiple comparison
                tests between experiments for autotroph biomass density $[kgkm^{-2}]$ in the seasonal and aseasonal system.
                  Data for each group consists of the median values for the last 10 simulated years ($n_{i} = 121;\\quad i = 1,\\ldots8$).
                  Significant differences between groups, i.e. where observed
                values are larger than the critical threshold ($\\alpha = 0.5$) are given in bold.",
                            "Kruskal-Wallis multiple comparison of autotroph biomass density."),
                align="rXXXX")

### Row specs
rows <- numeric(6)
rows[1] <- 7
for(i in 2:6){
      rows[i] <- rows[i-1]+(rows[1]-i+1)
}


# print(xauto,
#       file="../WriteUp/Dissertation/res/tables/autotroph_BMD_KW_Cell1Cell2.tex",
#       booktabs = T,
#       sanitize.text.function = function(x){x},
#       tabular.environment="tabularx",
#       width="\\textwidth",
#       add.to.row = list(list(rows), "[1ex]")
#       )
