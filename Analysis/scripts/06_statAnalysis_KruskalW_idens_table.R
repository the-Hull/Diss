## Table for autotroph ExpNo

load("./output/KruskalW/IDENS_env.Rda")
herb.IDENS <- do.call(cbind,
                    mget(ls(pattern=paste0("herbivore_ExpNo"),
                            envir = IDENS),
                         envir = IDENS))

names(herb.IDENS) <- sub("[[:alpha:]]*_[[:alpha:]]*_", "", names(herb.IDENS))
names(herb.IDENS) <- gsub("_", ".", names(herb.IDENS))

xauto <- xtable(herb.IDENS,
                label = "tab:chap:res:dyn:herbIND",
                caption = c("Results of \\textit{post-hoc} Kruskal-Wallis multiple comparison
                            tests between experiments for herbivore density $[nkm^{-2}]$ in the seasonal and aseasonal system.
                            Data for each group consists of the median values for the last 10 simulated years ($n_{i} = 121;\\quad i = 1,\\ldots8$).
                            Significant differences between groups, i.e. where observed
                            values are larger than the critical threshold ($\\alpha = 0.5$) are given in bold.",
                            "Kruskal-Wallis multiple comparison of herbivore density."),
                align="rXXXX")

### Row specs
rows <- numeric(6)
rows[1] <- 7
for(i in 2:6){
      rows[i] <- rows[i-1]+(rows[1]-i+1)
}


# print(xauto,
#       file="../WriteUp/Dissertation/res/tables/herbivore_IDENS_KW_Cell1Cell2.tex",
#       booktabs = T,
#       sanitize.text.function = function(x){x},
#       tabular.environment="tabularx",
#       width="\\textwidth",
#       add.to.row = list(list(rows), "[1ex]"),
#           caption.placement = "top"
# )
