## Table for MannW stats between Cells, Auto - Herb - Omn - Car
load("./output/MannW/betweenCells.Rda")
require(xtable)

MannW_betweenCells$P <- signif(MannW_betweenCells$P, 3)
MannW_betweenCells$Sig <- "***"

MannW_betweenCells <- MannW_betweenCells[,c(1,2,6,7,3,4,5)]
MannW_betweenCells$P <- as.character(MannW_betweenCells$P)

MannW_betweenCells$P <- paste0("$", sub("e-", "\\cdot 10^{-", MannW_betweenCells$P, fixed=T),"}$")
MannW_betweenCells$P[14] <- 0.00158

names(MannW_betweenCells) <- c("Functional Group",
                               "Experiment",
                               "Cell 0",
                               "Cell 1",
                               "$U$",
                               "$P$",
                               "Sig.")

xtab <- xtable(MannW_betweenCells,
               caption = c("Results of Mann-Whitney-U Tests comparing biomass density [$kg\\cdot km^{-2}$] across experiments
               between cells ($n_i = 122, \\quad with i = 1,..8$). Highly
               significant results ($p \\ll 0.05$) are marked with asterisks.",
                           "Comparison of biomass density between cells"),
               label = "tab:chap:res:comp")


print(xtab,
      file = "../WriteUp/Dissertation/app/tab/MannW_betweenCells.tex",
      caption.placement = "top",
      table.placement = "ht!",
      tabular.environment = "tabular*",
      booktabs = T,
      sanitize.text.function = function(x) x,
      sanitize.colnames.function = function(x) paste0("\\textbf{", x, "}"),
      add.to.row = list(list(8,15,23),
                        rep("[1ex]",3)),
      include.rownames = F
      )