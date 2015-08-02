fgdef <- read.csv("./report_data/CohortFunctionalGroupDefinitions.csv")
fg <- fgdef[fgdef$DEFINITION_Realm=="Terrestrial",]
fg <- fg[ ,-c(1,3,4,5,13,14)]
fg[ ,c(7,8)] <- round(log10(fg[ ,c(7,8)]/1000), 2)
fg[ ,2] <- paste0(substr(fg[ ,2], 1,5), ".")
names(fg) <- sub(".*_", "", names(fg))
names(fg) <- gsub("[[:punct:]]", " ", names(fg))
names(fg)[1] <- "Trophic Type"
names(fg)[2] <- "Reproduction"
names(fg)[3] <- "Metabolism"
names(fg)[4] <- "Herbivore"
names(fg)[5] <- "Carnivore"
names(fg)[6] <- "Activity"
names(fg)[7] <- "Min."
names(fg)[8] <- "Max."
fg <- fg[order(fg$`Trophic Type`),]

xtab <- xtable(x = fg,
               label = "tab:mat:madingley:func",
               caption = c("Functional group definitions based on categorical traits (trophic type, reproductive and metabolic strategy),
               and additional parameters that determine the ecology of a group (proportional assimilation efficiens and the proportion of each time step
               at which an organism is active). Maximum and minimum body mass determine the size range for the cohorts seeded into the model at the initial time step ($n = 112$ per functional group).",
                           "Functional group definitions"))

print(xtab,
      file = "../WriteUp/Dissertation/mat/tables/fgdef.tex",
      caption.placement = "top",
      floating.environment = "sidewaystable",
      table.placement = "htb!",
      tabular.environment = "tabulary",
      include.rownames = F,
      sanitize.colnames.function = function(x) paste0("\\textbf{", x, "}"),
      add.to.row = list(list(3,6), rep("[1ex]",2)),
      booktabs = T)

