load("./output/simConcise.Rda")
sim <- simConcise[ ,c(1,2,11)]
names(sim) <- c("Experiment", "Composition", "Initial Cohorts")
sim$`Richness` <- c(3, rep(2,3), rep(1,3), 0)
sim <- sim[, c(1,2,4,3)]
sim$`Initial Cohorts` <- sim$`Richness`*112


xtab <- xtable(x = sim,
               label = "tab:mat:exp", caption = c("Experiments, corresponding carnivore functional group composition and richness. Both systems were simulated in 100 replicates over 100 years for each experiment",
                           "Functional group definitions"))

print(xtab,
      file = "../WriteUp/Dissertation/mat/tables/exp.tex",
      caption.placement = "top",
      table.placement = "htb!",
      tabular.environment = "tabular*",
      include.rownames = F,
      sanitize.colnames.function = function(x) paste0("\\textbf{", x, "}"),
      add.to.row = list(list(1,4,7), rep("[1ex]",3)),
      booktabs = T)
