loc <- read.csv("./RAW/01_BEF_INITIAL2015-6-5_12.32.53/SpecificLocations.csv",
                header = T,
                stringsAsFactors = F)

loc$Location <- c("South Ugunda", "Central England")
loc$`Cell Code` <- c("Cell 0", "Cell 1")
loc$`Character` <- c("aseasonal", "seasonal")

loc <- loc[ ,c(3,1,2,4,5)]

xtab <- xtable(x = loc,
               label = "tab:mat:exp:loc", caption = c("Specific locations and characteristics of the modelled ecosystems.",
                                                      "Specific locations and characteristics of the modelled ecosystems")
)


print(xtab,
      file = "../WriteUp/Dissertation/mat/tables/loc.tex",
      caption.placement = "top",
      table.placement = "htb!",
      tabular.environment = "tabular*",
      include.rownames = F,
      sanitize.colnames.function = function(x) paste0("\\textbf{", x, "}"),
      booktabs = T)
