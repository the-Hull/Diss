# test <- wilcox.test(statsAov$Median~statsAov$PRESa, exact=T, estimate=T)


## Kruskal Wallis tests for differences between groups
## helper function
require(xtable)
require(dplyr)
require(pgirmess)
rdec <- function(x, k){format(round(x), nsmall=k)}


#
# load("./output/statsAov_medianCI.Rda")
# load("./output/simConcise.Rda")
# simConcise$ExpNo <- as.factor(simConcise$ExpNo)
#
# statsAov <- inner_join(statsAov, simConcise)

load("./output/statsAov.Rda")
statsAov$ExpNo <- as.factor(statsAov$ExpNo)


## Set up output paths:
outPath <- "./output/"
# bigtabsPath <- "output/MWU/bigtabs/"


## Build subsets, run kruskalmc and record differences for both cells and all FG
## individually

## ------ PRES ------

PRES <- new.env() #Storage for output

PRES$median <- new.env()

PRES$mwutest <- new.env()

cells <- unique(statsAov$CellCode)

fgroups <- unique(statsAov$FGroup)

expno <- levels(statsAov$ExpNo)
exptest <- expno
numexp <- length(exptest)

categ <- names(statsAov)[c(19:21)]

for(ce in cells){
      curPath <- paste0(outPath,"MannW/BMD_pres/", ce)
      xPath <- paste0(curPath, "/xtable")

      if(!is.factor(statsAov$ExpNo) | !is.factor(statsAov$FD)){
            break("Coerce to factors!")
      } else {
            paste("Factor Check successful!")
      }


      if(!file.exists(curPath)){
            dir.create(curPath, recursive=T)
      } else {}

      if(!file.exists(xPath)){
            dir.create(xPath, recursive = T)
      } else {}

      for(fg in fgroups){

            assign(paste0(fg, "_dat"), subset(statsAov, FGroup==fg &
                                                    TimeStep>=1080 &
                                                    CellCode==ce),
                   envir = PRES)



            for(cat in categ){


                  dat <- get(paste0(fg, "_dat"),
                             envir = PRES)


                  dat <- as.data.frame(dat)




                  n1 <- table(dat[,cat])[[1]]
                  n2 <- table(dat[,cat])[[2]]

                  assign(paste0(fg, "_", cat,  "_", ce, "_median"),

                         tapply(dat$Median,list(dat[,cat]), median)
                         ,
                         envir = PRES$median
                  )



                  assign(paste0(fg, "_", cat,  "_", ce, "_mwutest"),

                         {
                               wilcox.test(dat$Median~dat[,cat])
                               }
                         ,
                         envir = PRES$mwutest
                  )



                  # write.csv(dat.tmp, file=paste0(curPath,"/",fg, "_", cat, "_",ce, "_res.csv"))




#                   curr.test <- get(x = paste0(fg, "_", cat,  "_", ce, "_mwutest"),
#                                    envir = PRES$mwutest)
#
#                   curr.test[6] <- paste(cat,"for",fg, "in", ce, " biomass density [$kg\\cdot km^{-2}$]")



#
#                   xtab <- xtable(dat.tmp,
#                                  caption = paste0("$\\U_{",
#                                                   n1,
#                                                   "," ,
#                                                   n2,
#                                                   "} = ",
#                                                   round(as.numeric(curr.test[1]),2),
#                                                   "$ ",
#                                                   "$p = ",
#                                                   round(as.numeric(curr.test[3]), 4),
#                                                   "$ ",
#                                                   curr.test[5]),
#                                  label = "tab:"
#
#                   )
#
#                   print(xtab, file=paste0(xPath,"/",fg, "_", cat,  "_", ce, "_res.tex"),
#                         sanitize.text.function = function(x) x,
#                         tabular.environment = "tabular*",
#                         caption.placement = "top",
#                         booktabs = T,
#                         sanitize.colnames.function = function(x){
#                               paste0("\\textbf{", x, "}")
#                         })



            }


      }

}
# PRES.kw <- as.list(mget(ls(pattern = "res", envir = PRES), envir = PRES))

save(PRES, file="output/MannW/BMD_pres/PRES_env.Rda")
# save(PRES.mwu, file="output/MWU/PRES_MWU.Rda")
