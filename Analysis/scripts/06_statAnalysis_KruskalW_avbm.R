## Kruskal Wallis tests for differences between groups
## helper function
require(xtable)
require(dplyr)
require(pgirmess)
rdec <- function(x, k){format(round(x), nsmall=k)}


#
# load("./output/statsAvbm_medianCI.Rda")
# load("./output/simConcise.Rda")
# simConcise$ExpNo <- as.factor(simConcise$ExpNo)
#
# statsAvbm <- inner_join(statsAvbm, simConcise)

load("./output/statsAvbm.Rda")
statsAvbm$ExpNo <- as.factor(statsAvbm$ExpNo)


## Set up output paths:
outPath <- "./output/"
# bigtabsPath <- "output/KruskalW/bigtabs/"


## Build subsets, run kruskalmc and record differences for both cells and all FG
## individually

## ------ AVBM ------

AVBM <- new.env() #Storage for output

AVBM$rankmean <- new.env()

AVBM$kwtest <- new.env()

cells <- unique(statsAvbm$CellCode)

fgroups <- unique(statsAvbm$FGroup)

expno <- levels(statsAvbm$ExpNo)
exptest <- expno
numexp <- length(exptest)

categ <- names(statsAvbm)[c(1,12)]

for(ce in cells){
      curPath <- paste0(outPath,"KruskalW/AVBM/", ce)
      xPath <- paste0(curPath, "/xtable")

      if(!is.factor(statsAvbm$ExpNo) | !is.factor(statsAvbm$FD)){
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

            assign(paste0(fg, "_dat"), subset(statsAvbm, FGroup==fg &
                                                    TimeStep>=1080 &
                                                    CellCode==ce),
                   envir = AVBM)




            for(cat in categ){


                  dat <- get(paste0(fg, "_dat"),
                             envir = AVBM)

                  dat.tmp <- kruskalmc(as.formula(paste("Median~",cat)),
                                       data= dat)$dif.com

                  tr <- which(dat.tmp$difference==T)
                  ntr <- is.na(dat.tmp$difference)
                  dat.tmp[tr,1] <- apply(as.matrix(dat.tmp[tr,1]),
                                         1,
                                         FUN=function(x) paste0("\\(\\mathbf{", rdec(x,2), "}\\)"))
                  dat.tmp[tr,2] <- apply(as.matrix(dat.tmp[tr,2]),
                                         1,
                                         FUN=function(x) paste0("\\(\\mathbf{", rdec(x,2), "}\\)"))

                  dat.tmp <- dat.tmp[,1:2]

                  dat.tmp[ntr,] <- NA
                  colnames(dat.tmp) <- c("Obs.", "Crit.")



                  dat.tmp[,1] <- apply(as.matrix(dat.tmp[,1]),
                                       1,
                                       FUN = function(x){
                                             ifelse(substring(x,first = 1, last = 1)!="\\",
                                                    rdec(as.numeric(x),2),
                                                    x)
                                       })

                  dat.tmp[,2] <- apply(as.matrix(dat.tmp[,2]),
                                       1,
                                       FUN = function(x){
                                             ifelse(substring(x,first = 1, last = 1)!="\\",
                                                    rdec(as.numeric(x),2),
                                                    x)
                                       })
                  dat <- as.data.frame(dat)


                  assign(paste0(fg, "_", cat,  "_", ce, "_res"),

                         dat.tmp
                         ,
                         envir = AVBM
                  )


                  assign(paste0(fg, "_", cat,  "_", ce, "_ranks"),

                         tapply(rank(dat$Median),list(dat[,cat]), mean)
                         ,
                         envir = AVBM$rankmean
                  )



                  assign(paste0(fg, "_", cat,  "_", ce, "_kwtest"),

                         kruskal.test(dat$Median~dat[,cat])
                         ,
                         envir = AVBM$kwtest
                  )

                  write.csv(dat.tmp, file=paste0(curPath,"/",fg, "_", cat, "_",ce, "_res.csv"))




                  curr.test <- get(x = paste0(fg, "_", cat,  "_", ce, "_kwtest"),
                                   envir = AVBM$kwtest)

                  curr.test[5] <- paste(cat,"for",fg, "in", ce, "average body mass [$kg\\cdot n$]")


                  xtab <- xtable(dat.tmp,
                                 caption = paste0("$\\chi^{2}_{",curr.test[2], "} = ", round(as.numeric(curr.test[1]),2),"$ ",
                                                  "$p = ", round(as.numeric(curr.test[3]), 4), "$ ", curr.test[5]),
                                 label = "tab:"

                  )

                  print(xtab, file=paste0(xPath,"/",fg, "_", cat,  "_", ce, "_res.tex"),
                        sanitize.text.function = function(x) x,
                        tabular.environment = "tabular*",
                        caption.placement = "top",
                        booktabs = T,
                        sanitize.colnames.function = function(x){
                              paste0("\\textbf{", x, "}")
                        })



            }


      }

}
AVBM.kw <- as.list(mget(ls(pattern = "res", envir = AVBM), envir = AVBM))

save(AVBM, file="output/KruskalW/AVBM_env.Rda")
save(AVBM.kw, file="output/KruskalW/AvBM_KruskalW.Rda")
