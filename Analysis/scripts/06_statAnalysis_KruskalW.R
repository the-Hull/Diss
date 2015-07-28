## helper function
rdec <- function(x, k){format(round(x), nsmall=k)}
require(pgirmess)
require(xtable)
##

## Kruskal Wallis tests for differences between groups

load("./output/statsAov.Rda")

statsAov$ExpNo <- as.factor(statsAov$ExpNo)


## Set up output paths:
outPath <- "./output/"
bigtabsPath <- "output/KruskalW/bigtabs/"


## Build subsets, run kruskalmc and record differences for both cells and all FG
## individually

## ------ BMD ------

BMD <- new.env() #Storage for output
BMD$rankmean <- new.env()

BMD$kwtest <- new.env()



cells <- unique(statsAov$CellCode)

fgroups <- unique(statsAov$FGroup)

expno <- levels(statsAov$ExpNo)
# exptest <- expno[-8]
exptest <- expno
numexp <- length(exptest)

categ <- names(statsAov)[c(1,23)]


for(ce in cells){
      curPath <- paste0(outPath,"KruskalW/BMD/", ce)
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
                   envir = BMD)

            for(cat in categ){


                  dat <- get(paste0(fg, "_dat"),
                             envir = BMD)

                  dat.tmp <- as.data.frame(kruskalmc(as.formula(paste("Median~",cat)),
                                                     data= dat)$dif.com, colclasses=c("numeric", "numeric", "logical"))

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
                         envir = BMD
                  )

                  assign(paste0(fg, "_", cat,  "_", ce, "_ranks"),

                         tapply(rank(dat$Median),list(dat[,cat]), mean)
                         ,
                         envir = BMD$rankmean
                  )

                  assign(paste0(fg, "_", cat,  "_", ce, "_kwtest"),

                         kruskal.test(dat$Median~dat[,cat])
                         ,
                         envir = BMD$kwtest
                  )




                  write.csv(dat.tmp, file=paste0(curPath,"/",fg, "_", cat, "_",ce, "_res.csv"))


                  curr.test <- get(x = paste0(fg, "_", cat,  "_", ce, "_kwtest"),
                                   envir = BMD$kwtest)
                  curr.test[5] <- paste(cat,"for",fg, "in", ce, "biomass density [$kg\\cdot km^{-2}$]")
#
                  xtab <- xtable(dat.tmp,
                                 caption = paste0("$\\chi_{",curr.test[2], "} = ", round(as.numeric(curr.test[1]),2),"$ ",
                                                  "$p = ", round(as.numeric(curr.test[3]), 4), "$ ", curr.test[5]),
                                 label = "tab:"

                  )


                  print(xtab, file=paste0(xPath,"/",fg, "_", cat,  "_", ce, "_res.tex"),
                        sanitize.text.function = function(x){x},
                        tabular.environment = "tabular*",
                        caption.placement = "top",
                        booktabs = T,
                        sanitize.colnames.function = function(x){
                              paste0("\\textbf{", x, "}")
                        }
                        )

            }


      }

}
BMD.kw <- as.list(mget(ls(pattern = "res", envir = BMD), envir = BMD))
save(BMD, file="./output/KruskalW/BMD_env.Rda")
save(BMD.kw, file="output/KruskalW/BioMassDens_KruskalW.Rda")

if(!file.exists(bigtabsPath)){
      dir.create(bigtabsPath, recursive = T)
}

res <- new.env()
for(ca in categ){
      for(c in cells){

            bigtab <- do.call(cbind,
                              mget(ls(pattern=paste0(ca, "_", c),
                                      envir = BMD),
                                   envir = BMD))

            colnames(bigtab) <- gsub("_.*res[.]", " ", colnames(bigtab))

            assign(paste0("BMD.kw_",ca, "_", c),
                   bigtab,
                   envir = res)

            xbigtab <- xtable(bigtab)


            write.csv(bigtab,
                      file=paste0(bigtabsPath,
                                  "BMD.kw_",
                                  ca,
                                  "_",
                                  c,
                                  ".csv")
            )

            print(xbigtab, file=paste0(bigtabsPath,
                                       "BMD.kw_",
                                       ca,
                                       "_",
                                       c,
                                       ".tex"),
                  booktabs = T,
                  rotate.colnames = F
                  ,sanitize.text.function = function(x){x}
                  ,align="c"
            )

      }
}

bigtabsdata <- as.list(res)
save(bigtabsdata, file="./output/KruskalW/BMD.kw_bigtabs.Rda")
