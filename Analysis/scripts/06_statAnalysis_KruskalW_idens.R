## Kruskal Wallis tests for differences between groups

timecut <- statsAov[statsAov$TimeStep>=1080,]


load("./output/statsDens_medianCI.Rda")
load("./output/simConcise.Rda")
simConcise$ExpNo <- as.factor(simConcise$ExpNo)
statsDens$ExpNo <- as.factor(statsDens$ExpNo)

statsDens <- inner_join(statsDens, simConcise)


## Set up output paths:
outPath <- "./output/"
bigtabsPath <- "output/KruskalW/bigtabs/"


## Build subsets, run kruskalmc and record differences for both cells and all FG
## individually

## ------ IDENS ------

IDENS <- new.env() #Storage for output

IDENS$rankmedian <- new.env()

IDENS$ktest <- kruskal.test(statsDens$Median~statsDens$ExpNo)

cells <- unique(statsDens$CellCode)

fgroups <- unique(statsDens$FGroup)

expno <- levels(statsDens$ExpNo)
exptest <- expno[-8]
numexp <- length(exptest)

categ <- names(statsDens)[c(1,23)]

for(ce in cells){
      curPath <- paste0(outPath,"KruskalW/IDENS/", ce)
      xPath <- paste0(curPath, "/xtable")

      if(!is.factor(statsDens$ExpNo) | !is.factor(statsDens$FD)){
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

            assign(paste0(fg, "_dat"), subset(statsDens, FGroup==fg &
                                                    TimeStep>=1080 &
                                                    CellCode==ce),
                   envir = IDENS)




            for(cat in categ){


                  dat <- get(paste0(fg, "_dat"),
                             envir = IDENS)

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


                  assign(paste0(fg, "_", cat,  "_", ce, "_res"),

                         dat.tmp
                         ,
                         envir = IDENS
                  )


                  assign(paste0(fg, "_", cat,  "_", ce, "_ranks"),

                         tapply(rank(dat$Median),list(dat$ExpNo), median)
                         ,
                         envir = IDENS$rankmedian
                  )


                  write.csv(dat.tmp, file=paste0(curPath,"/",fg, "_", cat, "_",ce, "_res.csv"))

                  xtab <- xtable(dat.tmp)
                  print(xtab, file=paste0(xPath,"/",fg, "_", cat,  "_", ce, "_res.tex"),
                        sanitize.text.function = function(x) x)



            }


      }

}
IDENS.kw <- as.list(mget(ls(pattern = "res", envir = IDENS), envir = IDENS))

save(IDENS, files="output/KruskalW/IDENS_env.Rda")
save(IDENS.kw, file="output/KruskalW/IndDens_KruskalW.Rda")

if(!file.exists(bigtabsPath)){
      dir.create(bigtabsPath, recursive = T)
} else { }

res <- new.env()
for(ca in categ){
      for(c in cells){

            bigtab <- do.call(cbind,
                              mget(ls(pattern=paste0(ca, "_", c),
                                      envir = IDENS),
                                   envir = IDENS))

            colnames(bigtab) <- gsub("_.*res[.]", " ", colnames(bigtab))



            assign(paste0("IDENS.kw_",ca, "_", c),
                   bigtab,
                   envir = res)

            xbigtab <- xtable(bigtab)

            write.csv(bigtab,
                      file=paste0(bigtabsPath,
                                  "IDENS.kw_",
                                  ca,
                                  "_",
                                  c,
                                  ".csv")
            )

            print(xbigtab, file=paste0(bigtabsPath,
                                       "IDENS.kw_",
                                       ca,
                                       "_",
                                       c,
                                       ".tex"),
                  booktabs = T,
                  # ,rotate.colnames = T,
                  sanitize.text.function = function(x){x})

      }
}

bigtabsdata <- as.list(res)
save(bigtabsdata, file="./output/KruskalW/IDENS.kw_bigtabs.Rda")





