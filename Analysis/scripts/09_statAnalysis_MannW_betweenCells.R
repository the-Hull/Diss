load("./output/statsAov.Rda")
statsAov$FGroup <- as.factor(statsAov$FGroup)


## Script: Compare BMD for a fg between both systems in one experiment.
## Repeat for all FG. Store output in

exp <- unique(statsAov$ExpNo)

fgroups <- unique(statsAov$FGroup)

CELLS <- new.env()
CELLS$median <- new.env()
CELLS$mwu <- new.env()
categ <- names(statsAov)[2]

res.df <- data.frame(FGroup=character(31),
                     ExpNo=integer(31),
                     U=integer(31),
                     P=numeric(31),
                     Sig=character(31),
                     Cell0=integer(31),
                     Cell1=integer(31),
                     count0 = integer(31),
                     count1 = integer(31),
                     stringsAsFactors = F)

i <- 1

for(fg in fgroups){

      for(ex in exp){

            if(fg=="carnivore" & ex==8){

            } else {

                  tmp.dat <- subset(statsAov, FGroup==fg & ExpNo==ex & TimeStep>=1080)

                  n <- tapply(tmp.dat$Median, list(tmp.dat$CellCode), median)

                  n1 <- n[[1]]
                  n2 <- n[[2]]


                  assign(x = paste0("BMD_", fg, ex, "_mwu"),

                         wilcox.test(as.formula(paste("Median~", categ)),
                                     data=tmp.dat
                                     ),
                         CELLS$mwu)


                  assign(x = paste0("BMD_", fg, ex, "_median"),

                         tapply(tmp.dat$Median, list(tmp.dat$CellCode), median),

                         CELLS$median)


                  nobs <- table(tmp.dat$CellCode)

                  curr.test <- get(paste0("BMD_", fg, ex, "_mwu"),
                      envir = CELLS$mwu)

                  res.df[i, ] <- c(fg,
                                   ex,
                                   as.numeric(curr.test[1]),
                                   as.numeric(curr.test[3]),
                                   ifelse(as.numeric(curr.test[3])<0.05,"Sig","NA" ),
                                   n1,
                                   n2,
                                   nobs[[1]],
                                   nobs[[2]]
                                   )
                  i <- i+1
            }
      }
}

res.df$U <- as.numeric(res.df$U)
res.df$P <- as.numeric(res.df$P)
res.df$Cell0 <- as.numeric(res.df$Cell0)
res.df$Cell1 <- as.numeric(res.df$Cell1)

MannW_betweenCells <- res.df
save(MannW_betweenCells, file="output/MannW/betweenCells.Rda")
