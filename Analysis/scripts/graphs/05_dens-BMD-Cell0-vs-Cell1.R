
load("output/bootMedian_BMD.rda")


## extract data for Cell1 + Cell2
dat1 <- bootmedian_BMD$median[,,1]
dat2 <- bootmedian_BMD$median[,,2]

## Adjust Names for disting. after binding
dimnames(dat2)[[1]] <- paste0(dimnames(dat2)[[1]], "_Cell1")

#Bind and adjust rownames
datbar <- as.data.frame(rbind(dat1, dat2))
datbar$FGroup <- row.names(datbar)
datbar <- datbar[order(datbar$FGroup),]

auto.matbar <- as.matrix(datbar[grep("auto", datbar$FGroup), -9])
herb.matbar <- as.matrix(datbar[grep("herb", datbar$FGroup), -9])
car.matbar <- as.matrix(datbar[grep("car", datbar$FGroup), -9])





## Repeat for upper Conf
uci1 <- bootmedian_BMD$UCI[,,1]
uci2 <- bootmedian_BMD$UCI[,,2]

dimnames(uci2)[[1]] <- paste0(dimnames(uci2)[[1]], "_Cell1")

ucibar <- as.data.frame(rbind(uci1, uci2))
ucibar$FGroup <- row.names(ucibar)
ucibar <- ucibar[order(ucibar$FGroup),]
## Make sets for plotting
auto.ucibar <- as.matrix(ucibar[grep("auto", ucibar$FGroup), -9])
herb.ucibar <- as.matrix(ucibar[grep("herb", ucibar$FGroup), -9])
car.ucibar <- as.matrix(ucibar[grep("car", ucibar$FGroup), -9])



## Repeat for lower Conf

lci1 <- bootmedian_BMD$LCI[,,1]
lci2 <- bootmedian_BMD$LCI[,,2]

dimnames(lci2)[[1]] <- paste0(dimnames(lci2)[[1]], "_Cell1")

lcibar <- as.data.frame(rbind(lci1, lci2))
lcibar$FGroup <- row.names(lcibar)
lcibar <- lcibar[order(lcibar$FGroup),]

## Make sets for plotting
auto.lcibar <- as.matrix(lcibar[grep("auto", lcibar$FGroup), -9])
herb.lcibar <- as.matrix(lcibar[grep("herb", lcibar$FGroup), -9])
car.lcibar <- as.matrix(lcibar[grep("car", lcibar$FGroup), -9])





## Plotting
# ---- Auto
pos.auto <- barplot(auto.matbar, beside=T, ylim=c(0, max(auto.matbar, na.rm=T)+85000))
text(x=apply(pos.auto, MARGIN = 2, mean),
     apply(auto.matbar, MARGIN = 2, max)+70000, labels="***", cex=1.5)

abline(h=0)
arrows(x0 = as.numeric(pos.auto),
       y0 = as.numeric(auto.matbar),
       x1 = as.numeric(pos.auto),
       y1 = as.numeric(auto.ucibar))


arrows(x0 = as.numeric(pos.auto),
       y0 = as.numeric(auto.matbar),
       x1 = as.numeric(pos.auto),
       y1 = as.numeric(auto.lcibar))


## ---


# ------ HERB
par(mar=c(4.5,4,2,0.5))
pos.herb <- barplot(herb.matbar, beside=T, ylim=c(0, max(herb.matbar, na.rm=T)+17500))
text(x=apply(pos.herb, MARGIN = 2, mean),
     apply(herb.matbar, MARGIN = 2, max)+15000, labels="***", cex=1.5)

abline(h=0)
## -----


# ------ CAR
pos.car <- barplot(car.matbar, beside=T, ylim=c(0, max(car.matbar, na.rm=T)+15000))
text(x=apply(pos.car, MARGIN = 2, mean),
     apply(car.matbar, MARGIN = 2, max)+10000, labels="***", cex=1.5)

abline(h=0)
# END CAR ---


pos.auto