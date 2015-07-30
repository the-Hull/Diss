## Produce Barplots of Median Biomass / Abundance density
## requires output from boot_resample()


data_barplot <- function(dat,cell,logscale=F, addauto=F,...){

      cols <- data.frame(FGroup=c("autotrophs",
                                  "carnivores",
                                  "herbivores",
                                  "omnivores"),
                         Color=c("#A7F62F",
                                 "#CC2614" ,
                                 "#44CA9F" ,
                                 "#FFA41A"),
                         stringsAsFactors = F)

      cols <- cols[c(1,3,4,2), ]

      ce <- cell+1
      if(addauto==F){
            if(nrow(dat$median)==3){
                  cols <- cols[-1, ]
            }
      }


      if(logscale==T){
            dat.plot <- log10(dat$median[,,ce])
            uci <- log10(dat$UCI[,,ce])
            lci <- log10(dat$LCI[,,ce])

      } else {

            dat.plot <- dat$median[,,ce]
            uci <- dat$UCI[,,ce]
            lci <- dat$LCI[,,ce]

      }

      if(addauto==T){
            dat.plot <- rbind(autotroph=rep(0,8),
                                      dat.plot)

            uci <- rbind(autotroph=rep(0,8),uci)
            lci <- rbind(autotroph=rep(0,8),lci)
      } else {

      }


      xmax <- prod(dim(dat.plot))+
            0.5+
            (ncol(dat.plot)-1)
      ymax <- max(uci*1.04, na.rm=T)

      bars <- barplot(dat.plot,
              beside=T,
              col=cols$Color,
              xlim=c(0,xmax),
              xaxs='i',
              ylim=c(0,ymax),
              ...)

      abline(h=0,
             col="black",
             lwd=1.5)
      # axis(side=1, at = bars)

      arrows(x0 = bars,
             y0 = dat.plot,
             x1 = bars,
             y1 = uci,
             angle = 90,
             length = .07,
             lwd = 1,
             col="gray10")

      arrows(x0 = bars,
             y0 = dat.plot,
             x1 = bars,
             y1 = lci,
             angle = 90,
             length = .07,
             lwd = 1,
             col="gray10")


            invisible(list(data=dat.plot, loc=bars))

}