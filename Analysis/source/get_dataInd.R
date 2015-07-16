

## Function to pull data according to the list of simulations (list.simulations())

get.dataInd <- function(simlist, expno=NULL){

      values <- data.frame(ExpNo=integer(),
                           Folder=character(),
                           CellCode=character(),
                           SimNo=numeric(),
                           FGroup=character(),
                           IndDens=numeric(),
                           TimeStep=numeric(),
                           stringsAsFactors=F
      )


      if(is.null(expno)){

            sub <- simlist[simlist$CellCode!="Global", ]
            print("No experiments specified. Pulling all available data.")

      } else if (length(expno)==1){

            condition <- parse(text = condition_builder("ExpNo", "==", expno))
            sub.tmp <- subset(simlist, CellCode!="Global")
            sub <- subset(sub.tmp, eval(condition))


      }else{

            condition <- parse(text = condition_builder("ExpNo", "==", expno, "|"))
            sub.tmp <- subset(simlist, CellCode!="Global")
            sub <- subset(sub.tmp, eval(condition))

            #                   sub <- simlist[simlist$CellCode!="Global" & simlist$ExpNo==expno, ]
            print(paste("Pulling data for experiments:",
                        expno[1], "to", expno[length(expno)], sep=" "))
      }


      for(i in 1:nrow(sub)){


            nc <- nc_open(sub$Fullpath[i])


            if(length(nc$var)>12){
                  groups <- c(6,8,14)
            }else{
                  groups <- c(6,8)
            }

            tmp.nrows <- nc$dim[[1]]$len*length(groups)

            tmp_values <- data.frame(ExpNo=integer(tmp.nrows),
                                     Folder=character(tmp.nrows),
                                     CellCode=character(tmp.nrows),
                                     SimNo=numeric(tmp.nrows),
                                     FGroup=character(tmp.nrows),
                                     IndDens=numeric(tmp.nrows),
                                     TimeStep=numeric(tmp.nrows),
                                     stringsAsFactors=F
            )

            index.correct <- (0:(length(groups)-1)) * nc$dim[[1]]$len

            for(group in 1:length(groups)){
                  val <- ncvar_get(nc, nc$var[[groups[group]]])

                  for(tstep in 1:length(val)){

                        tmp_values[(tstep+index.correct[group]), ] <-
                              c(sub$ExpNo[i],
                                sub$Folder[i],
                                sub$CellCode[i],
                                sub$SimNo[i],
                                sub(" [[:alpha:]]*", "",
                                    gsub(" [[:alpha:]]*", "",
                                         nc$var[[groups[group]]]$name)),
                                val[tstep],
                                tstep
                              )

                  }

            }
            nc_close(nc)

            tmp_values$ExpNo <- as.integer(tmp_values$ExpNo)
            tmp_values$SimNo <- as.numeric(tmp_values$SimNo)
            tmp_values$TimeStep <- as.numeric(tmp_values$TimeStep)
            tmp_values$IndDens <- as.numeric(tmp_values$IndDens)

            print(paste("Currently Reading SimNo",
                        sub$SimNo[i],
                        "for",
                        sub$CellCode[i],
                        "of ExpNo", sub$ExpNo[i]))
            values <- rbind_list(values, tmp_values)
      }

      values$SimNo <- as.numeric(values$SimNo)
      values$TimeStep <- as.numeric(values$TimeStep)
      values$IndDens <- as.numeric(values$IndDens)
      return(values)
}
