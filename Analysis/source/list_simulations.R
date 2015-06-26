## Data Preparation Functions for subsequent analysis
## A. Hurley - Dissertation 2015

## Experiments stored in seperate folders, each with 100 simulations over
##      100 years.


# Loading data ------------------------------------------------------------

# precursor directory
# setwd("D:/OneDrive/Dissertation/Dissertation/ModelOutput")


#Function to create a list of all simulations (incl. file locations)
#       for each experiment


list.simulations <- function(){

      #get list of folders:
      listfolders <- paste(list.dirs("./RAW", recursive=F), "/", sep="")

      listfiles <- data.frame(ExpNo=integer(),
                              Folder=character(),
                              FileName=character(),
                              CellCode=character(),
                              SimNo=numeric(),
                              Fullpath=character(),
                              stringsAsFactors=F
      )
      #get list of files in each dir for seasonal and aseasonal systems
      ##      (cf. SpecificLocations)
      for(dir in 1:length(listfolders)){
            index <- length(list.files(listfolders[dir],
                                       pattern="BasicOutputs"))
            tmp <- data.frame(ExpNo=integer(index),
                              Folder=character(index),
                              FileName=character(index),
                              CellCode=character(index),
                              SimNo=numeric(index),
                              Fullpath=character(index),
                              stringsAsFactors=F
            )

            for(n in 1:length(list.files(listfolders[dir],
                                         pattern="BasicOutputs"))){
                  tmp[n, ] <-
                        c(dir,
                          listfolders[dir],

                          list.files(listfolders[dir],
                                     pattern="BasicOutputs")[n],

                          gsub(".nc", "",
                               gsub(".*_", "",
                                    list.files(listfolders[dir],
                                               pattern="BasicOutputs"))
                          )[n],

                          gsub("_.*", "",
                               gsub("-*[[:alpha:]]_*", "",
                                    list.files(listfolders[dir],
                                               pattern="BasicOutputs"))
                          )[n],


                          paste(listfolders[dir],
                                list.files(listfolders[dir],
                                           pattern="BasicOutputs")[n]
                                ,sep="")
                        )
                  tmp$SimNo <- as.numeric(tmp$SimNo)

            }
            listfiles <- rbind(listfiles,tmp)
            listfiles <- listfiles[with(listfiles, order(ExpNo, CellCode, SimNo)), ]
      }
      return(listfiles)
}
