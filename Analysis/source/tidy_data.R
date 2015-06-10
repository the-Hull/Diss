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


## Function to pull data according to the list of simulations (list.simulations())

get.data <- function(simlist, expno=NULL){
        
        values <- data.frame(ExpNo=integer(),
                             Folder=character(),
                             CellCode=character(),
                             SimNo=numeric(),
                             FGroup=character(),
                             MassDens=numeric(),
                             TimeStep=numeric(),
                             stringsAsFactors=F
                                )
        
        
        if(is.null(expno)){
                sub <- simlist[simlist$CellCode!="Global", ]
                print("No experiments specified. Pulling all available data.")
        }else{
                sub <- simlist[simlist$CellCode!="Global" & simlist$ExpNo==expno, ]
                print(paste("Pulling data for experiments:", 
                            expno[1], "to", expno[length(expno)], sep=" "))
        }
        
        
        for(i in 1:nrow(sub)){
                tmp_values <- data.frame(ExpNo=integer(),
                                         Folder=character(),
                                         CellCode=character(),
                                         SimNo=numeric(),
                                         FGroup=character(),
                                         MassDens=numeric(),
                                         TimeStep=numeric(),
                                         stringsAsFactors=F
                )
                
                nc <- nc_open(sub$Fullpath[i])
                
                if(length(nc$var)>12){
                        groups <- c(4,5,7,13)
                }else{
                        groups <- c(4,5,7)
                }
                
                for(group in groups){
                        val <- ncvar_get(nc, nc$var[[group]])
                        
                        for(tstep in 1:length(val)){
                                
                                tmp_values[tstep, ] <- 
                                        c(sub$ExpNo[i],
                                          sub$Folder[i],
                                          sub$CellCode[i],
                                          sub$SimNo[i],
                                          sub(" [[:alpha:]]*", "",   
                                              gsub(" [[:alpha:]]*", "",
                                                   nc$var[[group]]$name)),
                                          val[tstep],
                                          tstep
                                        )
                                
                        }
                        values <- rbind(values, tmp_values)
                }
                nc_close(nc)       
        }
        return(values)
}
