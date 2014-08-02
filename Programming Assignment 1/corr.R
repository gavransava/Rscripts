corr <- function(directory, threshold = 0){
        paste(getwd(),directory, sep = "/")
        setwd(directory)        # positions us inside the specdata
        id <- c(1:length(list.files(path=".")))
        result <- c()
        #build file names
        filename = ""
        for (i in id){
                if(i%/%10 == 0)
                        filename = paste("00",i, ".csv", sep="")
                else if(i%/%100 ==0)
                        filename = paste("0",i,".csv", sep="")
                else
                        filename = paste(i,".csv", sep="")
                
                # open each csv file and compute corr
                temp <- read.table(filename,header=TRUE,sep=",")
                good <- complete.cases(temp)
                num  <- nrow(temp[good, ])
                temp <- temp[good, ]
                temp <- temp[,c(2,3)]
                
                if(num<threshold){
                        next
                }else{
                        result <- c(result,cor(temp)[1,2])
                }
        }
        setwd("/home/sava/Rscripts") # remove this later (back to parent)
        result
}