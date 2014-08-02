complete <- function(directory, id = 1:332){
        paste(getwd(),directory, sep = "/")
        setwd(directory)        # positions us inside the specdata
        m <- matrix(,nrow=0, ncol=2)
        #build file names
        filename = ""
        for (i in id){
                if(i%/%10 == 0)
                        filename = paste("00",i, ".csv", sep="")
                else if(i%/%100 ==0)
                        filename = paste("0",i,".csv", sep="")
                else
                        filename = paste(i,".csv", sep="")
                
                # open each csv file and compute complete cases
                
                temp <- read.table(filename,header=TRUE,sep=",")                    
                good <- complete.cases(temp)
                num <- nrow(temp[good, ])
                newrow <- c(i,num)
                m <- rbind(m,newrow)
                
        } 
        row.names(m) <-NULL
        m <- as.data.frame(m)
        colnames(m) <- c("id","nobs")
        setwd("/home/sava/Rscripts") # remove this later (back to parent)
        m
}