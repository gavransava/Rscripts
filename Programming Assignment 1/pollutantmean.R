pollutantmean <- function(directory, pollutant, id = 1:332){
        paste(getwd(),directory, sep = "/")
        setwd(directory)        # positions us inside the specdata
        finalMean = c()
        #build file names
        filename = ""
        for (i in id){
                if(i%/%10 == 0)
                        filename = paste("00",i, ".csv", sep="")
                else if(i%/%100 ==0)
                        filename = paste("0",i,".csv", sep="")
                else
                        filename = paste(i,".csv", sep="")
                
                # open each csv file and compute mean
                tempMean = 0 
                temp = read.table(filename,header=TRUE,sep=",")
                if     (pollutant=="sulfate")
                        tempMean = round(temp$sulfate, digits =3)
                else if(pollutant=="nitrate")
                        tempMean = round(temp$nitrate, digits =3)                                        
                else
                        break
                
                finalMean = c(finalMean,tempMean)
        }
        setwd("/home/sava/Rscripts") # remove this later (back to parent)
        result = round(mean(finalMean, na.rm=TRUE), digits =3)     
        result
}