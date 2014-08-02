rankall <- function(outcome, num = "best"){
        ## Read outcome data
        data <- read.table("outcome-of-care-measures.csv",
                           header=TRUE, sep =",");
        
        ## Check that outcome is valid
        if(outcome!="heart attack" & outcome!="heart failure"
           & outcome!="pneumonia") stop("invalid outcome")
        
        ## empty dataframe
        result <- data.frame(matrix(NA,nrow=0,ncol=2))
        names(result) <- c("hospital","state")
        
        ## For each state, find the hospital of the given rank
        
        if(outcome=="heart attack")
                data <- data[,c(2,7,11)] 
        else if(outcome=="heart failure")
                data <- data[,c(2,7,17)]
        else 
                data <- data[,c(2,7,23)]
        
        data <- data[ which(data[,3] != "Not Available"), ]
        data[,3] = as.numeric(as.character(data[,3]))
        
        data <- data[order(data[,3], data[,1]), ]   ## sorting numeric rate first, same values by name lexicaly 
        splitdata <- split(data,data$State)         ## data is split but the ascending order is kept in subsets
        
        for (i in 1:length(splitdata)){
                hospital=""
                state=""
                temp <- splitdata[[i]]
                nrows <- nrow(temp)
                temp$rank <- 1:nrows  ## adds a new column with ascending numbers representing rank (the values are sorted already)
                if(num == "best"){                        
                        hospital <- as.character(temp[temp$rank == 1,1])
                        state <- as.character(temp[temp$rank == 1,2])
                }
                else if(num == "worst"){
                        hospital <- as.character(temp[temp$rank == nrows,1])
                        state <- as.character(temp[temp$rank == nrows,2])
                }
                else if(num > nrows){    ## rank is higher than available rows 
                        hospital <- NA
                        state <- NA
                }                        
                else{
                        hospital <- as.character(temp[temp$rank == num,1])
                        state <- as.character(temp[temp$rank == num,2])
                        
                }
                newRow <- data.frame(hospital=hospital, state=state)
                result <- rbind(result,newRow)
        }
        result
}
