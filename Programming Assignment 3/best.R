best <- function(state, outcome){
        ## Read outcome data
        data <- read.table("outcome-of-care-measures.csv",
                           header=TRUE, sep =",");
        
        ## Check that state and outcome are valid
        u <- unique(data[,7]) %in% state        
        if (sum(u) ==0) stop("invalid state")
        if(outcome!="heart attack" & outcome!="heart failure"
           & outcome!="pneumonia") stop("invalid outcome")
        
        ## Return hospital name in the state with lowest death rate
        
        if(outcome=="heart attack")
                data <- data[,c(2,7,11)] 
        else if(outcome=="heart failure")
                data <- data[,c(2,7,17)]
        else 
                data <- data[,c(2,7,23)]
        data <- data[ which(data$State == state), ]
        data <- data[ which(data[,3] != "Not Available"), ]
        data[,3] = as.numeric(as.character(data[,3]))
        
        data <- data[order(data[,3], data[,1]), ]
        as.character(data[1,1])
}