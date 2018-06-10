#WEEK 4 assignment
#Plot the 30-day mortality rates for heart attack
outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome_data)

outcome_data[, 11] <- as.numeric(outcome_data[,11])
hist(outcome_data[, 11], xlab = "Deaths", col = "Blue", main = "30-Day rates from heart attack")






#Finding the best hospital in a state
best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    df <- as.data.frame(cbind(outcome_data[, 2],   #name
                              outcome_data[, 7],   #state
                              outcome_data[, 11],  #heart attack
                              outcome_data[, 17],  #heart failure
                              outcome_data[, 23]), #pneumonia
                        stringsAsFactors = FALSE) 
                                               
    colnames(df) <- c("hospital_name","state","heart attack", "heart failure", "pneumonia")
    outcome_categories <- c("heart attack", "heart failure", "pneumonia")
    
    if(!state %in% df[, "state"]){
        stop('invalid state')
        }
    else if(!outcome %in% outcome_categories){
        stop('invalid outcome')
        }
    else{
        x <- which(df[,"state"] == state)
        y <- df[x, ]
        z <- as.numeric(y[, eval(outcome)])
        minimum <- min(z, na.rm = TRUE)
        hospital <- y[, "hospital_name"][which(z == minimum)]
        sorted <- sort(hospital)
        }
    ## Return hospital name in that state with lowest 30-day death
    return (sorted)
    ## rate
}



#Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
     df <- as.data.frame(cbind(outcome_data[, 2],   #name
                              outcome_data[, 7],    #state
                              outcome_data[, 11],   #heart attack
                              outcome_data[, 17],   #heart failure
                              outcome_data[, 23]),  #pneumonia
                        stringsAsFactors = FALSE) 
    
    colnames(df) <- c("hospital_name","state","heart attack", "heart failure", "pneumonia")
    outcome_categories <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if(!state %in% df[, "state"]){
        stop('invalid state')
    }
    else if(!outcome %in% outcome_categories){
        stop('invalid outcome')
    }
    else if(is.numeric(num)){
        x <- which(df[,"state"] == state)
        y <- df[x, ]
        z <- as.numeric(y[, eval(outcome)])
        y <- y[order(z, y[, "hospital_name"]), ]
        rank_result <- y[, "hospital_name"][num]
    }
    else if(!is.numeric(num)){
        if(num == "best"){
            result <- best(state, outcome)
        }
        else if(num =="worst"){
            x <- which(df[, "state"] == state)
            y <- df[x, ]
            z <- as.numeric(y[, eval(outcome)])
            y <- y[order(z, y[, "hospital_name"], decreasing = TRUE), ]
            rank_result <- y[, "hospital_name"][1]
        }
        else{
            stop('invalid rank')
        }
        
    }
    
    ## Return hospital name in that state with the given rank
    return(rank_result)
    ## 30-day death rate
}






    
    # Ranking hospitals in all states
    rankall <- function(outcome, num = "best"){
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        fd   <- as.data.frame(cbind(data[, 2],  # hospital
                                    data[, 7],  # state
                                    data[, 11],  # heart attack
                                    data[, 17],  # heart failure
                                    data[, 23]), # pneumonia
                              stringsAsFactors = FALSE)
        colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        fd[, eval(outcome)] <- as.numeric(fd[, eval(outcome)])
        
        ## Check that state and outcome are valid
        
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
            stop('invalid outcome')
        } else if (is.numeric(num)) {
            by_state <- with(fd, split(fd, state))
            ordered  <- list()
            for (i in seq_along(by_state)){
                by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                     by_state[[i]][, "hospital"]), ]
                ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
            }
            result <- do.call(rbind, ordered)
            output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
            names(output) <- c("hospital", "state")
        } else if (!is.numeric(num)) {
            if (num == "best") {
                by_state <- with(fd, split(fd, state))
                ordered  <- list()
                for (i in seq_along(by_state)){
                    by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                         by_state[[i]][, "hospital"]), ]
                    ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
                }
                result <- do.call(rbind, ordered)
                output <- as.data.frame(result, stringsAsFactors = FALSE)
                rownames(output) <- output[, 2]
            } else if (num == "worst") {
                by_state <- with(fd, split(fd, state))
                ordered  <- list()
                for (i in seq_along(by_state)){
                    by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                         by_state[[i]][, "hospital"], 
                                                         decreasing = TRUE), ]
                    ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
                }
                result <- do.call(rbind, ordered)
                output <- as.data.frame(result, stringsAsFactors = FALSE)
                rownames(output) <- output[, 2]
            } else {
                stop('invalid num')
            }
        }
        return(output)
    }
    
    # example output:
    r <- rankall("heart attack", 4)
    as.character(subset(r, state == "HI")$hospital)