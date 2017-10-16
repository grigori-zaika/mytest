rankhospital <- function(state, outcome, num = "best") {
        
        #require(c("Hmisc", "dplyr"))
        
        outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) stop("invalid outcome")  
        if (state %in% unique(outcome.data[,"State"]) == FALSE) stop("invalid state")
        
        
        part1 <- capitalize(strsplit(outcome, " ")[[1]][1])
        part2 <- capitalize(strsplit(outcome, " ")[[1]][2])
        column <- ifelse(!is.na(part2),
                         paste("Hospital.30.Day.Death..Mortality..Rates.from", part1, part2, sep = "."),
                         paste("Hospital.30.Day.Death..Mortality..Rates.from", part1, sep = "."))
        
        outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- suppressWarnings(as.numeric(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- suppressWarnings(as.numeric(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- suppressWarnings(as.numeric(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        
        test.outcome <- dplyr::filter(outcome.data, State == state)
        rank.table <- cbind(test.outcome[which(colnames(outcome.data)=="Hospital.Name")],
                            test.outcome[which(colnames(outcome.data)==column)])
        rank.table <- rank.table[!is.na(rank.table[2]),]
        rank.table <- rank.table[order(rank.table[2],rank.table[1]),]
        rank.table[3] <- rank(rank.table[2], na.last = TRUE, ties.method= "first")
        names(rank.table)[3] <- "Rank"
        
        if (num=="best") {
                result <- rank.table[,1][1]    
        } else if (num=="worst") {
                result <- rank.table[,1][nrow(rank.table)]  
        } else if (num > nrow(rank.table)) {
                result <- 'NA'
        } else {
                result <- dplyr::filter(rank.table, Rank == num)[,1][1]
        }
        
        print(result)
        
}