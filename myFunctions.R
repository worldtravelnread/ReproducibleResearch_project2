## Functions written for the Reproducible Research course - Project 2
## Sharon Francisco

## get factor levels
getFactorLevels <- function(myVector) {
        myFactor <- as.factor(myVector)
        numberOfLevels <- length(levels(myFactor))
        numberOfLevels <- paste("# of levels in the factor =  ", 
                                numberOfLevels, sep = "")
        print(c(numberOfLevels, levels(myFactor)))
}


## search and replace text in a data set
cleanUp <- function(find, replace, data) {
        beforePattern <- paste("(^|[A-z]|[^A-z])+", find, 
                               sep = "")
        afterPattern <- paste(find, "($|[A-z]|[^A-z])+", 
                              sep = "")
        data <- gsub(pattern = beforePattern, 
                     replacement = replace, x = data)
        data <- gsub(pattern = afterPattern, 
                     replacement = replace, x = data)
}


## convert symbols in a data set to NA
symbolToNA <- function(symbol, targetData) {
        indVectorInt <- grep(pattern = symbol, x = targetData, 
                             value = FALSE)
        for(i in seq_along(indVectorInt)) {
                is.na(targetData[indVectorInt[i]]) <- TRUE
        }
        targetData
}


## convert NAs in a data set to 0
convertNAto0 <- function(dataSet) {
        for(j in seq_along(dataSet)) {
                if(is.na(dataSet[j])) {
                        dataSet[j] <- 0
                }
                
        }
        dataSet
}


## search function
mySearch <- function(find, where) {
        target <- paste("(^|[A-z]|[^A-z])+", find, "|", find, 
                        "($|[A-z]|[^A-z])+", sep = "")
        result <- grep(pattern = target, x = where, value = TRUE)
        resultFact <- getFactorLevels(result)
}


## update the health or economic impacts in the data set
updtByCleanEVTYPE <- function(issue) {
        updatedByCleanEVTYPE <- newStormDataDfTbl %>%
                select(-cleanEVTYPE) %>%
                cbind(cleanEVTYPE) %>%
                group_by(cleanEVTYPE) %>%
                summarize(count = n(), DMG = sum(totalDMG), 
                          HEALTH = sum(totalHealth),
                          totalFatalities = sum(FATALITIES),
                          totalInjuries = sum(INJURIES))
        if(issue == "health") {
                updatedByCleanEVTYPE <- updatedByCleanEVTYPE %>%
                        arrange(desc(HEALTH))
        }
        else if(issue == "economic") {
                updatedByCleanEVTYPE <- updatedByCleanEVTYPE %>%
                        arrange(desc(DMG))
        }
        else {
                print("Argument needs to be either 'health' or 'economic'.")
        }
        print(head(updatedByCleanEVTYPE, 10))
}


