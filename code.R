## Code for Reproducible Research Project 2

## Installing Packages and Loading Libraries

if(!require(dplyr, quietly = TRUE)) install.packages("dplyr")
library(dplyr)
if(!require(ggplot2, quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

## set working directory
## workingDirChar <- "~/Documents/DataScience_JH/ReproducibleResearch/project2"
workingDirChar <- "C:/Users/b1050549/Documents/RResearch_project2"
setwd(workingDirChar)

## clear the environment
rm(list = ls())

## Downloading, Loading, and Processing the Raw Data

dataFileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
variableFileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"
faqUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf"

download.file(dataFileUrl, "StormData.csv.bz2")
download.file(variableFileUrl, "variable_info.pdf")
download.file(faqUrl, "faq.pdf")

dateDownloaded <- date()

## Read data into R

stormData <- read.csv("StormData.csv.bz2", header = TRUE, na.strings = "")
dim(stormData)

## Check the first 10 rows and the summary

head(stormData, 10)
summary(stormData)

## Convert to data frame table

stormDataDfTbl <- tbl_df(stormData)

## Get the levels of the EVTYPE variable

origEVTYPE <- levels(stormDataDfTbl$EVTYPE)
origEVTYPE

## Function to show factor levels

getFactorLevels <- function(myVector) {
        myFactor <- as.factor(myVector)
        numberOfLevels <- length(levels(myFactor))
        numberOfLevels <- paste("# of levels in the factor =  ", 
                                numberOfLevels, sep = "")
        print(c(numberOfLevels, levels(myFactor)))
}

## Remove the leading whitespace

cleanEVTYPE <- stormDataDfTbl$EVTYPE
cleanEVTYPE <- trimws(cleanEVTYPE, which = "left")
cleanEVTYPEFact <- getFactorLevels(cleanEVTYPE)

## Convert EVTYPEs to upper case

cleanEVTYPE <- toupper(cleanEVTYPE)
cleanEVTYPEFact <- getFactorLevels(cleanEVTYPE)

## Get the levels of the PROPDMGEXP variable

print(levels(stormDataDfTbl$PROPDMGEXP))

## Get the levels of the CROPDMGEXP variable

print(levels(stormDataDfTbl$CROPDMGEXP))

## Convert the levels to upper case

cleanPROPDMGEXP <- toupper(stormDataDfTbl$PROPDMGEXP)
cleanCROPDMGEXP <- toupper(stormDataDfTbl$CROPDMGEXP)
cleanPROPDMGEXPFact <- getFactorLevels(cleanPROPDMGEXP)
cleanCROPDMGEXPFact <- getFactorLevels(cleanCROPDMGEXP)

## Function to find and replace character strings

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

## Convert the letters to numbers

cleanPROPDMGEXP <- cleanUp("H", "2", cleanPROPDMGEXP)
cleanPROPDMGEXP <- cleanUp("K", "3", cleanPROPDMGEXP)
cleanPROPDMGEXP <- cleanUp("M", "6", cleanPROPDMGEXP)
cleanPROPDMGEXP <- cleanUp("B", "9", cleanPROPDMGEXP)
cleanPROPDMGEXPFact <- getFactorLevels(cleanPROPDMGEXP)

cleanCROPDMGEXP <- cleanUp("K", "3", cleanCROPDMGEXP)
cleanCROPDMGEXP <- cleanUp("M", "6", cleanCROPDMGEXP)
cleanCROPDMGEXP <- cleanUp("B", "9", cleanCROPDMGEXP)
cleanCROPDMGEXPFact <- getFactorLevels(cleanCROPDMGEXP)

## Function to replace the symbols (e.g., "?", "+") with the NA logical constant

symbolToNA <- function(symbol, targetData) {
        indVectorInt <- grep(pattern = symbol, x = targetData, 
                             value = FALSE)
        for(i in seq_along(indVectorInt)) {
                is.na(targetData[indVectorInt[i]]) <- TRUE
        }
        targetData
}

## Convert symbols to NA

cleanCROPDMGEXP <- symbolToNA("\\?", cleanCROPDMGEXP)
cleanCROPDMGEXPFact <- getFactorLevels(cleanCROPDMGEXP)

cleanPROPDMGEXP <- symbolToNA("-", cleanPROPDMGEXP)
cleanPROPDMGEXP <- symbolToNA("\\?", cleanPROPDMGEXP)
cleanPROPDMGEXP <- symbolToNA("\\+", cleanPROPDMGEXP)
cleanPROPDMGEXPFact <- getFactorLevels(cleanPROPDMGEXP)

## Convert to numeric

cleanPROPDMGEXP <- as.numeric(cleanPROPDMGEXP)
cleanCROPDMGEXP <- as.numeric(cleanCROPDMGEXP)

## Calculate extended damage amounts

calcPROPDMG <- stormDataDfTbl$PROPDMG * 10 ^ cleanPROPDMGEXP
calcCROPDMG <- stormDataDfTbl$CROPDMG * 10 ^ cleanCROPDMGEXP
head(calcPROPDMG)
head(calcCROPDMG)

## Function to convert the NAs in the calculated damage vectors to 0

convertNAto0 <- function(dataSet) {
        for(j in seq_along(dataSet)) {
                if(is.na(dataSet[j])) {
                        dataSet[j] <- 0
                }
        }
        dataSet
}

## Use the function on the vectors

calcPROPDMG <- convertNAto0(calcPROPDMG)
calcCROPDMG <- convertNAto0(calcCROPDMG)

print(head(calcCROPDMG))

## Calculate the total damage (i.e., property + crop)

totalDMG <- calcPROPDMG + calcCROPDMG
head(totalDMG)

## Calculate the total health impacts (i.e., fatalities + injuries)

totalHealth <- stormDataDfTbl$FATALITIES +
        stormDataDfTbl$INJURIES
head(totalHealth)

## Add the new variables to the original storm data set

newStormDataDfTbl <- cbind(stormDataDfTbl, cleanEVTYPE, 
                           cleanPROPDMGEXP, cleanCROPDMGEXP,
                           calcPROPDMG, calcCROPDMG, totalDMG,
                           totalHealth)
summary(newStormDataDfTbl)

## Group the data by cleanEVTYPE and summarize it

## Health impacts

healthByCleanEVTYPE <- newStormDataDfTbl %>%
        group_by(cleanEVTYPE) %>%
        summarize(count = n(), DMG = sum(totalDMG), HEALTH =
                          sum(totalHealth),
                  totalFatalities = sum(FATALITIES),
                  totalInjuries = sum(INJURIES)) %>%
        arrange(desc(HEALTH))
head(healthByCleanEVTYPE, 10)

## Economic impact

econByCleanEVTYPE <- newStormDataDfTbl %>%
        group_by(cleanEVTYPE) %>%
        summarize(count = n(), DMG = sum(totalDMG), HEALTH =
                          sum(totalHealth),
                  totalFatalities = sum(FATALITIES),
                  totalInjuries = sum(INJURIES)) %>%
        arrange(desc(DMG))
head(econByCleanEVTYPE, 10)

## Function to search for character strings

mySearch <- function(find, where) {
        target <- paste("(^|[A-z]|[^A-z])+", find, "|", find, 
                        "($|[A-z]|[^A-z])+", sep = "")
        result <- grep(pattern = target, x = where, value = TRUE)
        resultFact <- getFactorLevels(result)
}

## Search for FLOOD

mySearch("FLOOD", cleanEVTYPE)


## Consolidate COASTAL FLOOD

mySearch("COASTAL", cleanEVTYPE)


cleanEVTYPE <- cleanUp("COASTAL EROSION", "COASTAL flood", cleanEVTYPE)

cleanEVTYPE <- cleanUp("COASTAL/TIDAL FLOOD", "COASTAL flood",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COASTALFLOOD", "COASTAL flood", 
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("CSTL FLOOD", "COASTAL flood", 
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COASTAL  FLOODING", "COASTAL flood",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("TIDAL FLOOD", "COASTAL flood",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COASTAL FLOOD", "COASTAL flood",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COASTAL flood", "COASTAL flood",
                       cleanEVTYPE)
mySearch("COASTAL", cleanEVTYPE)

cleanEVTYPEFact <- getFactorLevels(cleanEVTYPE)

## Clean FLASH FLOOD

mySearch("FLASH", cleanEVTYPE)

cleanEVTYPE <- cleanUp("FLASH", "FLASH flood", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FLASH flood", "FLASH flood",
                       cleanEVTYPE)
mySearch("FLASH", cleanEVTYPE)

## Clean LAKESHORE FLOOD

mySearch("LAKESHORE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LAKESHORE", "LAKESHORE flood",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("LAKESHORE flood", "LAKESHORE flood",
                       cleanEVTYPE)
mySearch("LAKESHORE", cleanEVTYPE)

## Consolidate the remaining FLOOD events

mySearch("FLOOD", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DROWNING", "FLOOD", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FLOOD", "FLOOD", cleanEVTYPE)
mySearch("FLOOD", cleanEVTYPE)

## Capitalize the other FLOOD types

cleanEVTYPE <- cleanUp("COASTAL flood", "COASTAL FLOOD",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("FLASH flood", "FLASH FLOOD",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("LAKESHORE flood", "LAKESHORE FLOOD",
                       cleanEVTYPE)

mySearch("FLOOD", cleanEVTYPE)
mySearch("COASTAL", cleanEVTYPE)

## Check cleanEVTYPEs

cleanEVTYPEFact <- getFactorLevels(cleanEVTYPE)


## Function to update the summaries of the health and economic impacts

updtByCleanEVTYPE <- function(issue) {
        updatedByCleanEVTYPE <- newStormDataDfTbl %>%
                select(-cleanEVTYPE) %>%
                cbind(cleanEVTYPE) %>%
                group_by(cleanEVTYPE) %>%
                summarize(count = n(), DMG =
                                  sum(totalDMG), 
                          HEALTH = sum(totalHealth),
                          totalFatalities =
                                  sum(FATALITIES),
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
                print("Argument needs to be either 'health' or
                      'economic'.")
        }
        print(head(updatedByCleanEVTYPE, 10))
}

## Review health impacts

healthUpdt <- updtByCleanEVTYPE("health")

## Review economic impacts

econUpdt <- updtByCleanEVTYPE("economic")


## Clean TORNADO

mySearch("TORNADO", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FUNNEL", "TORNADO", cleanEVTYPE)
cleanEVTYPE <- cleanUp("TORNADO", "TORNADO", cleanEVTYPE)
mySearch("TORNADO", cleanEVTYPE)

## Clean HEAT events

mySearch("HEAT", cleanEVTYPE)

cleanEVTYPE <- cleanUp("EXTREME HEAT", "EXCESSIVE heat",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXCESSIVE HEAT", "EXCESSIVE heat",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("HEAT", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXCESSIVE heat", "EXCESSIVE HEAT",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXCESSIVE HEAT", "EXCESSIVE HEAT",
                       cleanEVTYPE)
mySearch("HEAT", cleanEVTYPE)

## Clean LIGHTNING

mySearch("LIGHTNING", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LIGHTNING", "LIGHTNING", cleanEVTYPE)
mySearch("LIGHTNING", cleanEVTYPE)

## Clean ICE STORM

mySearch("ICE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("ICE", "ICE STORM", cleanEVTYPE)
mySearch("ICE", cleanEVTYPE)

## Clean WINTER STORM and WINTER WEATHER

mySearch("WINTER", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WINTER STORM", "winter storm",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("winter storm", "winter storm",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("WINTER", "winter weather", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WINTRY MIX", "winter weather", cleanEVTYPE)
cleanEVTYPE <- cleanUp("winter weather", "winter weather",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("winter storm", "WINTER STORM",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("winter weather", "WINTER WEATHER",
                       cleanEVTYPE)
mySearch("WINTER", cleanEVTYPE)
mySearch("WEATHER", cleanEVTYPE)

## Clean HURRICANE

mySearch("TYPHOON", cleanEVTYPE)
cleanEVTYPE <- cleanUp("TYPHOON", "HURRICANE/TYPHOON", cleanEVTYPE)
mySearch("HURRICANE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HURRICANE", "HURRICANE/TYPHOON",
                       cleanEVTYPE)
mySearch("HURRICANE", cleanEVTYPE)

## Clean STORM SURGE

mySearch("SURGE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("STORM SURGE", "STORM SURGE/TIDE",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COASTAL SURGE", "STORM SURGE/TIDE",
                       cleanEVTYPE)
mySearch("SURGE", cleanEVTYPE)

## Clean HAIL

mySearch("HAIL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("MARINE HAIL", "marine hail",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("marine hail", "marine hail",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("HAIL", "HAIL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("marine hail", "MARINE HAIL",
                       cleanEVTYPE)
mySearch("HAIL", cleanEVTYPE)


## Clean TROPICAL STORM

mySearch("TROPICAL STORM", cleanEVTYPE)
cleanEVTYPE <- cleanUp("COASTAL STORM", "TROPICAL STORM", cleanEVTYPE)
cleanEVTYPE <- cleanUp("COASTALSTORM", "TROPICAL STORM", cleanEVTYPE)
cleanEVTYPE <- cleanUp("TROPICAL STORM", "TROPICAL STORM",
                       cleanEVTYPE)
mySearch("TROPICAL STORM", cleanEVTYPE)

## Update Health and Economic Impacts

updtByCleanEVTYPE("health")
updtByCleanEVTYPE("economic")


## Look at EVTYPEs

cleanEVTYPEFact <- getFactorLevels(cleanEVTYPE)

## Clean FREEZE

mySearch("FREEZE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FROST", "freeze", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FREEZE", "freeze", cleanEVTYPE)
cleanEVTYPE <- cleanUp("freeze", "FROST/FREEZE", cleanEVTYPE)
mySearch("FREEZE", cleanEVTYPE)

## Clean SURF

mySearch("SURF", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RIP", "high surf", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SWELL", "high surf", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SURF", "high surf", cleanEVTYPE)
cleanEVTYPE <- cleanUp("BLOW-OUT TIDE", "high surf", cleanEVTYPE)
cleanEVTYPE <- cleanUp("high surf", "high surf", cleanEVTYPE)
cleanEVTYPE <- cleanUp("high surf", "HIGH SURF", cleanEVTYPE)
mySearch("SURF", cleanEVTYPE)
mySearch("HEAVY SURF", cleanEVTYPE)

## Start cleaning SNOW

mySearch("SNOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("BLIZZARD", "BLIZZARD", cleanEVTYPE)

cleanEVTYPE <- cleanUp("HEAVY WET SNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXCESSIVE SNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD SNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD MAY SNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOWFALL RECORD", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOWSTORM", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW AND COLD", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW AND WIND", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW/BLOWING SNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW/COLD", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW/RAIN", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW\\\\COLD", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("THUNDERSNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW/HEAVY SNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW AND HEAVY SNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HEAVY SNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("heavy snow", "heavy snow", cleanEVTYPE)


cleanEVTYPE <- cleanUp("HEAVY LAKE SNOW", "lake-effect snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LAKE EFFECT SNOW", "lake-effect snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LAKE-EFFECT SNOW", "lake-effect snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("lake-effect snow", "lake-effect snow", cleanEVTYPE)


cleanEVTYPE <- cleanUp("SNOW SQUALL", "WINTER WEATHER", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WINTER WEATHER", "WINTER WEATHER",
                       cleanEVTYPE)


mySearch("SNOW", cleanEVTYPE)

## Clean FREEZ and SLEET

mySearch("FREEZI", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FREEZING FOG", "freezing fog", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FREEZING", "SLEET", cleanEVTYPE)
mySearch("FOG", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FOG", "DENSE FOG", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DENSE FOG", "DENSE FOG", cleanEVTYPE)
cleanEVTYPE <- cleanUp("freezing fog", "FREEZING FOG", cleanEVTYPE)
mySearch("FOG", cleanEVTYPE)
mySearch("FREEZI", cleanEVTYPE)
mySearch("SLEET", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SLEET", "SLEET", cleanEVTYPE)
mySearch("SLEET", cleanEVTYPE)


## Check cleanEVTYPEs again

cleanEVTYPEFact <- getFactorLevels(cleanEVTYPE)



## Search for WIND

mySearch("WIND", cleanEVTYPE)

## Start cleaning WIND

cleanEVTYPE <- cleanUp("BITTER", "extreme cold1/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXTREME WIND", "extreme cold1/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXTREME COLD/wind chill", 
                       "extreme cold1/wind chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("extreme cold1/wind chill", 
                       "extreme cold1/wind chill", cleanEVTYPE)

cleanEVTYPE <- cleanUp("COLD/WINDS", "cold/wind chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WIND CHILL", "cold/wind chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("cold/wind chill", "cold/wind chill", cleanEVTYPE)

cleanEVTYPE <- cleanUp("MARINE TSTM WIND", "MARINE thunderstorm wind",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("WAKE LOW WIND", "MARINE high wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WIND AND WAVE", "MARINE high wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("MARINE STRONG WIND", "MARINE strong wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("MARINE strong wind", "MARINE strong wind", cleanEVTYPE)

mySearch("THUNDER", cleanEVTYPE)
cleanEVTYPE <- cleanUp("BURST", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("GUSTNADO", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("TSTM", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("THUNDER", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("THUDERSTORM", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("THUNDEERSTORM", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("THUNDESTORM", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("THUNERSTORM", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("TUNDERSTORM", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("STORM FORCE", "thunderstorm wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("thunderstorm wind", "thunderstorm wind", cleanEVTYPE)

mySearch("GUSTY", cleanEVTYPE)
cleanEVTYPE <- cleanUp("GUSTY", "HIGH wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HIGH  WIND", "HIGH wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WIND STORM", "HIGH wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WIND GUST", "HIGH wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HIGH WIND", "HIGH wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HIGH wind", "HIGH wind", cleanEVTYPE)

cleanEVTYPE <- cleanUp("NON-SEVERE WIND", "STRONG wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("GRADIENT WIND", "STRONG wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WHIRLWIND", "STRONG wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WIND ADVISORY", "STRONG wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WIND DAMAGE", "STRONG wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WINDS", "STRONG wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WND", "STRONG wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("STRONG WIND", "STRONG wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("STRONG wind", "STRONG wind", cleanEVTYPE)

mySearch("WIND", cleanEVTYPE)
mySearch("wind", cleanEVTYPE)


## Clean COLD

mySearch("COLD", cleanEVTYPE)

cleanEVTYPE <- cleanUp("EXCESSIVE COLD", "extreme cold1/wind
                       chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXTENDED COLD", "extreme cold1/wind
                       chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXTREME COLD", "extreme cold1/wind
                       chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXTREME/RECORD COLD", "extreme
                       cold1/wind chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD COLD", "extreme cold1/wind
                       chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD  COLD", "extreme cold1/wind
                       chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SEVERE COLD", "extreme cold1/wind
                       chill",cleanEVTYPE)
cleanEVTYPE <- cleanUp("UNUSUALLY COLD", "extreme cold1/wind
                       chill",cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXTREME COLD/wind chill", 
                       "extreme cold1/wind chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("extreme", 
                       "extreme cold1/wind chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("COLD AIR", "cold/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COLD AND", "cold/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COLD TEMPERATURE", "cold/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COLD WAVE", "cold/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COLD WEATHER", "cold/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("PROLONG COLD", "cold/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("UNSEASONABLE COLD", "cold/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("UNSEASONABLY COLD", "cold/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("COLD", "cold/wind chill", cleanEVTYPE)
cleanEVTYPE <- cleanUp("cold/wind chill", "cold/wind chill",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("extreme cold1/wind chill", 
                       "extreme cold1/wind chill", cleanEVTYPE)

mySearch("COLD", cleanEVTYPE)
mySearch("cold", cleanEVTYPE)

## Clean RAIN

mySearch("RAIN", cleanEVTYPE)

cleanEVTYPE <- cleanUp("EARLY RAIN", "early rain", cleanEVTYPE)
cleanEVTYPE <- cleanUp("MONTHLY RAINFALL", "monthly rainfall",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD LOW RAIN", "record low rainfall",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("ABNORMALLY WET", "heavy rain", cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXCESSIVE PRECIPITATION", "heavy rain", cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXCESSIVE WETNESS", "heavy rain", cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXTREMELY WET", "heavy rain", cleanEVTYPE)


cleanEVTYPE <- cleanUp("HEAVY RAIN", "heavy rain", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RAIN", "heavy rain", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HEAVY PRECIP", "heavy rain", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HEAVY SHOWER", "heavy rain", cleanEVTYPE)

cleanEVTYPE <- cleanUp("heavy rain", "heavy rain", cleanEVTYPE)
cleanEVTYPE <- cleanUp("heavy rain", "HEAVY RAIN", cleanEVTYPE)
cleanEVTYPE <- cleanUp("early rain", "EARLY RAIN", cleanEVTYPE)
cleanEVTYPE <- cleanUp("monthly rainfall", "MONTHLY RAINFALL",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("record low rainfall", "RECORD LOW RAIN",
                       cleanEVTYPE)

mySearch("RAIN", cleanEVTYPE)

## Finish SNOW

mySearch("SNOW", cleanEVTYPE)

cleanEVTYPE <- cleanUp("EARLY SNOW", "early snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FIRST SNOW", "early snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("early snow", "early snow", cleanEVTYPE)

cleanEVTYPE <- cleanUp("LATE SEASON SNOW", "late snow",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("LATE-SEASON SNOW", "late snow",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("LATE SNOW", "late snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("UNUSUALLY LATE SNOW", "late snow",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("late snow", "late snow", cleanEVTYPE)

cleanEVTYPE <- cleanUp("ACCUMULATED SNOW", "snow accumulation",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("DRIFTING SNOW", "snow accumulation",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("MONTHLY SNOW", "snow accumulation",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("MOUNTAIN SNOW", "snow accumulation",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("SEASONAL SNOW", "snow accumulation",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW ACCUMULATION", "snow accumulation",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("snow accumulation", "snow accumulation",
                       cleanEVTYPE)

cleanEVTYPE <- cleanUp("LIGHT SNOW", "light snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW SHOWERS", "light snow",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("light snow", "light snow", cleanEVTYPE)

cleanEVTYPE <- cleanUp("MODERATE SNOW", "moderate snow",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("moderate snow", "moderate snow",
                       cleanEVTYPE)

cleanEVTYPE <- cleanUp("BLOWING SNOW", "blowing snow",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("LACK OF SNOW", "lack of snow",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW ADVISORY", "snow advisory",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("WET SNOW", "wet snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SNOW", "heavy snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("blowing snow", "BLOWING SNOW",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("early snow", "EARLY SNOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("lack of snow", "LACK OF SNOW",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("lake-effect snow", "LAKE-EFFECT SNOW",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("late snow", "LATE SNOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("light snow", "LIGHT SNOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("moderate snow", "MODERATE SNOW",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("snow accumulation", "SNOW ACCUMULATION",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("snow advisory", "SNOW ADVISORY",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("wet snow", "WET SNOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("heavy snow", "HEAVY SNOW", cleanEVTYPE)

mySearch("SNOW", cleanEVTYPE)

## Finish WIND
mySearch("WIND", cleanEVTYPE)
mySearch("wind", cleanEVTYPE)

cleanEVTYPE <- cleanUp("WIND", "STRONG wind", cleanEVTYPE)
cleanEVTYPE <- cleanUp("STRONG wind", "STRONG WIND", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HIGH wind", "HIGH WIND", cleanEVTYPE)
cleanEVTYPE <- cleanUp("MARINE high wind", "MARINE HIGH WIND", cleanEVTYPE)
cleanEVTYPE <- cleanUp("MARINE strong wind", "MARINE STRONG WIND", cleanEVTYPE)
cleanEVTYPE <- cleanUp("thunderstorm wind", "THUNDERSTORM WIND", cleanEVTYPE)

cleanEVTYPE <- cleanUp("cold/wind chill", "COLD/WIND CHILL",cleanEVTYPE)
cleanEVTYPE <- cleanUp("extreme cold1/wind chill", "EXTREME COLD/WIND CHILL",
                       cleanEVTYPE)

mySearch("wind", cleanEVTYPE)
mySearch("WIND", cleanEVTYPE)

## Clean SUMMARY

mySearch("SUMMARY", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SUMMARY", "summary", cleanEVTYPE)
cleanEVTYPE <- cleanUp("summary", "summary", cleanEVTYPE)
cleanEVTYPE <- cleanUp("summary", "SUMMARY", cleanEVTYPE)
mySearch("SUMMARY", cleanEVTYPE)

## Clean FIRE

mySearch("FIRE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WILDFIRE", "wildfire", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FIRE", "wildfire", cleanEVTYPE)
cleanEVTYPE <- cleanUp("wildfire", "wildfire", cleanEVTYPE)
cleanEVTYPE <- cleanUp("wildfire", "WILDFIRE", cleanEVTYPE)
mySearch("FIRE", cleanEVTYPE)

## Clean SMOKE

mySearch("SMOKE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SMOKE", "DENSE SMOKE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DENSE SMOKE", "DENSE SMOKE", cleanEVTYPE)

mySearch("SMOKE", cleanEVTYPE)

## Clean DROUGHT

mySearch("DROUGHT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DRIEST MONTH", "DROUGHT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DRY", "DROUGHT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DROUGHT", "DROUGHT", cleanEVTYPE)
mySearch("DROUGHT", cleanEVTYPE)
mySearch("DRY", cleanEVTYPE)

## Precipitation
mySearch("PRECIP", cleanEVTYPE)
cleanEVTYPE <- cleanUp("PRECIP", "PRECIPITATION", cleanEVTYPE)
cleanEVTYPE <- cleanUp("PRECIPITATION", "PRECIPITATION", cleanEVTYPE)
mySearch("PRECIP", cleanEVTYPE)


## More cleaning

cleanEVTYPE <- cleanUp("ABNORMAL WARMTH", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("ABNORMALLY DRY", "DROUGHT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("AVALANCE", "AVALANCHE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("BEACH", "COASTAL FLOOD", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DAM FAILURE", "DAM BREAK", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DUST DEVEL", "dust devil", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DUST DEVIL", "dust devil", cleanEVTYPE)
cleanEVTYPE <- cleanUp("dust devil", "dust devil", cleanEVTYPE)
cleanEVTYPE <- cleanUp("dust devil", "DUST DEVIL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DUSTSTORM", "DUST STORM", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HEAVY MIX", "WINTER WEATHER", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HEAVY SEAS", "HIGH SURF", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HIGH SEAS", "HIGH SURF", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HIGH TEMPERATURE RECORD", "EXCESSIVE HEAT",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("HIGH TIDES", "HIGH SURF", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HIGH WATER", "flood", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HIGH WAVES", "HIGH SURF", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HOT", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HEAT", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HYPOTHERMIA", "HYPOTHERMIA/EXPOSURE", cleanEVTYPE)
cleanEVTYPE <- cleanUp("ICY ROADS", "ICE STORM", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LANDSLIDE", "DEBRIS FLOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LANDSLUMP", "DEBRIS FLOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LANDSPOUT", "TORNADO", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LIGHTING", "LIGHTNING", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LIGNTNING", "LIGHTNING", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LOW TEMPERATURE", "COLD/WIND CHILL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("LOW TEMPERATURE RECORD", " EXTREME COLD/WIND CHILL",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("MARINE MISHAP", "MARINE ACCIDENT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("MUD", "DEBRIS FLOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("ROCK SLIDE", "DEBRIS FLOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("DEBRIS FLOW", "DEBRIS FLOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("PROLONG WARMTH", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RAPIDLY RISING WATER", "flood", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD COOL", "EXTREME COLD/WIND CHILL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD LOW RAINFALL", "record low rainfall",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD LOW", "EXTREME COLD/WIND CHILL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("record low rainfall", "RECORD LOW RAINFALL",
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD HIGH", "EXCESSIVE HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD TEMPERATURE", "EXCESSIVE HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("EXCESSIVE HEAT", "EXCESSIVE HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("RECORD WARM", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HEAT", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FLOYD", "HURRICANE/TYPHOON", cleanEVTYPE)
cleanEVTYPE <- cleanUp("GLAZE", "FREEZING FOG", cleanEVTYPE)
cleanEVTYPE <- cleanUp("ROGUE WAVE", "HIGH SURF", cleanEVTYPE)
cleanEVTYPE <- cleanUp("ROUGH SEAS", "HIGH SURF", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SAHARAN DUST", "DUST STORM", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SMALL STREAM", "SMALL STREAM", cleanEVTYPE)
cleanEVTYPE <- cleanUp("SMALL STREAM", "SMALL STREAM", cleanEVTYPE)
cleanEVTYPE <- cleanUp("FLD", "flood", cleanEVTYPE)
cleanEVTYPE <- cleanUp("TORNDAO", "TORNADO", cleanEVTYPE)
cleanEVTYPE <- cleanUp("UNSEASONABLY COOL", "COLD/WIND CHILL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("UNSEASONAL", "COLD/WIND CHILL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("UNSEASONABLY WARM", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("UNUSUAL", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("URBAN", "URBAN/SMALL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("URBAN/SMALL", "URBAN/SMALL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("VERY WARM", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("VO", "VOLCANIC ASH", cleanEVTYPE)
cleanEVTYPE <- cleanUp("VOLCANIC ASH", "VOLCANIC ASH", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WALL CLOUD", "TORNADO", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WARM", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WATER", "WATERSPOUT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WAYTERSPOUT", "WATERSPOUT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WATERSPOUT", "WATERSPOUT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("COLD/WIND CHILL", "COLD/WIND CHILL", cleanEVTYPE)
cleanEVTYPE <- cleanUp("HEAT", "HEAT", cleanEVTYPE)
cleanEVTYPE <- cleanUp("BLOWING DUST", "DUST STORM", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WET SNOW", "wet snow", cleanEVTYPE)
cleanEVTYPE <- cleanUp("WET", "UNSEASONABLY WET", cleanEVTYPE)
cleanEVTYPE <- cleanUp("UNSEASONABLY WET", "UNSEASONABLY WET", cleanEVTYPE)
cleanEVTYPE <- cleanUp("wet snow", "WET SNOW", cleanEVTYPE)
cleanEVTYPE <- cleanUp("COASTAL FLOOD", "COASTAL FLOOD", 
                       cleanEVTYPE)
cleanEVTYPE <- cleanUp("flood", "flood", cleanEVTYPE)
cleanEVTYPE <- cleanUp("flood", "FLOOD", cleanEVTYPE)

## Check on remaining EVTYPEs

cleanEVTYPEFact <- getFactorLevels(cleanEVTYPE)

print(length(cleanEVTYPE))


## Update impacts
healthEvents <- updtByCleanEVTYPE("health")
economicEvents <- updtByCleanEVTYPE("economic")

## Plot impacts

health <- ggplot(head(healthEvents,5), aes(cleanEVTYPE, HEALTH))
health <- health + geom_point() + labs(x = "Event", y = 
                                               "Number of Fatalities and Injuries", title = 
                                               "Weather Events Most Harmful to Population Health")
print(health)

economic <- ggplot(head(economicEvents,5), aes(cleanEVTYPE, DMG))
economic <- economic + geom_point() + labs(x = "Event", y = 
                                                   "Total Property & Crop Damage ($)", title =
                                                   "Weather Events with Greatest Economic Consequences")
print(economic)

