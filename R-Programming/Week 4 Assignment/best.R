#set the working directory
setwd("/Volumes/StorageLocker/Coursera/Data Science/datasciencecoursera/R-Programming/Week 4 Assignment");

findLowest <- function(outcome, data){
	validOutcomes <- c("heart attack", "heart failure", "pneumonia");
	if(outcome == validOutcomes[1]){
		##heart attack
		values <- data[, 11]
	}

	if(outcome == validOutcomes[2]){
		##heart failure
		values <- data[, 17]
	}

	if(outcome == validOutcomes[3]){
		##neumonia
		values <- data[, 23]
	}

	lowest <- min(values, na.rm=TRUE);
	index <- which(values == lowest);

	return(data[index, 2]);

}

best <- function(state, outcome) {

	##read outcome data
	rawData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", header=TRUE, na.strings=c("Not Available"), colClasses="character");
	rawData[,11] <- as.numeric(rawData[,11]);
	rawData[,17] <- as.numeric(rawData[,17]);
	rawData[,23] <- as.numeric(rawData[,23]);

	## Check that state and outcome are valid
	validOutcomes <- c("heart attack", "heart failure", "pneumonia");
	if(!any(rawData$State == state)){
		stop("invalid state");
	} else if(!any(outcome == validOutcomes)){
		stop("invalid outcome");
	} else {
		## Return hospital name in that state with lowest 30-dat deatrate

		dataForState <- subset(rawData, State == state);

		if(outcome == validOutcomes[1]){
			##heart attack
			return(findLowest(validOutcomes[1], dataForState));
		}

		if(outcome == validOutcomes[2]){
			##heart failure
			return(findLowest(validOutcomes[2], dataForState));
		}

		if(outcome == validOutcomes[3]){
			##neumonia
			return(findLowest(validOutcomes[3], dataForState));
		}
	}

}