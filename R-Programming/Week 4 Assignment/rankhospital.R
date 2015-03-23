##set the working directory
##setwd("/Volumes/StorageLocker/Coursera/Data Science/datasciencecoursera/R-Programming/Week 4 Assignment");

getHospitalByRankForOutcome <- function(data, outcome, ranking){
	data[,outcome] <- as.numeric(data[,outcome]);
	sortedData <- data[order(data[,outcome], data[,2], na.last=NA),];
	length <- nrow(sortedData);
	if(ranking == "best"){
		return(sortedData[1, 2]);
	} else if(ranking == "worst"){
		return(sortedData[length, 2]);
	} else {
		if(ranking > as.numeric(ranking)){
			return("NA");
		} else{
			return(sortedData[as.numeric(ranking), 2]);
		}
	}
}

rankhospital <- function(state, outcome, num = "best"){
	## Read outcome data
	rawData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", header=TRUE, na.strings=c("Not Available", "NA"), colClasses="character");
	
	## Check that state and outcome are valid
	validOutcomes <- c("heart attack", "heart failure", "pneumonia");
	if(!any(rawData$State == state)){
		stop("invalid state");
	} else if(!any(outcome == validOutcomes)){
		stop("invalid outcome");
	} else {
		## Return hospital name in that state with the given rank
		## 30-day death rate
		stateData <- subset(rawData, State == state);
		if(outcome == validOutcomes[1]){
			##heart attack
			return(getHospitalByRankForOutcome(stateData, 11, num));
		}

		if(outcome == validOutcomes[2]){
			##heart failure
			return(getHospitalByRankForOutcome(stateData, 17, num));
		}

		if(outcome == validOutcomes[3]){
			##neumonia
			return(getHospitalByRankForOutcome(stateData, 23, num));
		}

	}
	
}