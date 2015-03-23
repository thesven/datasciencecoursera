##set the working directory
##setwd("/Volumes/StorageLocker/Coursera/Data Science/datasciencecoursera/R-Programming/Week 4 Assignment");

getHospitalForState <- function(data, state, outcome, ranking){
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

getOutcomeNumber <- function(outcome){
	if(outcome == "heart attack"){
		return(11);
	} else if(outcome == "heart failure"){
		return(17);
	} else if(outcome == "pneumonia"){
		return(23);
	} else {
		stop("invalid outcome");
	}
}

rankall <- function(outcome, num = "best") {

    ## Read outcome data
    rawData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", header=TRUE, na.strings=c("Not Available", "NA"), colClasses="character");
	## Check that state and outcome are valid
	validOutcomes <- c("heart attack", "heart failure", "pneumonia");
	if(!any(outcome == validOutcomes)){
		stop("invalid outcome");
	} else {
		
		## For each state, find the hospital of the given rank
		## Return a data frame with the hospital names and the
		## (abbreviated) state name

		## get a list of all the states
		states <- sort(unique(rawData$State));
		numStates <- length(states);
		hospitals <- rep("", numStates);
		outcomeNumber <- getOutcomeNumber(outcome);

		for(i in 1:numStates){
			stateData <- subset(rawData, State==states[i]);
			hospitals[i] <- getHospitalForState(stateData, states[i], outcomeNumber, num);
		}

		return(data.frame(hospital=hospitals, state=states));

	}


	
}
