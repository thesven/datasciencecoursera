#set the working directory
setwd("/Volumes/StorageLocker/Coursera/Data Science/datasciencecoursera/R-Programming/Week 2 Assignement");

pollutantmean <-function(directory, pollutant, id =1:332) {
	#create a vector to hold our data
	pollutant_means = c();
	
	#get a list of all the files in the supplied directory
	files = list.files(directory, pattern="*.csv", full.names=TRUE);
	
	#loop over the required files and calculate the pollutant mean
	for(i in id){
	
		#load in the current file
		current_file = read.csv(files[i], header=TRUE, na.strings=c("NA"));
	
		#get the desired pollutant values for this file -- remove all N/A values
		pollutant_values <- na.omit(current_file[, pollutant])
	
		#store the data for later calculations
		pollutant_means <- c(pollutant_means, pollutant_values)	
	}
	return(round(mean(pollutant_means), 3))
}

#Output Test
#print(pollutantmean("specdata", "sulfate", 1:10))
#print(pollutantmean("specdata", "nitrate", 70:72))
#print(pollutantmean("specdata", "nitrate", 23))