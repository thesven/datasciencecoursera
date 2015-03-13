#set the working directory
#setwd("/Volumes/StorageLocker/Coursera/Data Science/datasciencecoursera/R-Programming/Week 2 Assignement");

corr <- function(directory, threshold = 0){
	
	#get a list of all the fies in the supplied directory
	files <- list.files(directory, pattern="*.csv", full.names=TRUE)
	
	#create a vector to store or data
	corr_data <- vector()
	
	#variable for vector index
	index = 1
	#loop over each file
	for(i in files){
		
		#load in the current file
		current_file <- read.csv(i, header=TRUE, na.strings=c("NA"));
		
		#get the number of complete cases in the file
		number_complete_cases <- nrow(na.omit(current_file))
		
		#check to see if the number of complete cases is greater than the defined threshold
		if(number_complete_cases > threshold){
			corr_data[index] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs");
			index <- index + 1;
		}
		
	}
	
	return(corr_data)
	
}