#set the working directory
setwd("/Volumes/StorageLocker/Coursera/Data Science/datasciencecoursera/R-Programming/Week 2 Assignement");

complete <- function(directory, id=1:332){
	
	#get a list of all the fies in the supplied directory
	files = list.files(directory, pattern="*.csv", full.names=TRUE);
	
	#create an empty vector for holding values
	cases <- vector();
	
	#current file position
	index <- 1
	for(i in id){
		
		#load the current file
		current_file <- read.csv(files[i], header=TRUE, na.strings=c("NA"))
		
		#find the amount of compelte cases in the file
		complete_cases <- nrow(na.omit(current_file))
		
		#add them to our storage for later output
		cases[index] <- complete_cases
		
		#increment our file index by 1
		index <- index + 1;
		
	}
	
	result <- data.frame(id=id, nobs=cases);
	return(result);
	
}

#Output Test
#print(complete("specdata", 1))
#print(complete("specdata", c(2, 4, 8, 10, 12)))
#print(complete("specdata", 30:25))
#print(complete("specdata", 3))