## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. The makeCacheMatrix and cacheSolve functions will aid in the creation of a matrix with a cahced inverse

## makeCacheMatrix creates a special matrix object that has the ability cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	
	##will hold the inverse matrix once set, initialized to null
	inverse_matrix <- NULL
	
	##function for setting the matrix
	set <- function(new_matrix){
		x <<- y
		inverse_matrix <<- NULL
	}
	
	##function for getting the current value of the matrix
	get <- function(){
		return(x)
	}
	
	##function for getting the inverse matrix
	get_inverse <- function(){
		return(inverse_matrix)
	}
	
	##function for setting the inverse matrix
	set_inverse <- function(inverse){
		inverse_matrix <<- inverse
	}
	
	##create a list of the values for access
	list(set=set, get=get, get_inverse=get_inverse, set_inverse=set_inverse)
}


## Returns the inverse of a matrix (assumes the matrix is always invertable). Will check to see if the inverse has already been computed, and returns a cached result if available. If not it will compute the inverse and store the value in cache for later use and return the value
cacheSolve <- function(x, ...) {
	
	#get the current inverted value
	inverse = x$get_inverse()
	
	##check to see if the inverse has been set
	if(is.null(inverse)){
		
		##the value has not been cached
		
		##get the current value of the matrix and invert
		inverse <- solve( x$get())
		
		##cache it to the matrix passed into the function
		x$set_inverse(inverse)
		
		return(inverse)
		
	} else {
		
		return(inverse)
	
	}
}
