## This assignment explains how caching function can potantially
## save time by caching time consuming computations.

## This function will create Matrix that would help cache inverse 
## matrix. 

makeCacheMatrix <- function(x = matrix()) {
	invm <- NULL
	set <- function (y){
		x <<-y
		invm <<- NULL
	}

	get <- function () x
	
	setinvm <- function (inversemetrix) invm <<- inversemetrix

	getinvm <- function() invm 
	
	list( set = set, get = get,
	      setinvm = setinvm,
	      getinvm = getinvm)		

}


## This function calculates the inverse of the special "Matrix" ## created with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	invm <- x$getinvm()
	if(!is.null(invm )) {
		message("getting cached data")
		return (invm)
	}
	
	data <- x$get()
	invm  <- solve(data, ...)
	x$setinvm(invm)
        invm  
}
