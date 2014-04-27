# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Initialize inverse to be a null matrix
    inv <- NULL 
	
	## Assign data to matrix x using input
	setmatrix<- function(y) {
                x <<- y
                inv <<- NULL
        }
	## Return Matrix
	getmatrix<- function() x
	
	## Calculate and assign Inverse
	setinv <- function(solve) inv <<- solve	
	
	## Get the Inverse
	getinv <- function() inv
	
	## List of methods
	list(setmatrix=setmatrix,
	     getmatrix=getmatrix,
		 setinv=setinv,
		 getinv=getinv)

}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## Check if matrix is Square
		if (dim(x$getmatrix())[1] == dim(x$getmatrix())[2])
		{
			## get the Inverse. Need not exist at this point
			inv <- x$getinv()
			## Check if the Inverse is already calculated
			if(!is.null(inv)) {
					message("getting cached data")
					return(inv)
			}
			## Get matrix
			data <- x$getmatrix()
			## Calculate Inverse
			inv <- solve(data)
			## Set it for later use
			x$setinv(inv)
			## Return the inverse
			inv
		}
		else {
			message("Cannot Calculate Inverse as the matrix is not a square")
            return(NULL)
		}
}
