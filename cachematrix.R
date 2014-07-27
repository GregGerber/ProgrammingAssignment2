## These functions store an invertible matrix,
## which can be called using the getmatrix()
## function. cacheSolve() computes the
## inverse of the matrix, which is stored
## in getinvmatrix(). If cacheSolve() is
## run after the inverse matrix has already
## been solved, cacheSolve() will use the
## cached matrix inverse.

## This function caches the value of a matrix.
## makeCacheMatrix assumes the matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	setmatrix <- function(y) {
		x <<- y
		m <<- NULL
	}
	getmatrix <- function() x
	setinvmatrix <- function(x) m <<- solve(x)
	getinvmatrix <- function() m
    list(setmatrix = setmatrix, getmatrix = getmatrix,
		 setinvmatrix = setinvmatrix,
		 getinvmatrix = getinvmatrix)
}


## cacheSolve computes the inverse of a matrix and caches the value.
## If the matrix inverse has already been computed, the cached value
## is returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        x$setinvmatrix(data)
}
