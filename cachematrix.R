## The following functions make it possible to cache the inverse of a matrix. 
##If an inverse has already been calculated and the matrix has not changed, then it can be looked up in the cache rather than recomputed, which is very time-saving. 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y){ ## Set the value of the matrix
		x <<- y
		m <<- NULL
	}
	get <- function() x ## Get the value of the matrix
	setInverse <- function(solve) m <<- solve ## Set the value of the inverse matrix
	getInverse <- function() m ## Get the value of the inverse matrix
	list(set = set, get = get,  
		setInverse = setInverse,
		getInverse = getInverse) ## Store the cache
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated( and the matrix has not changed), the nthe cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
	if(!is.null(m)){ ## Getting cache if is available
		message("getting cache data")
		return(m)
	}
	data <- x$get() 
	m <- solve(data, ...) ## Calculate the inverse matrix
	x$setInverse(m)
	m ## Return a matrix that is the inverse of 'x'
}
