##The first function you use to create a special matrix
## it sets and gets the value of the matrix and it will set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    	m <- NULL
    	set <- function(y) {
       		 x <<- y
       		 m <<- NULL
    }
    	get <- function() x
    	setinverse <- function(inverse) m <<- solve
    	getinverse <- function() m
    	list(set = set, get = get,
         	setinverse = setinverse,
         	getinverse = getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
    	if(!is.null(m)) {
        	message("getting cached data")
       		return(m)
    }
    	data <- x$get()
   	m <- solve(data, ...)
    	x$setinverse(m)
   	m
}
