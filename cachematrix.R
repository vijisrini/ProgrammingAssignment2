#this function creates a matrix
# It sets the value of the matrix
# It gets the value of the matrix
# It sets the value of the inverse matrix
# It gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # Initialize the matrix
        m <- NULL
        # store the value of the matrix with the new value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the stored matrix value
        get <- function() x
        
        # set the inverse function to get the inverse of the matrix
        setinverse <- function(solve) m <<- solve
        # get the inverse function value
        getinverse <- function() m
        # return the list where each list is a function.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function calculates the inverse of the "matrix" created above.
 # This function computes the inverse of the special"matrix" returned by 
 # makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
 # has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x = matrix(), ...) {
        # get the cached inverse matrix value
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # If it is not cached already, calculate the inverse and store it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        # return the inverse matrix
        m
}
