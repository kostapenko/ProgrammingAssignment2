# This is solution of Programming Assignment 2 on Week 3. There are two 
# functions that cache the inverse of a matrix.


# This function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
        inversed <- NULL
        set <- function(y) {
                matrix <<- y
                inversed <<- NULL
        }
        get <- function() {
                matrix
        }        
        setinversed <- function(i) {
                inversed <<- i
        }        
        getinversed <- function() {
                inversed
        }        
       
        list(set = set, get = get,
             setinversed = setinversed,
             getinversed = getinversed)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(CacheMatrix, ...) {
        i <- CacheMatrix$getinversed()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- CacheMatrix$get()
        i <- solve(data, ...)
        CacheMatrix$setinversed(i)
        i
}

# Testing solution.

test <- makeCacheMatrix(matrix(1:4, ncol = 2, nrow = 2))
cacheSolve(test)
cacheSolve(test)

