# R. Nunez April 25, 2015

# This code is submited for free as it without guarante and in accordance with 
# the Honor Code in Courser.com - R Programming course.

# Since matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly, 
# then the function listed below will work to cache the inverse of a matrix: 

# a) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# b) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not # changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
      # We need to initialize two variables for a dimensions matrix (m, y) instead of a single variable for a vector
      m <- NULL
      y <- NULL
      setmatrix <- function(y) {
            x <<- y
            m <<- NULL
      }
      getmatrix <- function() x                  # Returns matrix 
      setinverse <- function(solve) m <<- solve  
      getinverse <- function() m                 
                                                 # Create a lkist of the functions used within this function
      list(setmatrix = setmatrix, getmatrix = getmatrix,getinverse = getinverse,setinverse = setinverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
# This function computes, caches, solves, set and returns matrix inverse
      
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }

      data <- x$getmatrix()
      x$setmatrix(data)
      m <- solve(data, ...)
      x$setinverse(m)
      m
}