## In this script we create two functions for calculating the inverse of a matrix 
# and store it in cache

## This function creates a list of functions (set, get, setinverse, getinverse) for a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves the inverse of a matrix and store it.

cacheSolve <- function(x, ...) {

  # returns the inverse of the matrix if any
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # gets the data of matrix x for calculation
  data <- x$get()
  
  # Method to solve the inverse of a matrix
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
