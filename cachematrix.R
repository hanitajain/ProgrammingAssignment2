## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)     # for getting inverse of non-square matrices also
makeCacheMatrix <- function(x = matrix()) {
  # initializing the inverse
  inv <- NULL
  
  ## setting the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() {
    ## returning the matrix
    x
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## To get the inverse of the matrix
  getInverse <- function() {
    inver <- ginv(x)
    inver%*%x
  }
  
  ## Return a list of methoda
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## return the inverse if its already set
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## return the matrix from our object
  data <- x$get()
  ## calculating the inverse using solve function
  inv <- solve(data,...)
  
  ## set the inverse to the object
  x$setInverse(inv)
  ## Return the matrix
  inv
}
