#-------------------------------------------------------------------------------
#     Author:   Eileen Jiang
#     Date:     Nov 21, 2015
#     Purpose:  Assignment 2 Cached Matrix
#-------------------------------------------------------------------------------

##  The functions creates a special "matrix" object that can cache its inverse 
##  and allows the user to compute the inverse of the special "matrix" or
##  or retrieve the inverse from the cache if the inverse has previously been 
##  calculated. 

##  List of functions that allows the user to set the value of a matrix, 
##  get the value of the matrix, set the value of the matrix's inverse, and get
##  the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  Function retrieves the value of the inverse of the matrix set in
##  makeCacheMatrix if it has already been calculated and cached. If nothing 
##  has been cached, the function will calculate and return the inverse of the 
##  matrix and cache the result

cacheSolve <- function(x, ...) {
  #-----------------------------------------------
  # @args
  #   x:  object created by 'makeCacheMatrix'
  # @returns:
  #   inverse of a matrix
  #-----------------------------------------------
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
