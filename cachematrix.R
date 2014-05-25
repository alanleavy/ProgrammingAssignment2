## The functions in this file provide an efficient alternative to calling 
##  the standard solve() function repeatedly for the same matrix. 
##  This is achieved by calling solve() once and then caching the result 
##  for future reuse.
##
## Sample usage:
##    m <- matrix(c(4,7,6,2),2,2)
##    cm <- makeCacheMatrix(m)
##    cacheSolve(cm)


## makeCacheMatrix() takes a standard  matrix object and encapsulates 
##  it in an object with, effectively, the following private properties:
##    cached_matrix : a copy of the matrix that was passed to makeCacheMatrix()
##    cached_inverse : a copy of the inverse of cached_matrix. this property is 
##                      set to NULL, until such time as the setinverse() method 
##                      function is called for the object.Ideally,  this should 
##                      only be done by makeCacheMatrix()'s sister function, 
##                      cacheSolve().
## makeCacheMatrix() returns a named list of public method functions to operate 
##  on the new object:
##    set : set the value of the cached_matrix property
##    get : get the value of the cached_matrix property
##    setinverse : set the value of the cached_inverse property
##    getinverse : get the value of the cached_inverse property

makeCacheMatrix <- function(cached_matrix = matrix()) {
  
    ## private property for cacheing the inverse of the matrix
  cached_inverse <- NULL
  
    ## Method function
  set <- function(y) {
    cached_matrix <<- y
    cached_inverse <<- NULL
  }
  
  ## Method function 
  get <- function() cached_matrix
  
  ## Method function
  setinverse <- function(inverse) cached_inverse <<- inverse
  
  ## Method function
  getinverse <- function() cached_inverse
  
    ## return the list of method functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() takes an encapsulated matrix object, previously returned 
##  by makeCacheMatrix(), and returns its inverse. Aside from this first 
##  parameter, the other parameters and the returned result are identical 
##  to the standard solve() function, the only difference being that the
##  inverse of the encapsulated matrix is calculated only the first time 
##  cacheSolve() is called for it. For subsequent calls, the cached result 
##  is returned instead.

cacheSolve <- function(x, ...) {
  
    ## Get the cached inverse
  inverse <- x$getinverse()
  
    ## If it's not NULL, then return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
    ## Otherwise, calculate the inverse using solve(), cache it and return it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
