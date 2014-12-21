## The following two functions calculate and store the value of the inverse of an
## input matrix x. The first function constructs a list of functions whereby the 
## second function may store and/or access the global values of the matrix x and
## its inverse m in order to retrieve and return m if possible (sparing 
## calculation) or calculate m if necessary.

## The 'makeCacheMatrix' function defines the functions 'set': sets global/parent
## frame values for x and resets m to NULL (important for the control structure of 
## 'cacheSolve'); 'get': retrieves the global value x;'setinverse': sets the value
## of m to be the inverse of x; 'getinverse': retrieves the global value of the 
## inverse m. It returns these four functions as a list to be accessed by the 
## function 'cacheSolve.'

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


## The 'cacheSolve' function uses getinverse to retrieve the global value of m,
## the inverse of x. If the value is not NULL, the function returns the stored
## value of m forgoing redundant calculations. Otherwise, the 'get' function 
## is used to retrieve the global value x assigned to "data", 'solve' calculates
## the inverse of "data" then assigned to m, the calculated inverse of x is then
## stored in the global variable m, and finally m is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
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