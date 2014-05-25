
## the first function caches the value of the inverse 
## of a given reversible matrix and the second function 
## returns either the cached inverse matrix or calculates it

## makeCacheMatrix function takes a matrix and returns a list
## that has the values for get, set, getinv and setinv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  ##returns the following list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheSolve function checks to see if the inverse of the matrix 
##has already been calculated. If so it gets the cached value, if not 
## it calculates the inverse using the solve function. In either
## situation it returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  ##checkes to see if the inverse has been cached
  if(!is.null(m)) {
    ##returns the message if it finds the cached value
    message("getting cached data")
    ##returns the cached inverse matrix
    return(m)
  }
  data <- x$get()
  ##calculates the inverse of the matrix
  m <- solve(data, ...)
  #changes the value of setinv to the newly calculated matrix
  x$setinv(m)
  ##returns the inverse matrix
  m
}
