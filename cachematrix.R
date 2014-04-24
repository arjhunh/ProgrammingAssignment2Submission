##The first function makeCachematrix takes a matrix as input and creates a "Special Matrix".
##The second function cacheSolve uses the first function in its implementation and calculates
##the inverse.

## makeCacheMatrix takes a matrix as its input, creates a "Special matrix" and returns a list that 
##comprises of 4 functions get, set, getInverse and setInverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) i <<- Inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes the makeCacheMatrix function in its implementation. It takes the
##"special matrix" from the first function. The output is the inverse from either the cache or 
##computation

cacheSolve <- function(x, ...) {
  i <- x$getInverse() ## takes the "special matrix" from the first function
  if(!is.null(i))     ## checks if there is a cache
  {
    message("getting cached data")
    return(i)         ## if there is, it just returns the cached output ie. the inverse.
  }
  data <- x$get()         ## if the there is no cache 
  i <- solve(data, ...)   ## inverse is calculated here
  x$setInverse(i)         ## save it to the cache
  i                       ## and returns it
}
