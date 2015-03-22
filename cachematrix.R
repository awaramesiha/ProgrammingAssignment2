## Two functions are constructed to find inverse of a matrix and
## to further cache it

## The following function returns a list of functions to set and
## get a matrix and mean

makeCacheMatrix <- function(x = matrix()) {
  b<-NULL
  set = function(z){
    x <<- z
    b <<- NULL
  }
  get = function() x
  setinverse = function(inv) b <<- inv
  getinverse = function() b
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function caches the inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  b=x$getinverse()
  if(!is.null(b)) {
    message("getting cached data")
    return(b)
  }
  data <- x$get()
  b <- solve(data)
  x$setinverse(b)
  b
}
