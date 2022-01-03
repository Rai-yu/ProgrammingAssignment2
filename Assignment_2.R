library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- mean
  getinv <- function() m
  {
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}