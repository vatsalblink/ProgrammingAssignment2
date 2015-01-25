## Put comments here that give an overall description of what functions do
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  #Setting Matrix
  set <- function(yy) {
    x <<- yy
    inverse = NULL
  }
  get <- function() x
  setinv <- function(inverse) inverse <<- inverse
  getinv <- function() inverse
  
  # Return matrix with new function
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  
  if (!is.null(inverse)) {
    message("get cached data")
    return(inverse)
  }
  
  inputdata <- x$get()
  inverse <- solve(inputdata, ...)
  
  x$setinv(inverse)
  
  inverse
}
