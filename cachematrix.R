## below are the functions that cache and compute the inverse of
## a matrix

## This function creates special "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special "matrix"
##returned by makecacheMatrix above. If inverse has been calculated
## already, then cacheSolve Should retrieve the inverse the 
## from the cache

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}
