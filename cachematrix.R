## These two functions together allow the calling program to cache a matrix inverse. For example,
## if "x" is a matrix, then one can cache the matrix inverse as follows
##
## y <- makeCacheMatrix (x)
## cacheSolve(y)
##
## Then each subsequent call to cacheSolve will return the matrix inverse without having to recalculate it.
## 


## makeCacheMatrix MUST be called once for each new matrix. In addition to setting the matrix inverse to NULL,
## it provides functions used by CacheSolve for setting / getting a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function () x
  setinv <- function (inv) {
    ix <<- inv
  }
  getinv <- function () ix
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes the list object returned from makeCacheMatrix. It returns the matrix inverse. 
##
## The first time cacheSolve is called, it retrieves the matrix, computes the inverse, and stores the inverse. 
## For subsequent calls, cacheSolve merely retrieves the "cached" inverse.

cacheSolve <- function(x, ...) {
  ## Check to see if inverse has already been calculated
  ix <- x$getinv()

  if (!is.null(ix)) {
    ## Just return cached inverse
    message ("getting cached data")
    return(ix)
  }
  
  ## Inverse not cached. Retrieve the matrix, and compute, store, and retrun the inverse
  data <- x$get()
  ix <- solve(data, ...)
  x$setinv(ix)
  ix
}