## Collection of functions to support caching of matrix inverses

## Usage:
##   m <- matrix(rnorm(9), 3, 3)
##   M <- makeCacheMatrix(m)
##   Minv <- cacheSolve(M)

## Verification:
##   Minv <- cacheSolve(M)  # should see "getting cached data" message
##   Minv %*% M$get()  # should return 3 x 3 identity matrix (within tolerances)
##   M$get() %*% Minv  # should return 3 x 3 identity matrix (within tolerances)

## Creates a cache object that stores a matrix and its inverse

makeCacheMatrix <- function(my_x = matrix()) {
  my_xinv <- NULL
  set <- function(x) {
    my_x <<- x
    my_xinv <<- NULL
  }
  get <- function() my_x
  setinverse <- function(xinv) my_xinv <<- xinv
  getinverse <- function() my_xinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes a matrix inverse, pulling from cache if previously computed

cacheSolve <- function(x, ...) {
  xinv <- x$getinverse()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinverse(xinv)
  xinv
}
