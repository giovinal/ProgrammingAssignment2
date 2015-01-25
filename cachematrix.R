## Caching time consuming computations : 
## Storing in cache the result of a matrix inversion
## As this operation is very time-consuming on big matrices
## The result will be retrieved instantanly instead of being recomputed each time

## This function creates a special matrix 
## More particurlarly, its creates a set of functions to :
## get and set the value of the matrix
## get and set the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##This function calculates the inverse of a matrix via the function solve
##It checks prior to computation if the value of the inverse has been cached
##If not the computation and storage of the inverse take place
##Otherwise the cached value is rendered
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
