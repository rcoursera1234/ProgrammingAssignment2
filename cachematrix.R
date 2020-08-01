## The makeCacheMatrix function is used to create a matrix where the Inverse 
## is saved in Inv if was calculated before
## The cacheSolve function calculates the inverse ones. And returns the already
## calculeted value if posible.

## Creates a matrix where the Inverse is saved in Inv if was calculated before

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a matrix with Inverse on cache functionality
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function () x
  setsolve <- function(solve) Inv <<- solve
  getsolve <- function() Inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function checks if Inv is NULL and if it is, it calcules the inverse
## saves it on the custom matrix object and return it. If it's not NULL returns
## its already calculated value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getsolve()
  if (!is.null(Inv)) {
    message("getting cache data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setsolve(Inv)
  Inv
}
