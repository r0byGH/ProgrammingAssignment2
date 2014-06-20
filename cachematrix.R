## The following functions implement a cached version of the inverse of a matrix
## IMPORTANT: it is supposed that the data provided i.e. the matrix supplied
##  is always invertible. 


## The makeCacheMatrix function creates a special "matrix", 
##  which is really a list containing functions to
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the matrix inverse (setsolve)
##  - get the value of the matrix inverse (getsolve)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
      x <<- y
      s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function calculates the inverse of the special "matrix" created with the 
## makeCacheMatrix function. However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
# via the setsolve function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  
  ## Inverse calculation: the solve() function solves the equation a %*% x = b
  ## we will use it to solve the a %*% x = i equation where i is the identity matrix 
  ## with same dimension as x in order to obtain the inverse
  i <- diag(nrow(data))
  s <- solve(data, i, ...)
  x$setsolve(s)
  s
}
