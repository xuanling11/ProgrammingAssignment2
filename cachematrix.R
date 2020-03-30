## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { ## create set function
    x <<- y
    inv <<- NULL ## innitiate inv variable as NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix 
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data") ## exclude situation if inv is not NULL
    return(inv)
  }
  data <- x$get() ## get x data
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv      
}
