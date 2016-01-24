## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates and returns a list of 4 functions,
## which set/get the square matrix itself and its inverse
## the value of matrix and its inverse can be accessed with
## lexical scoping

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve gets a list made by makeCacheMatrix function,
## it first looks if inverse matrix is already calculated
## if so, it returns the inverse
## if not, it calculates the inverse, sets the value, and returns

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
