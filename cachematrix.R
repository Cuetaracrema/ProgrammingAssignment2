## Put comments here that give an overall description of what your
## functions do

## First, we initialize objects (x, m); second we define the functions for objects 
## of makeCacheMatrix() and finally, we create a new object by returning a list()

## Write a short comment describing this function

## we initialize objects (x, m)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

## we define the functions for objects of makeCacheMatrix()
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
## we create a new object by returning a list()
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
    
## cacheSolve() is required to retrieve the inverse from a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##  The function attempts to retrieve a inverse from the matrix passed in as the argument.
  m <- x$getsolve()
  ## if the value here is not equal to NULL, we have a valid cached data and we can return it to the parent environment
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if !is.null(m) is FALSE, we gets the result (inverse of the matrix) from the input object
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
