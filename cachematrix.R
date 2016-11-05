## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function creates a list containing functions to
##    1. set the values of the matrix
##    2. get the values of the matrix
##    3. set the values of the inverse of matrix
##    4. get the values of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## function to set the values of the matrix
  setMatrix <- function(y){
    x <<- y
    m <<- NULL
  }
  ## function to get the values of the matrix
  getMatrix <- function() x
  ## function to set the values of inverse matrix
  setInverse <- function(solve) m <<- solve
  ## function to get the values of inverse matrix
  getInverse <- function() m
  
  ## creating a list of functions to be returned
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## cacheSolve function calculates the inverse of the list created in makeCacheMatrix function
## It first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache, and skips execution
## Otherwise, the inverse is calculated and set to the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## checking whether the inverse exists in the cache or not
  m <- x$getInverse()
  ## if inverse exists
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## get the matrix for which inverse has to be calculated 
  mtx <- x$getMatrix()
  ## finding the inverse of the matrix and setting it to cache
  m <- solve(mtx)
  x$setInverse(m)
  ##printing the result
  m
}