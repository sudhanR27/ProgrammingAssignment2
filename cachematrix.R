## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##inv caches the inverse
##x is the matrix
## Functions are 1.set 2.get 3.setInv 4.getInv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv 
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)

}


## Write a short comment describing this function
##m is the received matrix(in case of computation)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m,...)
  x$setInv(inv)
  inv
}
