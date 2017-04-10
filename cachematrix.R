## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_mtrx <- function(y) {
    x <<- y
    inv <<- NULL }
  
  get_mtrx <- function() x
  set_inv <- function(inv_mtrx) inv <- inv_mtrx
  get_inv <- function() inv 
  
  list(setMatrix = set_mtrx, getMatrix = get_mtrx, setInverse = set_inv, getInverse = get_inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)) return (inv)
  data <- x$get_mtrx()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
  
}
