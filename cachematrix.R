## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_mtrx <- function(y) { #set matrix value 
    x <<- y
    inv <<- NULL }
  
  get_mtrx <- function() x #get matrix value
  set_inv <- function(inv_mtrx) inv <- inv_mtrx #set inverse matrix
  get_inv <- function() inv  #get inverse matrix
  
  list(setMatrix = set_mtrx, getMatrix = get_mtrx, setInverse = set_inv, getInverse = get_inv) #return a list 

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv() #get the inverse from makeCacheMatrix fumction 
  if(!is.null(inv)) return (inv)
  data <- x$get_mtrx()  #get the the matrix from makeCacheMatrix fumction 
  inv <- solve(data, ...)
  x$set_inv(inv) #get the inverse from makeCacheMatrix fumction 
  inv
  
}
