## This R code includes a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ##set the value of the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
    
    
  }
  
  ##get the value of the matrix
  
  get <- function() x
  
  ##set the inverse of the matrix
  
  setinverse <- function(inverse) inv <<- inverse
  
  ##get the inverse of the matrix
  getinverse <- function() inv
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  
  if(!is.null(inv))
  {
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
  
  
}
