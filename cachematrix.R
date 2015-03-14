## The functions below compute matrix inversion by cashing the result in memory
## rather than computing it repeatedly to avoid costly computation where possible

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #define i in the current environment
  
  set <- function(y) { #set the matrix
    x <<- y
    i <<- NULL #define i in the parent environment
  }
  get <- function() x #return the set matrix
  setinverse <- function(solve) i <<- solve #apply solve and return to i in parent
  getinverse <- function() i #return inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
##(and the matrix has not changed), then the cacheSolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
