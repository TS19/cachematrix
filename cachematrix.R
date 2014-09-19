#Matrix Inversion is usually a costly computation and there may be some benefit to
#catching the inverse of the matrix rather than compute it repeatedly. The
#following two functions are used to cache the inverse of the matrix.

#makeCacheMatrix creates a list containing a function to 
#1. set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {         ## This function creates a special matrix object that can cache its reverse.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks that
#matrix has been properly computed.Then it gets the result and skips the computation.
#If not, it computes the inverse,sets the value in the cache via the set inverse function.



# This function assumes that the matrix is always invertible.


cacheSolve <- function(x,...) {                 ## This function computes the inverse of the special matrix returned by makeCachematrix above.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}





