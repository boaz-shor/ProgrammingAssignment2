## write 2 function, the first is to create the special matrix 
## and the second to cache the inverse


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## mean has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }else{
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
}
