

## make cache matrix:: this function return the list of four functions that store the cache matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL;
    set <- function(y) {
      x <<- y
      i <<- NULL;
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## calculate inverse of matrix x :: Check if inverse exists in cache, return inverse from cache otherwise calculate and return.

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

x<-matrix(1:4, nrow = 2, ncol = 2)
x
x_inv <- makeCacheMatrix(x)
cacheSolve(x_inv)





