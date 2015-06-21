## makeCacheMatrix caches a matrix
## cacheSolve calculates inverse of matrix if not already calculated

## caches a matrix that can be called by the get,set,getInverse, setInverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  
  list(set=set, get=get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## calculate if not already the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  if (det(data) == 0){
    message("Matrix is singular. Determinant cannot be divided by 0.")
    return(1)
  }
  i <- solve(data,...)
  x$setInverse(i)
  i
}
