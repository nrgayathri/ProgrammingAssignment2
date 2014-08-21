## The function caches the inverse of a matrix given as parameter to the function

## Set is used to set the matrix variable and get is used to get the value of the 
## matrix. getinverse is used to get the inverse of the matrix and setinverse
## calls the sovle function to set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(minverse) im <<- minverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Checks if inverse of a matrix is already available if yes it is fetched 
## from the cache, else the setinverse function is called to inverse the 
##matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- makeCacheMatrix(x)
  inversemat <- mat$getinverse()
  if(!is.null(inversemat)) {
    message("getting cached data")
    return(inversemat)
  }
  data <- mat$get()
  inversemat <- solve(data, ...)
  mat$setinverse(inversemat)
  inversemat
}
