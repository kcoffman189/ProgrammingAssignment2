## These functions calculate the inverse of a matrix and saves it
## to the cache so that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.


makeCacheMatrix <- function(x = matrix()) {  
  ## create a matrix object x and some associated sub-functions/methods
  
  ## define the cache m
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in the
    ## parent environment
    m <<- NULL ## re-initialize m in the parent environment to null
  }
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal
  ## to the inverse of the matrix x
  getinverse <- function() m ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates the inverse of the matrix that was created with the makeCacheMatrix
## function. It does so by checking to see if the inverse has already been caclulated, and if it 
## has, it retrieves the inverse from the cache and skips the computation. 
## Otherwise, it calculates the matrix inverse and uses the setinverse function to set the value 
## of the inverse in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
