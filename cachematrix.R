## Create function to set and get the inverse of a matrix 

## The makeCacheMatrix cache the inverse of a specified matrix x which I have called inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y){
        x <<- y
        inv <<- NULL
        }
   get <- function() x
   setinv <- function(inv_matrix)  inv <<- inv_matrix
   getinv <- function() inv
   list(set = set, get = get, setinv = setinv, getinv = getinv)
      
}


## The cacheSolve function checks if the inverse matrix is cached and return it. 
##Otherwise calculate the inverse of the matrix and return the value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' in the cache
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## If it is not in the cache calculate the inverse
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
