## Use <<- to expose internal environment


## define foure functions and return them as a list

makeCacheMatrix <- function(x = matrix()) {
  
    inverse <- NULL
    # clear the mean and assign the new matrix
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x  # return the input matrix x
    setinverse <- function(inv) inverse <<- inv  # Set inverse to solve
    getinverse <- function() inverse   # Return Inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  

}


## if matrix has not changed return inverse otherwise solve and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse() # get the inversed matrix 
  
  if(!is.null(inverse)) { # if in memory
    message("this is cached data")
    return(inverse) # return 
  }
  data <- x$get() # if not,  x$get to get the matrix
  inverse <- solve(data) # solve
  x$setinverse(inverse) #  set the inverse
  inverse # return
}
