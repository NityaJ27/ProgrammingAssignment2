makeCacheMatrix <- function(x=matrix()){   ## A function is created to make the inverse of a given matrix
   inv <- NULL
   set <- function(y) {
     x <<- y
     inv <<- NULL
     }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function()  {inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}

cacheSolve <- function(x,...) {                   ## A Function is created to return the inverse of the matrix
  inv <- x$getInverse()
  if(!is.null(inv))  {                            ## Checks to see if the matrix was already invered before, and that the answer is cached or not
    message ("Obtaining Cached Data")             ## If yes, then the cached data is displayed, and the inverse is not computed again
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
