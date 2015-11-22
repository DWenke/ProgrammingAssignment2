## R Programming assignment 2. These function demonstrates the value of 
##    caching the inverse of a matrix instead of computing it repeatedly.
##    There are two functions to achieve the objective, see the
##    descriptions of each below. 

## The makeCacheMatrix function creates a matrix object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  matinv <- NULL             
  
  set <- function(y) {       
      x <<- y                 
      matinv <<- NULL                
    }
  get <- function() x   
    
  setmatinv <- function(inverse) matinv <<- inverse
  getmatinv <- function() matinv
  
  #returns a vector with a list of the functions defined above.  
    list(set = set, 
         get = get,
         setmatinv = setmatinv,
         getmatinv = getmatinv) 
  }


## The cacheSolve function checks to see if the inverse has already been
##     calculated and is cached. If not, it computes the inverse of the
##     matrix returned by the above makeCacheMatix function. 

cacheSolve <- function(x, ...) {
  
    matinv <- x$getmatinv()
      if(!isnull(matinv)) {
        message("Getting cached data")
        return(matinv)
      }
      data <- x$get()
      matinv <- solve(data, ...)
      x$setmatinv(matinv)
      matinv
  }
