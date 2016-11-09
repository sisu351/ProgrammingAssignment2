##new created special matrix that cache the inverse 
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL  
     set <- function(y){  
               x <<- y    
              inv <<- NULL
             }
        get <- function() x  
        setInverse <- function(solveMatrix) inv <<- solveMatrix  
        getInverse <- function() inv  
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }
##new computes the inverse of special matrix returned by makeCacheMatrix
 cacheSolve <- function(x, ...) { 
          inv <- x$getInverse()
          if(!is.null(inv)){
                   message("getting cached data")    
                  return(inv)
                  }
       data <- x$get()
         inv <- solve(data,...)
         x$setInverse(inv)
      }
