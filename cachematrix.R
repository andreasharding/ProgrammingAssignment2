#' Memoization: Assignment to cache computationally expensive results


#' factory for inverse object, based on mean example
#' @param x - a matrix whose inverse is to be calculated
#' @return list with set/get functions for both x and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## private variables (NB - include x too)
  inv <- NULL
  
  ## setters/getters (NB - set also clears inv as there is presumably a new matrix)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## finally wrap all functions in a list and return it
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


#' returns cached version or calculates value if not cached, then caches it
#' @param x - a matrix whose inverse is to be calculated
#' @param ... any remaining optional parameters (passed to solve)
#' @return inv - a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## first try to get a cached inverse
    inv <- x$getinverse()
    
    ## if this succeeds, return it and print message before returning from function
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## if you get this far, there's no cached version, so get the matrix via getter call
    data <- x$get()
    
    ## do the hard work here
    inv <- solve(data, ...)
    
    ## then store result for future use
    x$setinverse(inv)
    
    ## and finally return the actual value calculated
    inv
}

## test cases

## make some simple matrices for testing
a <- matrix(c(2,4,6,8), nrow = 2, ncol = 2, byrow = TRUE)
b <- matrix(c(2,3,5,7), nrow = 2, ncol = 2, byrow = FALSE)
c <- matrix(c(1,1,2,4), nrow = 2, ncol = 2, byrow = FALSE)

## solve these for reference of good output
solve(a)
solve(b)
solve(c)

## now make the function objects/lists
aa <- makeCacheMatrix(a)
bb <- makeCacheMatrix(b)
cc <- makeCacheMatrix(c)

## then perform memoized solve...
## first time has to calculate inverse
cacheSolve(aa)
cacheSolve(bb)
cacheSolve(cc)

## second time around can retrieve values from cache (and prints message)
cacheSolve(aa)
cacheSolve(bb)
cacheSolve(cc)
