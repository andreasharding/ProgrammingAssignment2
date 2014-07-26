#' Memoization: Assignment to cache computationally expensive results


#' factory for inverse object, based on mean example
#' @param x - a matrix whose inverse is to be calculated
#' @return list object with set/get for both x and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#' returns cached version or calculates value if not cached, then caches it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
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
