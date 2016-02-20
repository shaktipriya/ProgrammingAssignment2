## Two R functions are written to cache potentially time-consuming computations.
## If the computation is used repeatedly and the contents not changing , the output value
## value is cached so that when we need it again, it can be looked up in cache
## rather than doing all the computations again.
## Scoping rules of the R language is used and how they can be manipulated to 
## preserve state inside of an R object. 
##

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  
  get <- function()x
## calculates the matrix inverse using the solve function.
  setInverse <- function(solve) minverse <<- solve(x)
## gets the inverse
  getInverse <- function() minverse
## passes the value of the function makeCacheMatrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minverse <- x$getInverse()
## Now the function will check whether the matrix contains the inverse value 
##stored in cache.
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  
## If matrix doesnot contain value, the function will get the inverse.
  data <- x$get()
  minverse <- solve(data, ...)
  x$setInverse(minverse)
  minverse
  
}

## example on how to run the functions
## make a matrix.
##mat <- matrix(c(3,-5,8,10,2,-88,9,20,-7),3,3)
##pass the matrix as an argument for the the function makeCacheMatrix
## m1 <- makeCacheMatrix(mat)
## pass the return of the funtion makeCacheMatrix as an argument to the 
## function cacheSolve
## t1 <- cacheSolve(m1)
