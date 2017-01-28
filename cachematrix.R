## The following code consists of two functions that allow to cache the inverse of a matrix
## without costly computation.

#############################
##    INPUTS & OUTPUTS:    ##
#############################

## 1. "makeCacheMatrix" function:
### - takes as input "x" - an invertible matrix
### - returns a list containing function to
####  1. set the matrix
####  2. get the matrix
####  3. set the inverse
####  4. get the inverse
## This list is then used as the input to cacheSolve()

## 2. "cacheSolve" function:
### - takes the list created in makeCacheMatrix as input
### - returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the result and skips the computation.
## Otherwise, it computes the inverse, sets the value in the cache via setinverse function.

######
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set=set, 
    get=get, 
    setinverse=setinverse, 
    getinverse=getinverse)
}

######
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


###############################
##    Example how-to run:    ##
###############################

## > x = rbind(c(2, -1/2), c(-1/2, 2))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  2.00 -0.50
## [2,] -0.50  2.00

## > cacheSolve(m)
##          [,1]     [,2]
## [1,] 0.5333333 0.1333333
## [2,] 0.1333333 0.5333333

## This data is cached, hence when cacheSolve(m) is run another time, it returns:

## getting cached data.
##          [,1]     [,2]
## [1,] 0.5333333 0.1333333
## [2,] 0.1333333 0.5333333


## Now if the code is run again with the results given before it will returns the matrix set
## when called x = rbind(c(2, -1/2), c(-1/2, 2)):

## m  = makeCacheMatrix(x)
## > m$get()
##          [,1]     [,2]
## [1,] 0.5333333 0.1333333
## [2,] 0.1333333 0.5333333

## >  cacheSolve(m)
##       [,1]  [,2]
## [1,]  2.00 -0.50
## [2,] -0.50  2.00  = the call "x = rbind(c(2, -1/2), c(-1/2, 2))" 
