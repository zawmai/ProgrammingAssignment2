## This script takes advantage of R lexical and dynamic scoping to cache
## the matrix's inverse if it hasn't already been computed. Otherwise, it will
## call on the cached matrix inverses.

## Usage Example:
##  mat <- matrix(1:4, 2, 2)
##  cacheHelper <- makeCacheMatrix(mat)
##  mat_inv <- cacheSolve(cacheHelper)
##  cacheSolve(cacheHelper)
##  getting cached data...
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

## Creates an list object containing functions for setting/getting matrix and
## setting/getting the inverse matrix for caching purposes.
## This is the helper function.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL #
    set <- function(y) {
        # '<<-' operator assigns variable 'x' and 'inverse' 
        # from its parent environment. In this case, it's the formal argument 
        # 'x' and the local variable 'inverse' 
        # from the first level annonymous function.
        x <<- y 
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, 
         getInverse = getInverse, 
         setInverse = setInverse)
}


## Returns an inverse of matrix. If already been computed, it will return
## the cached inverse matrix. Otherwise, it will compute the inverse, then cache
## it to be recalled for future purposes. Formal argument 'x' should be a list
## object with following functions: set, get, setInverse, getInverse.
## This is the main function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message('getting cached data...')
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
