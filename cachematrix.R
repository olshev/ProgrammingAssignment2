## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

## Spetial object that has 4 attributes
## * get\set matrix
## * getinverse\setinverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    changed <- TRUE
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        changed <<- TRUE
    }
    get <- function() x
    setinverse <- function(inverse) {
        inv <<- inverse
        changed <<- FALSE
    }
    getinverse <- function() inv
    getchanged <- function() changed
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse,
         is.changed = getchanged)
}


## Function returns invert matrix of object x
## created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## If object changed, then solve again
    if (x$is.changed()==TRUE) {
        inverse <- NULL
    } else {
        inverse <- x$getinverse()   
    }
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse    
}
