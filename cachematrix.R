## These functions cache the inverse of a matrix
## to prevent compute it repeatedly.


## mackCacheMatrix creates a special matrix object
## that cache its inverse
## set: set the matrix; get: get the matrix
## setinverse: set inverse matrix
## getinverse: get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y){
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inver <<- solve
    getinverse <- function() inver
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the matrix
## returned by makCacheMatrix. If the inverse has already
## been calculated, this function will retrieve the
## inverse matrix from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
    if(!is.null(inver)){
        message("getting cached data")
        return(inver)
    }
    inputmatrix <- x$get()
    inver <- solve(inputmatrix)
    x$setinverse(inver)
    inver
}
