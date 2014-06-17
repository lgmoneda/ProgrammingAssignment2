## makeCacheMatrix() will create a special type of matrix, which is able to
## store its inverse and prevent unnecessary calculations by calling
## cacheSolve function, which returns the inverse matrix calculated previously
## or calculate and stores it in this special type of matrix.

## Take a matrix and transforms it on the Cache Matrix, a new type of matrix
## with some functions to set, get itself and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        
        get <- function() x
        setInv <- function(solve) inv <<- solve(x)
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve will try to get a not null inverse stored in the CacheMatrix.
## if there's a inverse stored, it will be returned. Otherwise, the function
## will calculate the matrix inverse, store and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
