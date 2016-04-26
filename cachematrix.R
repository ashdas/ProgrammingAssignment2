## makeCacheMatrix() takes an matrix vector as an input
## It returns a list - with:
##              get --> To get the vector
##              set --> To set the vector
##              setInverse --> returns the inverse of the numeric vector
##              getInverse --> will be used by cacheSolve() to return the inverse
##  

makeCacheMatrix <- function(x = matrix()) {
        inv1 <- NULL
        set <- function(y) {
                x <<- y
                inv1 <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv1 <<- solve
        getInverse <- function() inv1
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve() checks if the mtrix inverse has already been calculated
## If Yes, call makeCacheMatrix$getInverse to get the inverse.
## If No, calculate the inverse. the
## Function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv1 <- x$getInverse()
        if(!is.null(inv1)) {
                message("getting cached inverse of the matrix")
                return(inv1)
        }
        data <- x$get()
        inv1 <- solve(data, ...)
        x$setInverse(inv1)
        inv1
}
