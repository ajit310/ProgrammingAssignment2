## Accept an invertible square matrix, perform inverse of the
## matrix and the result is stored. In case the matix is unchanged the
## the stored result is displayed. Hence, avoid re-calculation. In case
## a new matrix is provided the whole process starts from the beginning.

## This function accepts an invertible square matix and it creates four 
## objects for initialising the value of inverse function and recalling
## the underlying functions.

makeCacheMatrix <- function(x = matrix()) {
        inv  <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This funciton first recalls the inverse matrix and if the matrix is 
## not null, displays the stored data. Else accepts the input matix and 
## calculates / displays the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
