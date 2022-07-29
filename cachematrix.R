
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## initialize
        ini <- NULL
        ## set the matrix
        set <- function(y) {
                x <<- y
                ini <<- NULL
        }
        ## get the matrix
        get <- function() 
        ## return the matrix
            x
        ## set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        ## get the inverse of the matrix
        getInverse <- function() 
        ## return the inverse 
           ini
        ## return a list of the methods
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## compute the inverse of the special matrix returned by "makeCacheMatrix" above. 

cacheSolve <- function(x, ...){
    ## return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## return a matrix if it is the inverse of x matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## get the matrix
    data <- x$get()
    ## solve the inverse using matrix multiplication
    m <- solve(data, ...)
    # set the inverse of inverse matrix
    x$setinverse(m)
    # return the matrix
    m
}
