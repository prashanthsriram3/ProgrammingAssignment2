## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL   # initialize variable which contains inv: i
        set <- function(y) {
                x <<- y      # changing the matrix
                i <<- NULL   # Reset i to NULL
        }
        get <- function() x   # get matrix x
        setinv <- function(inv) i <<- inv   # set the inverse of x
        getinv <- function() i   # get the cached inverse or NULL depending on value of i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)   # return list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()      # get the current inverse
        if(!is.null(i)) {    # check is inverse is already cached
            message("getting cached data")   
            return(i)        # return cached inverse
        }
        data <- x$get()      # get x if inverse because cached
        i <- solve(data, ...)   # compute inverse
        x$setinv(i)      # set the inverse
        i                # return inverse
}
