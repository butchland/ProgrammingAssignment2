
## constructs a special kind of matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## returns a matrix that caches its inverse
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix) invmatrix <<- matrix
        getinverse <- function() invmatrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## returns a stored value of the inverse matrix if already computed
## otherwise just calls solve() to compute inverse matrix and cache it
## before returning the inverse matrix

cacheSolve <- function(x, ...) {
        ## returns a matrix that is the inverse of x
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)) {
                return(invmatrix)
        }
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setinverse(invmatrix)
        invmatrix
}
