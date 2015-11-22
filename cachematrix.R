## Programming assignment 2
## set of functions to create and get matrix with cached inverse

## stores the matrix and cache data
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

# Initialization
#mx<-makeCacheMatrix(matrix(rnorm(9),3,3))

#caclulates inverse on the matrix or returns previously cached if set
cacheSolve <- function(x, ...) {
    ii <- x$getinverse()
    if(!is.null(ii)) {
        message("getting cached data")
        return(ii)
    }
    data <- x$get()
    ii <- solve(data, ...)
    x$setinverse(ii)
    ii
}

#cacheSolve(mx)

