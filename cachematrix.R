## The function 'makeCacheMatrix' is absolutely the same as 'makeVector' from the example.
## The  methods 'setmean' and 'getmean' are renamed to 'setinverse' and 'getinverse' since
## they set and return the inverse of a matrix, respectively.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(invr) m <<- invr
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The fuction 'cacheSolve' is almost the same as 'cachemean' yet it calls the 'solve' function
## instead of 'mean' to inverse the supplied matrix. It also checks whether the matrix is 
## inveritble and returns a message if it is not so. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        if(!(nrow(data)==ncol(data)) || det(data) == 0) {
                message("matirx is either non-square or singular")
                invisible(m)
        }
        else {
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
}
