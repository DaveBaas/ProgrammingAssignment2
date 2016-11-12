## This is programming assignment 2 of the coursera class R 
## programming, student Dave Baas
## Two functions are created to calculate a matrix' inverse and 
## cache the inverse. 

## The function makeCachematrix creates a special matrix object, 
## in the form of a list, which can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                       ## set m to empty variable
        set <- function(y) {                            ## make the function set
                x <<- y                                 ## set y to x in other environment
                m <<- NULL
        }
        get <- function() x                             ## make function to get x 
        setinv <- function(inv) m <<- inv               ## set inv to m and store in other environment
        getinv <- function() m                          ## get m 
        list(set = set, get = get,                      ## return list with set,get,setinv,getinv
             setinv = setinv,
             getinv = getinv)
}


## The function cachesolve checks if the inverse availabe in 
## cache, if so it return the inverse from cache. If not, it 
## calculates and returns inverse and stores in cache. 

cacheSolve <- function(x, ...) {
        m <- x$getinv()                                 ## get cached data
        if(!is.null(m)) {                               ## continue if m has a value
                message("getting the cached data")      ## return message
                return (m)                              ## return inverse from cache
        }
        data <- x$get()                                 ## if not, store matrix in data
        m <- solve(data,...)                            ## calculate inverse
        x$setinv(m)                                     ## store inverse in cache
        m                                               ## return inverse m
}
