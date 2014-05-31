## These functions create a cached matrix object
## that allows user to calculate matrix inverse once and cache it
## rather than recomputing each time.

## This function creates a matrix object
## with the ability to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function on the first call 
## will solve the inverse of a "cached matrix" object
## return the inverse and store that inverse in the cache 
## on the subsequent calls it retreives the cache rather than recomputing
## the cache retrieval is flagged by the print statement

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

# ------------------------------------------------------------
# example
# ------------------------------------------------------------
# x=matrix(1:9,nrow=3)
# x[3,2]=0

# x
#     [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    0    9

# solve(x)
#           [,1]       [,2]        [,3]
# [1,] -1.2500000  1.0000000  0.08333333
# [2,] -0.1666667  0.3333333 -0.16666667
# [3,]  0.4166667 -0.3333333  0.08333333

# xc <- makeCacheMatrix(x)
# cacheSolve(xc)
#           [,1]       [,2]        [,3]
# [1,] -1.2500000  1.0000000  0.08333333
# [2,] -0.1666667  0.3333333 -0.16666667
# [3,]  0.4166667 -0.3333333  0.08333333

# cacheSolve(xc)
# getting cached data <---------------------------------
#            [,1]       [,2]        [,3]
# [1,] -1.2500000  1.0000000  0.08333333
# [2,] -0.1666667  0.3333333 -0.16666667
# [3,]  0.4166667 -0.3333333  0.08333333
