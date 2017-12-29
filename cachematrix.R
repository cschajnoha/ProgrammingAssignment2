## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
## the below function makeCacheMatrix will calculate the inverse of a matrix 
## the result of the calculation is stored in cache

##to test code use below:
##source cacematrix.R
##testdata <- makeCacheMatrix()
##testdata$set(matrix(1:4,2,2))
##cacheSolve(testdata)

makeCacheMatrix <- function(x = matrix()) {
        #m is the cache, default set to NULL
        m <- NULL
        #generate the matrix from input x as object y
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve)m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## this function will check cache for the calculated result of makeCacheMatrix function
## that reports the inverse of a square matrix


cacheSolve <- function(x = matrix()) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## tests if values are in cache. If they are, returns the inverted matrix.
        if (!is.null(m)) {
                message("data is in cache. retrieving...")
                message("data retrieved, inverted matrix values below:")
                ## after messages, return what was read from cache
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        return(m)
}
