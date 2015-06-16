# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


# Added a few test cases to test this function

#Test Cases 1
# set.seed(10)
# r = rnorm (16)
# r
# mm = matrix(r,4,4)
# mm
# mc = makeCacheMatrix(mm)
# cacheSolve(mc)
# cacheSolve(mc)

#Test Cases 2
# set.seed(10)
# r = rnorm (16, 2,2)
# r
# mm = matrix(r,4,4)
# mm
# mc = makeCacheMatrix(mm)
# cacheSolve(mc)
# cacheSolve(mc)
