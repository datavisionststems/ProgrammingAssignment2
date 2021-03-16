## Put comments here that give an overall description of what your
## functions do

# Since with matrices we don't divide, there is no concept of dividing by a matrix.
# But we can multiply by an inverse, which achieves the same thing.
# The Inverse of a Matrix is the same idea as the reciprocal of a number
# When we multiply a matrix by its inverse we get the Identity Matrix
# Matrix inversion is an expensive computation process and so caching it is of some benefits 
# The pair of functions that are used to stores a matrix and caches its inverse.

# This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# This function computes the inverse of the matrix objecr created by makeCacheMatrix function
# If the inverse has already been calculated and the matrix has not changed),
# then it is possible to retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

# # Test the functions
testM1 <- matrix(rnorm(5, 5, 5), 4, 4)
testM2 <- makeCacheMatrix(testM1)
cacheSolve(testM2)
