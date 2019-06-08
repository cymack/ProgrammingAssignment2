## R Programming Assignment 2
## Student: Charlotte Y. Mack

## The functions in this script create an object that caches a matrix and the
## value of its inverse, if this is known, and a function that returns either
## the cached inverse if it is not NULL, or the newly calculated inverse, which
## then becomes the cached inverse.

## makeCacheMatrix makes a list that 
#       sets the value of the matrix to be inverted;
#       gets the value of this matrix from the parent environment;
#       sets the value of the inverse (initially NULL);
#       gets the value of the inverse.
#
# Once the matrix object has been created, its value can be reset by reassigning
# its $set item, e.g.:
#       testMatrix <- makeCacheMatrix()
#       testMatrix$set <- matrix(c(1, 1, 3, 1), nrow = 2)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


# cacheSolve takes a structure that was creaeted by makeMatrix and returns the
# inverse of the matrix contained in it. If the inverse has already been
# calculated and is in the environment, cacheSolve will return the cached value
# from getinverse. Otherwise, cacheSolve calculates the value with solve() and
# returns it.

# cacheSolve appears to work for square matrices of arbitrary dimension:
#       testMatrix$set(matrix(c(1, 2, 1, 0, 1, 0, 1, 2, -1), nrow = 3))
#       cacheSolve(testMatrix)

# If the matrix is not invertible then cacheSolve passes along an error from
# solve().
#       testMatrix$set(matrix(c(1, 0, 3, 0), nrow = 2))
#       cacheSolve(testMatrix)

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
