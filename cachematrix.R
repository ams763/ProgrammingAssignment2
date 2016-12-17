## The functions makeCacheMatrix and cacheSolve work together to manage caching
## of the inverse of a user-defined n-by-n matrix. The matrix and its inverse are
## both stored as local variables of makeCacheMatrix, and they can be accessed or
## modified using functions defined within makeCacheMatrix. The function
## cacheSolve uses the "getinverse" and "setinverse" subfunctions of makeCacheMatrix 
## to check whether the inverse of the stored matrix has already been cached
## within makeCacheMatrix, and to cache it if it has not. This allows the user to
## avoid directly computing the matrix inverse if its value is already known,
## decreasing runtime in applications.

## First, makeCacheMatrix creates an object containing variables for the matrix
## and its inverse, and functions to get or to set each of these. It returns a
## list containing these four functions. Since the listed functions are created 
## inside makeCacheMatrix, they will have access to its local variables even when
## they are called in the global environment or in another function. This is due
## to R's lexical scoping rules.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(y) {
                inv <<- y
        }
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Now, cachesolve checks if the "inv" variable of makeCacheMatrix has already
## been set. If so, it returns the stored value. If not, it directly computes the
## inverse of the matrix variable stored in makeCacheMatrix, and stores the result
## in the "inv" variable of makeCacheMatrix. All of these operations use the
## subfunctions defined in makeCacheMatrix.

## Note that the argument x in cacheSolve must be the output of makeCacheMatrix
## i.e. a list of functions, whereas makeCacheMatrix uses x for a user-defined
## matrix. Also note that "inv" here is a local variable of cacheSolve, not
## the local variable "inv" of makeCacheMatrix. The whole point of the function
## is to make these values the same.


cacheSolve <- function(x) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Retrieving cached data...")
                return(inv)
        } else {
                inv <- solve(x$get())
                x$setinverse(inv)
                inv
        }
}
