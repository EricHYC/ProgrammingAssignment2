## Function makeCacheMatrix creates a ¡°matrix¡±and can:
## set the elements of the matrix; get the elements of the matrix; 
## set the elements of the matrix inverse;
## get the elements of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    tempinv <- NULL
    set <- function(y) {
        x <<- y
        tempinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) tempinv <<- inverse
    getinverse <- function() tempinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the above "matrix" and
## returns it as list

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("good")
        return(inv)
    }
    matrix_to_invert <- x$get()
    inv <- solve(matrix_to_invert, ...)
    x$setinverse(inv)
    inv
}
