##makeCacheMatrix: This function creates a special "matrix" object
##that can cache its inverse.

## Write a short comment describing this function
##
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ivse <- NULL
    set <- function(y) {
        x <<- y
        ivse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) ivse <<- solve
    getinverse <- function() ivse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

}


## Write a short comment describing this function
##
##This function (cacheSolve) computes the inverse of the object returned
##by makeCacheMatrix function. First checking if the inverse has already been calculated.
##If so, we get the result via get function("x$get()") and finish computing. If not,
##it computes the matrix inverse and sets the value with the function("x$setinverse(inv)")
##finally returns the inverse("inv").

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
    ivse <- x$getinverse()
    if(!is.null(ivse)) {
        message("getting cached inverse matrix")
        return(ivse)
    }
    data <- x$get()
    ivse <- solve(data, ...)
    x$setinverse(ivse)
    ivse    
}
