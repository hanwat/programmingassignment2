## makeCacheMatrix creates a special "matrix", 
## which is really a list containing functions (closures) to
## set & get the value the original matrix, and
## set & get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
        z <- NULL

        setFn <- function(y) {
                x <<- y
                 ## Reset so we don't hold onto stale inverses when changing the original matrix. 
                z <<- NULL
        }

        getFn <- function() { 
                x 
        }

        setinverseFn <- function(inverse) {
                z <<- inverse
        }

        getinverseFn <- function() { 
                z 
        }

        list(set = setFn, get = getFn,
             setinverse = setinverseFn,
             getinverse = getinverseFn)
}

## This function checks to see if the inverse of x exists (assigned to z); 
## if z is not NULL it will return the cached data, otherwise it will calculate it and store the
## result to be retrieved on the next call for the same matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getinverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }

        ## Actually calculate the inverse if we don't have it saved and store result.
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}
