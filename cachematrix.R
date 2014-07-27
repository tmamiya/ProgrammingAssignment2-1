## We will use R's Lexical Scoping, 
## ability to reference an object outside of its calling environment and store an inverse matrix and than 
## check whether it has not been calculated or if the matrix itself has changed.
##  and than test Below you will see two parts, the first section is the cacheing of a an inverse matrix

## if you'd like you can use this example and code to test c=rbind(c(1, -1/4), c(-1/4, 1)), this is a matrix
## that actually has an iverse matrix, once you call the above you can pass c in the function
## cacheM <- makeCacheMatrix(c)
## cacheSolve(cacheM)
## cacheM$get()
## cacheM$getsolve()
## cacheSolve(cacheM)


# This function creates a special "matrix" object that can cache its inverse.

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

## This Function will first test whether the inverse of the matrix has been stored, if the invese of matrix
## has not been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {        
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
