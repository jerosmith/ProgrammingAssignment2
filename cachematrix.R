
## The following two functions work together to speed up the process of computing the inverse
## of a matrix.

## The makeCacheMatrix function creates a "matrix" object (actually a list of four funcions) 
## that can both cache and retrieve matrices in the environment of the function body (child of the global environment).
## The functions are the following:
## 1) set(x) - Stores matrix 'x' in the matrix cache.
## 2) get() - Retrieves matrix 'x' from the matrix cache.
## 3) setinverse(i) - Stores the inverse matrix of 'x' (matrix 'i') in the inverse matrix cache.
## 4) getinverse() - Retrieves the stored inverse matrix 'i' from the inverse matrix cache.

makeCacheMatrix <- function(x = matrix()) {
        ## Returns a list of four functions, to cache and retrieve a matrix and its inverse.
        i <- NULL  ## Initializes the inverse matrix cache for a new "matrix" object (list).
        set <- function(y) {  ## Stores the matrix 'y' (argument) in the matrix cache 'x'.  (Called by the user or another function).
                x <<- y  
                i <<- NULL  ## Initializes the inverse matrix cache for the new matrix 'y'.
        }
        get <- function() x  ## Retrieves matrix 'x' from the matrix cache. (Called by the user or another function).
        setinverse <- function(inverse) i <<- inverse  ## Stores the computed inverse matrix 'inverse' in the inverse matrix cache. (Called by the cacheSolve function).
        getinverse <- function() i  ## Retrieves the stored inverse matrix 'i' from the inverse matrix cache. (Called by the cacheSolve function).
        list(set = set, get = get,  ## Returns the list of four functions.
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function returns the inverse of the matrix stored in "matrix" object 'z'.
## If the inverse matrix cache is not empty, it returns it; otherwise it computes the inverse matrix,
## stores it in the inverse matrix cache for next time, and returns it.

cacheSolve <- function(z, ...) {
        ## Returns a matrix that is the inverse of the matrix stored in the cache of "matrix" object 'z'.
        i <- z$getinverse()  ## Retrieves the matrix stored in the cache.
        if(!is.null(i)) {  ## If the cache is not empty, returns the cache.
                message("getting cached data")
                return(i)
        }
        data <- z$get()  ## Otherwise retrieves the matrix in the store,
        i <- solve(data, ...)  ## computes the inverse of its matrix,
        z$setinverse(i)  ## and stores it in the cache for next time.
        i  ## Finally, returns the inverse, regardless of whether it came from the cache or was computed.
}

