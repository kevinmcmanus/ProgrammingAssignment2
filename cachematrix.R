
## makeCacheMatrix returns a list of functions to get and set a matrix and its inverse
## the matrix and its inverse are stored locally in the environment of the function.

makeCacheMatrix <- function(x = matrix()) {
        
        ## inv holds the cached inverse matrix
        
        ## Null out the cached inverse value upon invocation
        inv <- NULL
        
        ## create the get/set function definitions
        set    <- function(y) {
                  x <<- y
                 inv <<- NULL
                 }
        
        get    <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        ## build the list of functions and return it
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of the matrix contained in arg x
## it returns the cached inverse from the function environment if it exists
## otherwise cacheSolve computes the inverse, stuffs it in function environment and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## return the cached inverse if it exists
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## compute the inverse and save it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        ## serve it up
        inv

}
