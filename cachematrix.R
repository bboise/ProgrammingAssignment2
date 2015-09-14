
## Create a special object that stores a matrix and allows you to cache the inverse of the matrix

## Create a matrix that's really a list of a function to 
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL             ## Initialize the inverse of the matrix to be NULL
    set <- function(y) {  ## overwrite the values of X using values in set command
        x <<- y           ## make matrix X using values supplied by Y (give X new values)    
        m <<- NULL        ## set the inverse to NULL, because the matrix has changed
    }
    get <- function() x   ## return the matrix
    setinverse <- function(solve) m <<- solve   ## assign the inverse matrix to be the input supplied
    getinverse <- function() m                  ## return the inverse matrix supplied by setinverse()
    list(set = set, get = get,                  ## generate a list of the properties of the function makeCacheMatrix
         setinverse = setinverse,
         getinverse = getinverse)

}


## if the inverse of the matrix has not be set or calculated,
## return the result of solve(x) which is the inverse of matrix X
## if the property has been set or the inverse have been previously calculated), return the cached value
cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()  ## Check to see if inverse of x is in cacheMatrix
    if(!is.null(m)) {    ##  if it set in cache, 
        message("getting cached data")  ## print message
        return(m)        ## return cached inverse matrix
    }
    data <- x$get()      ## get cached matrix
    m <- solve(data, ...)  ## calculate the inverse, because it's not in cache
    x$setinverse(m)      ## set the calculated inverse to the cached matrix
    m                    ## return the inverse
}
