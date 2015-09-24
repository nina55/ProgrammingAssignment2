## These two functions are used to create a special "matrix" object 
## that stores a numeric matrix and caches its inverse.

## The first function, `makeCacheMatrix`, creates a special "matrix" object, 
## which is really a list containing functions to

## 1)  set the value of the square invertible matrix
## 2)  get the value of the square invertible matrix
## 3)  set the value of the inverse of the matrix
## 4)  get the value of the inverse of the matrix

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

## The second function, `cacheSolve`, computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache. That is why
## this function first checks to see if the inverse has already been 
## calculated. If so, it `get`s the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data with 
## the `solve` function and sets the value of the inverse in the cache 
## via the `setsolve` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
