## cachematrix.R
## 17Jun2015 MGK	Created

## makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse
## Calling makeCacheMatrix() creates (in a unique environment) 6 distinct elements, 
## 4 functions (get, set, getinverse and setinverse) and two objects (m and x), where
## x is the original matrix and m is its inverse

## Note the use of the <<- operator which can be used to assign a value to an object in an 
## environment that is different from the current environment
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
		# return a list of functions in the same environment
		list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: returns a matrix that is the inverse of 'x', if the inverse has already been calculated, it returns the 
## cached inverse instead
## Note the object x in this function is the list class returned from makeCacheMatrix()
cacheSolve <- function(x, ...) 
{
        m <- x$getinverse()
        ## inverse already cached?
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## get original matrix, invert and set
		data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
