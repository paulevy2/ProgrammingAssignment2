## makeCacheMatrix 
## creates an object containing four functions (methods)
##     set
##         allows the matrix value stored in the object to be changed
##     get
##         returns the input matrix
##     setinverse
##         this caches the calculated inverse
##     getinverse
##         this returns the inverse if it has already been cached by setinverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve 
## returns the inverse of the matrix stored in an object created 
## by the makeCacheMatrix function

##     first it checks if the inverse has already been calculated
##     if m is not null then this matrix has already been inverted and m is the solution
##     if m is null, the inverse is calculated and cached to m with setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
