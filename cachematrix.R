## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function starts by assigning NULL to m
## set is a function that takes in an argument y, assigns the value of y to x and assigns NULL to m
## The values x and m are global varibles thanks to the <<- operator
## get is a function that returns x, which is undefined in get() so it finds the value in the parent environment
## setinverse is a function that assigns a value to m, using <<- so it appears in the parent environment for later use
## getinverse is a function that returns m, which we have a value for thanks to <<- in the setinverse function
## list returns a list of all the functions so we can use $ to call them

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function () {
                x
        }
        setinverse <- function(inverse) {
                m <<- inverse
        }
        getinverse <- function () {
                m
        }
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## We use x$getinverse() to see if we have an inverse for the value x in the cache
## If this does not return null, we print a message and return the inverse that was in the cache
## If there was no inverse in the cache, we use the solve function to find the inverse of the matrix
## Then we store the inverse using the setinverse function
## Finally, the code returns the solved inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
