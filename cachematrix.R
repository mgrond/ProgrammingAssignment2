## M. Grond (01-03-2017)

## A pair of functions, makeCacheMatrix() and cacheSolve(), cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

# makeCacheMatrix creates a special "vector", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {     # x is initialized as a function argument, so no further initialization is required within the function
        inv <- NULL                             # inv is set to NULL, initializing it as an object within the makeCacheMatrix() environment to be used by later code in the function
        set <- function(y) {
                x <<- y                         # Assign the input argument to the x object in the parent environment, 
                inv <<- NULL                    # Assign the value of NULL to the inv object in the parent environment. This line of 
                                                # code clears any value of inv that had been cached by a prior execution of cacheSolve().
        }
        get <- function() x                     # The symbol x is not defined within get(), but R retrieves it from the parent environment of makeCacheMatrix() due to lexical scoping
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,                         # gives the name 'set' to the set() function defined above
             get = get,                         # gives the name 'get' to the get() function defined above
             setinverse = setinverse,           # gives the name 'setinverse' to the setinverse() function defined above
             getinverse = getinverse)           # gives the name 'getinverse' to the getinverse() function defined above
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

#  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#  Is is assumed that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {                # starts with a single argument, x, and an ellipsis that allows the caller to pass additional arguments into the function.
                                                
        # Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()                   # attempts to retrieve a inverse from the object passed in as the argument
        if(!is.null(inv)) {                     # checks to see whether the result is NULL
                message("getting cached data")  
                return(inv)                     # if not equal to NULL, there is a valid cached inverse and will be returned to the parent environment
        } 
        data <- x$get()                         # gets the matrix from the input object
        inv <- solve(data, ...)                 # calculates the inverse of the matrix with the solve() function
        x$setinverse(inv)                       # uses the setinverse() function on the input object to set the inverse in the input object
        inv                                     # returns the value of the inverse to the parent environment by printing the mean object.
}
