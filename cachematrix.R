##cacheMatrix.R
##provides a data structure for storing a matrix and its inverse, and an accessor method for the inverse that uses cached values for +
##      efficiency.
##
##
##functions:    makeCacheMatrix - data structure that stores a matrix and its inverse, and provides basic accessors.
##              cacheSolve - takes a cache matrix, and returns the inverse, using cached results or updating the cache as needed.

## Code is EXCESSIVELY COMMENTED so new R programmers can see why everything is the way it is.



## makeCacheMatrix - Stores data for a matrix and possibly its inverse. Returns a list of accessor methods.
## Argument:            matrixToInitialize - a matrix to store
## Constructor:         makeCacheMatrix
## accessor methods:    get/set for the matrix
##                      getInverse/setInverse for the inverse of the given matrix.
##
## No error checking, local validation, or computation of data, it's just a data structure.
makeCacheMatrix <- function(matrixToInitialize = matrix()) {
        
        ##this puts the matrix and inverse data in the scope of this function/object.
        ##i gotta say, overloading function() is a pretty opaque way of dealing with something so fundamental to OOP.
        ##Also, i diverge from the example here to clarify the scope of the argument.
        cachedMatrix <- matrixToInitialize
        cachedInverse <- NULL
        
        ##set initializes the external storage and stores a matrix in it. Inverse is initialized but not computed.
        set <- function(matrixToInitialize) {
                #note that this locally scoped matrixToInitialize takes precedence over the function argument.
                #also '<<-' digs into the (constructor) function's scope, allowing the data to persist.
                cachedMatrix <<- matrixToInitialize
                
                #if you're reinitializing the matrix, the old inverse is NO DANG GOOD NO MO
                cachedInverse <<- NULL
        }
        
        ## get retrieves the primary data stored in the CacheMatrix, the matrix itself
        get <- function() {
                ## return the cached matrix
                cachedMatrix
        }
        
        
        ##similar get/set methods for the inverse, cachedInverse.
        setInverse <- function(inverseToInitialize) {
                cachedInverse <<- inverseToInitialize
        }
        getInverse <- function() cachedInverse
        
        
        ##the return value for calling this function is a list of the methods it contains.
        ##this exposes them to external environments, allowing them to be called externally.
        
        ##the a = a syntax provides names to the methods using the named object capability of R lists. 'handle' = 'pointer to function'.
        ##i'm personally of the opinion that distinct things need distinct names but wtf do i know. I'm following the rails here.
        
        ##returning these accessor methods, that depend on the data fields in the main function scope prevents the whole object +
        ##from being garbage collected as long as this list is not collected.
        list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve - searches for a cached inverse of a cacheMatrix, returns it if availiable, and computes/caches it if not.
## Arguments:           x - a cacheMatrix to get the inverse from
##                      ... - extra arguments get passed on to solve() if the result aint cached already.
## THIS FUNCTION CAN HAVE SIDE EFFECTS: if x hain't got an inverse to start, by golly it will by the end of this function.
## no error checking or even input validation.
cacheSolve <- function(x, ...) {
        
        ## first, we see if we can do this the easy way, without computation.
        if (is.null(x$getInverse()) == FALSE) {
                
                ## extract the inverse so it doesn't prohibit garbage collection of the original (pass by value, not reference)
                temp <- x$getInverse()
                
                #break, returning a
                return(temp)
        }
        ##if execution proceeds to this point, we need to get that inverse.
        ##what follows is implcitly an 'else' to the 'if' above.
        
        
        ##from inside out:
        ## x.get() - gets the matrix x stores
        ## solve() - computes the inverse of the stored matrix, passing along '...'.
        temp <- solve(x$get(),...)
        
        ##update x
        x$setInverse(temp)
        
        ##return the inverse at the end of execution
        temp
        
}
