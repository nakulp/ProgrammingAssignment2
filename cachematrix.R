#-------------------------------------------------------------------------------#
#    Code below consists of two functions namely : -
# 1. makeCacheMatrix - This function creates a special "matrix" object 
#    that can cache its inverse 
# 2. cachesolve - This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been 
#    calculated (and the matrix has not changed), then the cachesolve should 
#    retrieve the inverse from the cache.
#-------------------------------------------------------------------------------#

makeCacheMatrix <- function(x=matrix(0,0,0)){
        inv <- NULL             # initializing the inverse as NULL
        set <- function(y){     # Set functions creates a new matrix and overwrites old value of inverse to NULL
                x <<- y         # value of x will be retained in parent environment as well
                inv <<- NULL
                
        }
        get <- function() x     # Returns the matrix defined by function set
        setinverse <- function(inverse) inv <<- inverse # Saves the value of inverse to a matrix named inv
        getinverse <- function() inv                    # Returns the inverse matrix
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) #Creates a list of all the matrices
        
}

cachesolve <- function(x,...) {
        inv <- x$getinverse()   #getinverse function will return the saved inverse matrix
        if(!is.null(inv)) {     #if the inverse value is not equla to NULL it will recall the old value and will not recalculate
                message("getting cached data")
                return(inv)
        }
        data <- x$get()         #if the value of inverse is NULL then it will calculate the inverse of the matrix
        inv <- solve(data)
        x$setinverse(inv)       #this sets the new value of inverse which can be recalled
        inv
}


