## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
    # x = original matrix
    # m = inverted matrix
    m  <- NULL
    set  <- function(y)
    {
        x <<- y
        m <<- NULL 
    }
    
    # return matrix
    get  <- function() x
    
    # override previous inverse and assign argument to inverse
    setinverse  <- function(inverse) m  <<- inverse
    
    #return inverse
    getinverse  <- function() m
    
    # create list of function
    list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) 
{
    # get inverse
    m  <- x$getinverse()
    
    # if inverse exists, return cached inverse
    if (!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
    
    #get matrix
    data  <- x$get()
    
    # inverse computation
    m  <- solve(data, ...)
    
    # caching inverse
    x$setinverse(m)
    
    #return inverse
    m
}
