## Caching the inverse of a matrix rather than compute it repeatedly. 
## A pair of functions that cache the inverse of a matrix.
## Assumption: The matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function ( x = matrix () ) {
    m <- NULL
    set <- function ( y ) {
        x <<- y
        m <<- NULL
    }
    get <- function ( ) x
    setmean <- function ( mean ) m <<- mean
    getmean <- function( ) m
    list( set = set, get = get, setmean = setmean, getmean = getmean )   
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function ( x, ... ) {
        
    ## Return a matrix that is the inverse of 'x'
  
    m <- x$getmean( )
    if ( !is.null ( m ) ) {
    ## Getting Inverse of a Matrix from Cached Data
        return ( m )
    }
    data <- x$get( )
    ## Calculating Inverse of a Matrix for first time
    m <- mean ( data, ... )
    x$setmean( m )
    m
}
