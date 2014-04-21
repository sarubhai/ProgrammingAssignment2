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
    setInvMat <- function ( solve ) m <<- solve
    getInvMat <- function( ) m
    list( set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat )   
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function ( x, ... ) {
        
    ## Return a matrix that is the inverse of 'x'
  
    m <- x$getInvMat( )
    if ( !is.null ( m ) ) {
    ## Getting Inverse of a Matrix from Cached Data
        # print ( "Getting Inverse of a Matrix from Cached Data" )
        return ( m )
    }
    data <- x$get( )
    ## Calculating Inverse of a Matrix for first time
    m <- solve ( data, ... )
    x$setInvMat( m )
    # print ( "Calculating Inverse of a Matrix for first time" )
    return( m )
}

############# Sample Execution Data Set ##################

## source('C:/Code/ProgrammingAssignment2/cachematrix.R')
## mat1 <- matrix ( c ( 1, 0, 0, 1 ), 2, 2 )
## x <- makeCacheMatrix ( mat1 )
## cacheSolve ( x )

## mat2 <- matrix ( c ( 4, 3, 3, 2 ), 2, 2 )
## y <- makeCacheMatrix ( mat2 )
## cacheSolve ( y )

## mat3 <- matrix ( c ( 1, 2, 3, 4 ), 2, 2 )
## z <- makeCacheMatrix ( mat3 )
## cacheSolve ( z )

## cacheSolve ( y )
## cacheSolve ( x )
## cacheSolve ( z )

##########################################################
