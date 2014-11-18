## Put comments here that give an overall description of what your
## functions do
#
## Following the vector example from the instructions ...
## we'll first create a function makeCacheMatrix which takes a matrix as
## input (which should be invertable but this is not checked). The funct
## returns a list of 4 more functions ("methods") which can be called by 
## later & by other functions depending on tests of the current state of
##variables. 
##
## The second function cacheSolve takes 1 input (plus potentially other
## options which could be passed to the sub-functions).  The input it
## takes is the "special" matrix created by makeCacheMatrix which contains
## the actually matrix passed into it plus a list of (sub)functions (methods)
## that can be called. The function checks whether the inverse matrix
## has previously been calculated, and if so returns the value from 
## memory.  Otherwise, it uses the makeCacheMatrix$get to grab the 
## matrix; solves for the inverse; writes the calculated inverse to memory
## using makeCacheMatrix$setinv  so that subsequent calls can read inverse
## w/o calculting; and returns the inverse matrix.



## Write a short comment describing this function
## This funct creates a list of 4 more functs: set, get, setinv 
## and getinv.  These functions basic task is simply returning an 
## appropriate value to the apprrop environment.  

makeCacheMatrix <- function(x = matrix()) {
    
    mat_inv <- NULL
    ## set funct allows us to input a new matrix (passed in as y)
    ## also resets mat_inv so we won't read the inverse of an old matrix
    ## from memory, instead will recalc.
    set <- function(y) {
          x <<- y    
          mat_inv <<- NULL
    }
    
    ## get just passes value of inputed matrix back
    get    <- function()   { x }
    
    ## setinv takes the input (which, when called in cacheSolve is the
    ## inverted matrix) and passes it back to mat_inv in the global envir
    setinv <- function(inv){ mat_inv <<- inv }
    
    ## getinv just passes back mat_inv
    getinv <- function()   { mat_inv }

    # Return value of function = a list of 4 functions.
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## this funct will return the inverse a (invertable) square matrix.
## it will do so by first checking whether the value already exists
## and if so pulling it memory. Otherwise it will calc inverse (if 
##possible) and return values that ensure the next time 
## funct is called, it will return value from memory instead of recalc.
##
## The input to cacheSolve is the funct makeCacheMatrix which contains
##the matrix to be inverted as well as a list of sub-functions.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


    ## use makeCacheMatrix$getinv to load mat_inv locally.  Will either
    ## be NULL or prev calc'ed inverse
    mat_inv <- x$getinv()

    ## if mat_inv !NULL just return mat_inv
    if(!is.null(mat_inv)) {
        message("getting cached data")
        return(mat_inv)
    }    

    ##use makeCacheMatrix$get to load inputted matrix
    data <- x$get()
 
    ## calc inverse
    mat_inv <- solve(data, ...)

    ## use makeCacheMatrix$setinv to cache inverse
    x$setinv(mat_inv)
    
    ## return inverse
    mat_inv
    
    
    
}
