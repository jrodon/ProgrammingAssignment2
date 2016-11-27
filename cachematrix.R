## The 2 functions defined here create a special object that stores a matrix
## and caches its inverse.
##
## The function makeCacheMatrix initializes the variables containing the values 
## of an input matrix and its (not yet calculated) inverse, as well as 
## 4 functions that "set" and "get" the values of said matrix and its inverse, 
## and returns those functions as a list.
## 
## makeCacheMatrix <- function(x = matrix())  : Initializes 'x' and asigns to 
##                                              it the input matrix 
##
## m <- NULL  : Initializes 'm' as a NULL (empty) variable 
## 
## set <- function(y) : Defines the 'set' function with argument 'y' 
##
## x <<- y  : The value of 'y' is assigned (set) to 'x' in the parent 
##            environment of the 'set' function, i.e., in the environment 
##            of 'makeCacheMatrix' 
##
## m <<- NULL : 'm' is assigned (set) the NULL value, again in the environment 
##              of 'makeCacheMatrix'
## Thus the 'set' function, when called with the argument 'y', will "reset" the 
## values of 'x' and 'm' to 'y' and NULL, respectively, in the environment 
## defined by 'makeCacheMatrix'. That makes the new values of 'x' and 'm' 
## available to the other functions defined in that environment.
## 
## get <- function() x  : 'get' is defined as a function with no arguments, that
##                        returns (gets) the value of 'x'
##                        
## setInv <- function(inv) : Defines the 'setInv' function and initializes 'inv'
##
##  m <<- inv : Assignes (sets) the value of 'inv' to 'm' in the environemnt 
##              defined by 'makeCacheMatrix'
##  Thus like the 'set' function before, 'setInv' will assign the value of 'inv'
##  passed to it to the variable 'm', and make it available in the environment 
##  of 'makeCacheMatrix'.
##  
##  getInv <- function() m  : 'getInv' is defined as a function with no 
##                            arguments, that returns (gets) the value of 'm'
##                            
##  list(set = set, get = get, setInverse = setInv, getInverse = getInv)
##  This is the output of the function 'makeCacheMatrix'. It is a list object 
##  with named elements each containing a function. Those functions are defined 
##  within the environment of 'makeCacheMatrix'.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() 
    x
  setInv <- function(inv) 
    m <<- inv
  getInv <- function() 
    m
  list(set = set, get = get, setInverse = setInv, getInverse = getInv)
}

## The function 'cacheSolve' takes as argument an object of the class 
## 'makeCacheMatrix', defined with its namesake function.
## This function recovers the value of the matrix to be inverted from within the
## environment contained by the 'makeCacheMatrix' object and either calculates
## or retrieves from cache the value of that matrix inverse.
## 
## cacheSolve <- function(x, ...) : The argument 'x' has to be an object of the  
##                                  class 'makeCacheMatrix'.
##  
##  mat <- x$getInverse() : Calls the 'getInv' function defined in the 
##                          environment 'makeCacheMatrix' and assigned to the 
##                          list element called 'getInverse', to assign to 'mat'
##                          the value of the inverse already cached.
##                        
## if(!is.null(mat)) ... return(mat)  : Checks the value of 'mat' retrieved just 
##                                      before. If it is not empty (NULL) then 
##                                      returns its value and exits the function.
##                                  
## data <- x$get()  : Calls the 'get' function defined in the environment
##                    'makeCacheMatrix' and assigned to the list element
##                    called 'get', to assign to 'data' the value of the
##                    matrix given as an argument to the 'makeCacheMatrix'
##                    function.
##                    
## mat <- solve(data, ...)  : Calculates the inverse of the matrix with the 
##                            'solve' function and assigns it to 'mat'.
##                          
## x$setInverse(mat)  : Calls the 'setInv' function defined in the environment
##                      'makeCacheMatrix' and assigned to the list element
##                      called 'setInverse', to assign to 'm' the value of the
##                      inverse matrix just calculated (stored in 'mat').

cacheSolve <- function(x, ...) {
  mat <- x$getInverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setInverse(mat)
  mat
}
