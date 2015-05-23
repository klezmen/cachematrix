##This is assignment 2 of the coursera course: R-Programming may 2015

##The first function, makeVector creates a special "vector", which is really a list containing a function to:
##	1.	set the value of the vector
##	2.	get the value of the vector
##	3.	set the value of the mean
##	4.	get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  first_Init <- NULL                                         ## Initialize with first_init  
  set <- function(y) {                                    ## Create function "set"
    in_cacheX <<- y                                   ## Input matrix to be put in  cache
    m_cache <<- NULL                                ## Initialize to NULL          
  }
  get <- function() in_cacheX                               ## create function to be called with $set (essence of lexical scoping@@)
  set_cache_m <- function(first_Init) m_cache <<- first_Init    ## Set value of set_cache_m in cache.       
  get_cache_m <- function() m_cache                       
  list(set = set, get = get,             
       set_cache_m = set_cache_m,
       get_cache_m = get_cache_m)
}


## This function will cache the inverse function of r to spare memory allocation and reduce computational load.
##The function will do the following
##	1.	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##	2.	cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##     3.          If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x) {                     ## Function call for input x (the input matrix )
  first_Init<- x$get_cache_m()               ## Put M in cache
  if(!is.null(first_Init)) {                 ## If m is null.......
    message("getting cached data")  ## If m is not NULL/empty, return value of m with a message.
    return(first_Init)
  }                                      
  cleanMatrix <- x$get()               ## Make sure nested function can be called with  $get                        
  matrixInv <- solve(cleanMatrix)   ## Use solve() to invert the input matrix cleanMatrix and put in matrixInv
  x$set_cache_m(matrixInv)             ## Put x in cache
  matrixInv                            
}


## Create a 1000x1000 matrix to test the functions
## Please not that calculating the inverse of a matrix only works on a square matrix.
NCols=1000
NRows=1000

##Test the results with this matrix
## ddtest and dda are the same matrices, but ddtest is of class dataframe
ddtest<-matrix(runif(NCols*NRows), ncol=NCols)
ddtest<-as.data.frame(ddtest)
dda <- makeCacheMatrix()
dda$set(matrix(runif(NCols*NRows), ncol=NCols))
##dda is a 1000x1000 matrix of the class "matrix"
dda$get()
 cacheSolve(dda)
##It works!!!

##Now lets look at the system runnning time to get a view on computational efficiency

system.time(solve(ddtest))         ##without caching

## Timing stopped at: 0.003 0 0.123 
## user  system elapsed 
##  1.973   0.009   1.968 

system.time(cacheSolve(dda))         ##with caching
## Spectacular results!!!
##user  system elapsed 
##      0       0       0 

##Conclusion:
##Computational time is greatly reduced with the cached version of inversing matrices. 

