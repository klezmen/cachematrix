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


##Test the results with this matrix
dd <- makeCacheMatrix()
dd$set(matrix(c(0,2,2,0),2,2))
dd$get()
 cacheSolve(dd)
##It works