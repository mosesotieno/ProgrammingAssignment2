#---- Header ----

#--- cache_solve_matrix

#--- Purpose: Do the ProgrammingAssignment2 in Coursera

#--- Author: Moses Otieno

#--- Date: 12Aug2020


#---- Body ----


#--- overall descriptions of the functions

# makeCacheMatrix - It creates a special matrix that cache 
# cacheSolve - It computes the inverse of special matrix cached by makeCacheMatrix


## This function creates a special matrix 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){x}
  setmatr <- function(solve){m <<- solve}
  getmatr <- function(){m}
  list(set = set,
       get = get,
       setmatr = setmatr,
       getmatr = getmatr)
  
}


## The function computes the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatr()
  if(!is.null(m)){
    message("Cached data from previous result")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatr(m)
  m
}


set.seed(10); x <- matrix(rnorm(36, 10, 1), 6)
y <- makeCacheMatrix(x)
cacheSolve(y)


