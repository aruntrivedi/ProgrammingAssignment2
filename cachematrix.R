## Open R Studio and setwd() to the directory eg.c:/users/username/cache1/ProgrammingAssignment2/cachematrix.R
## "cache1" is the local repo where forked file is cloned
## Open cahematrix.R file forked from github repo to define the function

## create makeCacheMatrix () function to define set of functions 
## these functions are to set and get the value of the matrix
## it also has a function call to CashSolve which will inverse value of matrix defined in makeCacheMatrix()

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
setmatrix <- function (y) {
  x <<- y
  m <- NULL
}
getmatrix <- function () x
cacheinv <- function (inv) m <<- inv
getinv <- function () m
list (setmatrix=setmatrix, getmatrix=getmatrix, cacheinv=cacheinv, getinv=getinv)
}


## function expects input matrix to be inverted
## input matrix is assigned to m
## retrieve cache if its not NULL 
## else matrix is populated using getmatrix and
## assigned to z
## matrixinverse function is executed on z and
## set inverted matrix is done on m which is final
## value of the inverted matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv ()
  if (!is.null(m)){
    message("getting cached matrix")
    return(m)
    }
  z <- x$getmatrix()
  m <- solve(z) %*% z
  x$cacheinv(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
