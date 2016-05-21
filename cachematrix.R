
## NAME: ABHISHEK SAXENA
## DATE: 21st MAY 2016
## FUNCTION DESCRIPTIONS - Overall 3 functions created
## 1. matrixEqual - Compares two matrix and whether they are same
## 2. makeCacheMatrix - Takes input of a matrix which needs to be inverted
## 3. cacheSolve - Returns either a chached inverted matrix or create a inverse and caches it

## Initializing few variables
cacheVal <- NULL
originalmatrix<-NULL
returnCacheObject<-FALSE

## 1. matrixEqual - Compares two matrix and whether they are same
## ---------------------------------------------------------------
matrixEqual <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

## 2. makeCacheMatrix - Takes input of a matrix which needs to be inverted
## ------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  cacheSolve(x)
}

## 3. cacheSolve - Returns either a chached inverted matrix or create a inverse and caches it
## -------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Check if originalmatrix is NULL - means calling for inverse for first time, else originalmatrix
  ## will have a value.
  if (is.null(originalmatrix))
  {
    originalmatrix<<-x
  }
  else
  {
    ## check if same matrix is being asked to be inverted OR new matrix?
    if (matrixEqual(originalmatrix,x))
    {
      ## if same matrix - then return Cached object
      returnCacheObject <- TRUE
    }
    else
    {
      ## else do not return cached object
      originalmatrix<<-x
      returnCacheObject <- FALSE
    }
  }
  
  if (returnCacheObject==FALSE)
  {
    ## Cache it then
    print("Cache created")
    cacheVal<<-solve(x)
    x<-cacheVal
  }
  else
  {
    ## Return the cache
    x<-cacheVal
    print("Getting Cached data")
  }
  
  #return the matrix
  x
}
