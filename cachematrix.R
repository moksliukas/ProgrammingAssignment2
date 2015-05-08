
## 1. Function 'makeCacheMatrix' verify  whether the matrix is square. If not - return error message.
## 2. Function 'set' sets original data
## 3. Function 'get' gets original data
## 4. Function 'setinv' sets inverse matrix
## 5. Function 'getinv' gets inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x)!=nrow(x)) stop ("not square matrix!")
  
  inv<-NULL
  set <- function(y) {
  x <- y
  inv<-NULL
  }
  get <- function() x
  setinv<-function(mtr) inv<<-mtr
  getinv<-function() inv
  list(set = set, get = get, setinv=setinv, getinv=getinv)
}

## Function return inverse matrix from x. Function using R function 'solve()'.
## If inverse matrix are in cache, function return data from cache.
## If inverse matrix not exist in cache - function  calculate inverse matgrix
 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Testing functions

## Not square matrix
B = matrix( 
     c(2, 4, 3, 11,5,6), 
     nrow=2, 
     ncol=3)

a<-makeCacheMatrix(B)

## Square matrix
B = matrix( 
  c(2, 4, 3, 11, 2, 4, 3, 11,5), 
  nrow=3, 
  ncol=3)

a<-makeCacheMatrix(B)

cacheSolve(a)
cacheSolve(a)
cacheSolve(a)

#
