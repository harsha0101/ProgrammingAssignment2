## This function creates a special "matrix" object that can cache its inverse

##Following to that contains a list of another set of functions to
##set the matrix
##get the matrix
##setting the inverse of the matrix
##getting the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
invrs<-NULL
set<-function(y){
    x<<-y
    invrs<<-NULL
}

get<- function(x)
  setinverse<-function(inverse) invrs<<-inverse
  getinverse<-function()invrs
  list(set=set, get=get, setinverse=setinverse,
       getinverse=getinverse)
}  

##The following calculates the inverse of the special "matrix" created with the above function

##It checks the inverse of the matrix, if the inverse has already been calculated and the matrix has
##not changed, then it retrieves the inverse from the cache

##If the null of the inverse is true, it calculates the inverse and sets the new value in cache 

cacheSolve <- function(x, ...) {
  invrs<-x$getinverse()
  if(!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }

  data<-x$get()
  invrs<-inverse(data, ...)
  x$setinverse(invrs)
  invrs
}
  ## Returns a matrix that is the inverse of 'x'
