##Inverse Matrix Function
## This function stores the inverse of a matrix in a cache. If the
## inverse of the matrix is already exists, the value is not recalculated
## but instead is pulled from the cache. If a new matrix is used, the
##cache is cleared and the value is recalculated. 

## The first function makes the matrix cache for the matrix to be stored.
##It creates a list of objects that retrieve and sets values. Every time
##a new matrix is used that will need to be inverted, the cache is cleared
##until the second function finds the inverse and stores it. 
makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m <<-solve
  getinv<-function () m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##The second function looks within the first to see if there is already
##a matrix stored in the cache. If there is already a matrix, it prints 
##the matrix. Otherwise, it solves for the inverse of the matrix and 
##pushes it into the cache.

cacheSolve<-function(x, ...){
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m
}

##sample matrix and analysis. 
mtrx<-matrix(3:6, nrow=2, ncol=2)

aMatrix<-makeCacheMatrix(mtrx)

aMatrix$get()
cacheSolve(aMatrix)