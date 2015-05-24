makeCacheMatrix <- function(x = matrix()) {
  z<-NULL
  set<-function(y){
    x<<-y
    z<<-NULL
  }
  get<-function() x
  setmatrixinv<-function(solve) z<<- solve
  getmatrixinv<-function() z
  list(set=set, get=get,
       setmatrixinv=setmatrixinv,
       getmatrixinv=getmatrixinv)
}

cacheSolve <- function(x=matrix(), ...) {
  z<-x$getmatrixinv()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  matrix<-x$get()
  z<-solve(matrix, ...)
  x$setmatrixinv(z)
  z
}

test1<-function(matrix1){
  temp<-makeCacheMatrix(matrix1)
  cacheSolve(temp)
  sol<- cacheSolve(temp)
  sol
}

r<-rnorm(100)
mat1<-matrix(r,nrow=10, ncol=10)
sol<-test1(mat1)
