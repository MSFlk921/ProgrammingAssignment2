## The first function make the Matrix and the second cache the value of inverse
## 
## For example, you can make the Matrix 
## m <- makeMatrix(matrix(1:4,2,2)) 
## and then get the catch of the inverse
## cacheSolve(m)    
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 


## This functon create the Matrix and you can set the value of it

makeMatrix <- function(x = numeric()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, 
       get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function is the inverse of the Matrix

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
