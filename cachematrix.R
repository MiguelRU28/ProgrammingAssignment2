## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1.MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

library(MASS) # calling for calculating inverse 

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL # initiating with inverse as Null
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x #function to obtain inverse of the matrix
  }
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}

## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Write a short comment describing this function

cacheSolve<-function(x,...)######### gets cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){         #checking whether inverse is NULL
    message("getting cached data!")
    return(inv)  #returning inverse value
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv ##return a matrix that is the inverse of "x"
}
