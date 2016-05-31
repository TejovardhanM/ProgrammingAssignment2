## This function take the square matrix and does the inverse of matrix.
## Below are the calling mechanism.
#CallMatrixtoInverse <-makeCacheMatrix()
#CallMatrixtoInverse$get()
#CallMatrixtoInverse$set(matrix(1:4,2,2))
#CallMatrixtoInverse$get()
#cacheSolve(CallMatrixtoInverse)

## Write a short comment describing this function

makeCacheMatrix<-function(x=matrix())
{
  InverseMatrix<-NULL # Initiating Null value for mean.
  set<-function(y) # Creating a set function internally.
  {
    x<<-y # saving it to global variable, a default value set by the user: 
    InverseMatrix<<-NULL # Resetting the Mean value.
  }
  get<-function()return (x) #retrieve global variable.
  
  setInv<-function(inverse) return (InverseMatrix<<-inverse) 
  getInv<-function()return (InverseMatrix)
  
  list(set=set,
       get=get,
       setInv=setInv,
       getInv=getInv)
}

cacheSolve<-function(x)
{
  
  Final.Inverse<-x$getInv()
  
  if(!is.null(Final.Inverse))
  {
    message("Getting cached data.")
    return(Final.Inverse)
  }
  
  doInv<-x$get() # Assign the matrix data..
  Final.Inverse<-solve(doInv) # apply the Solve function to data.
  x$setInv(Final.Inverse) # assign the value to global variable of X
  return(Final.Inverse)
}
