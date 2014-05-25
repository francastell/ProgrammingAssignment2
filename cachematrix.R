## These functions have the main goal to calculate a matrix inverse 
## and by doing so it makes use of R lexical scoping to cache previously 
## computed data for performance improvement

## makeCacheMatrix function is responsible for encapsulating the matrix inverse
## value and associated functions to manipulate the data

makeCacheMatrix <- function(x = matrix()) {
        # variable to hold the calculated inverse
        inv <-NULL
        
        #setters and getters for matrix and inverse
        set<-function(m1){
                m <<-m1
                inv <<- NULL
        }
        get<-function(){
                m
        }
        setInv<-function(i){
                inv <<- i
        }
        getInv<-function(){
                inv
        }
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve function is responsible for using the  given function as parameter
## to calculate the inverse of a matrix
cacheSolve <- function(x, ...) {
        #get the inverse and if already in cache return it
        i <- x$getInv()
        if (!is.null(i)){
                message ("getting cached matrix inverse")
                return (i)
        }
        
        # if inverse has not been calculated, do so now 
        x$set(...)
        mtx <-x$get()
        i <- solve(mtx)
        
        #now cache calculted value
        x$setInv(i)
        print ("solved i")
        print(x$getInv())
}

## test function 
test<-function(){
        mat<-matrix(1:4, nrow=2, ncol=2)
        f<-makeCacheMatrix()
        for (i in 1:3){
                cacheSolve(f, mat)
        }
}
