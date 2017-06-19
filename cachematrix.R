## Put comments here that give an overall description of what your
## functions do

# 1. A function named 'makeCacheMatrix()' is declared and told to take a matrix 'x'
#    as an argument.
# 2. A variable 'inv_x' is declared inside the makeCacheMatrix() environment and 
#    its value set to NULL. This will store the inverse matrix once it has been
#    calculated.
# 3. A function named 'set_matrix()' is declared / defined. This function takes two
#    arguments, namely 'elements' (a numeric vector specifying the values of the 
#    matrix elements), and 'num_rows', specifying the number of rows (and columns).
# 4. set_matrix() updates the matrix 'x', which was introduced in the parent 
#    environment rather than within the set_matrix() definition and therefore must
#    be accessed via the <<- operator. Invertible matrices must be square, so
#    the arguments named nrow and ncol are both set to the same value (num_rows).
# 5. set_matrix() must also immediately erase the value of the inverse matrix 
#    upon having set a new value for the original/uninverted matrix, and again
#    requires the <<- operator since inv_x is declared in the parent environment.
# 6. A function named 'get_matrix()' is declared/defined, which takes no argument
#    and simply returns whatever 'x' is currently storing.
# 7. A function named 'set_inverse()' is declared/defined, which takes the variable
#    'inverse' as its only argument and assigns its value to 'inv_x' in the parent
#    environment via the use of <<-.
# 8. A function named 'get_inverse()' is declared/defined, which takes no argument
#    and simply returns whatever 'inv_x' is currently storing.
# 9. Finally a list is returned, with each item in the list allowing each of the 
#    above-mentioned functions within makeCacheMatrix() to be accessed via a name
#    in conjunction with the $ operator later (rather than a numbered index). The
#    name they are each accessed by is set to the same as each function name just
#    for simplicity.

makeCacheMatrix <- function(x = matrix()) {  
      inv_x <- NULL
      
      set_matrix <- function(elements, num_rows){
            x <<- matrix(elements, nrow = num_rows, ncol = num_rows)
            inv_x <<- NULL
      }
      get_matrix <- function() x
      set_inverse <- function(inverse){
            inv_x <<- inverse 
      }
      get_inverse <- function() inv_x 
      
      list(set_matrix = set_matrix, get_matrix = get_matrix,
           set_inverse = set_inverse, get_inverse = get_inverse)  
}

# 1. The function 'cacheSolve()' is declared, taking an argument 'x' (which is an
#    object of the type 'makeCacheMatrix') as well as further unspecified arguments
#    indicated by '...'. This x is not a simple matrix like the x used in the 
#    definition of makeCacheMatrix() above.
# 2. A variable named 'inv_x' is instantiated, being assigned the value returned by
#    calling x$get_inverse() (this command is enabled by the list returned by the
#    makeCacheMatrix() function), i.e. this 'inv_x' takes on the value of the 
#   'inv_x' variable found within the makeCacheMatrix() environment.
# 3. The value now stored by the 'inv_x' found within the cacheSolve() environment
#    is inspected to see if it is NULL, and if it is not NULL then there is 
#    already a cached record of an inverse matrix available and there is no need 
#    to recalculate it; instead it is simply returned and cacheSolve() exits here.
# 4. If the value of inv_x was in fact NULL then cacheSolve() will not have exited
#    and a value for inv_x is calculated via the 'solve()' function. This takes
#    as an argument the matrix retrieved via x$get_matrix() and returns its 
#    inverse.
# 5. The makeCacheMatrix object 'x' passed to cacheSolve() then has the 'inv_x' 
#    variable in its own environment updated with the output of 
#    solve(x$get_matrix())... which has just been stored in the variable 'inv_x' 
#    that exists inside this cacheSolve() environement, via the x$set_inverse() 
#    function. 

#  Test these functions via following example command sequence:

#  source("cachematrix.R")
#  matrix_element_vector1 <- c(4,3,3,2)
#  my_matrix <- matrix(matrix_element_vector1, nrow = 2, ncol = 2)
#  mCM <- makeCacheMatrix(my_matrix)
#  mCM$get_matrix()
#  mCM$get_inverse()
#  cacheSolve(mCM)
#  mCM$get_inverse()
#
#  matrix_element_vector2 <- c(3,4,2,3)
#  mCM$set_matrix(matrix_element_vector2, 2)
#  mCM$get_matrix()
#  mCM$get_inverse()
#  cacheSolve(mCM)
#  mCM$get_inverse()

cacheSolve <- function(x, ...) {
        inv_x <- x$get_inverse()
 
        if(!is.null(inv_x)){     
          message("getting cached data")
          return(inv_x)
        }
        
        inv_x <- solve(x$get_matrix())
        x$set_inverse(inv_x) 
}