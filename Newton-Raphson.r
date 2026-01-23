f <- function(x) {x^2-2}
print(f(5))

fstrich <- function(x) {2*x}

x <- 2 
abbruch <- 0.0001

approx <-function(x) {x - f(x)/fstrich(x)}

appri <- x[1]
x[1] <- 2 

