## add_2(3, 5) = 8.
add_2 <- function(x, y) {
    x + y
}

## returns numbers in a vector greater than 10.
above10 <- function(x) {
    use <- x > 10
    x[use]
}

## additional argument for greater than, with deafult as 10.
above <- function(x, n = 10) {
    use <- x > n
    x[use]
}

## returns mean of columns in a matrix or dataframe.
## example: columnmean(airquality, FALSE)
columnmean <- function(y, removeNA = TRUE) {
    nc <- ncol(y) ## no. of columns.
    means <- numeric(nc) ## create vector of length nc with all values 0.
    for(i in 1:nc) {
        means[i] <- mean(y[,i], na.rm = removeNA) ## takes func argument removeNA.
    }
    means
}

## Lexical Scoping.

## searches the workspace for a z variable.
f <- function(x, y) {
    x^2 + y / z ## z is a free variable
}

## cube <- make.power(3) assigns the function pow to cube with n as 3.
## cube(10) = 1000.
make.power <- function(n) {
    pow <- function(x) {
        x^n
    }
    pow
}

