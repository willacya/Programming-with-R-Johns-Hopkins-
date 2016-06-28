## Maximising a normal likelihood

## set.seed(1); normals <- rnorm(100, 1, 2)
## nLL <- make.NegLogLik(normals)
make.NegLogLik <- function(data, fixed=c(FALSE,FALSE)) {
    params <- fixed
    function(p) {
        params[!fixed] <- p
        mu <- params[1]
        sigma <- params[2]
        a <- -0.5*length(data)*log(2*pi*sigma)
        b <- -0.5*sum((data-mu)^2) / (sigma^2)
        -(a + b)
    }
}

## estimating parameters

## > optim(c(mu = 0, sigma = 1), nLL)$par
##       mu     sigma
## 1.218239  1.787343

## fixing mu = 2
## > nLL <- make.NegLogLik(normals, c(FALSE,2))
## > optimize(nLL, c(-1, 3))$minimum
## [1] 1.217775

## fixing sigma = 1
## > nLL <- make.NegLogLik(normals, c(1,FALSE))
## > optimize(nLL, c(1e-6, 10))$minimum
## [1] 1.800596

## plotting the likelihood

## > nLL <- make.NegLogLik(normals, c(1, FALSE))
## > x <- seq(1.7, 1.9, len = 100)
## > y <- sapply(x, nLL)
## > plot(x, exp(-(y - min(y))), type = "l")

## > nLL <- make.NegLogLik(normals, c(FALSE, 2))
## > x <- seq(0.5, 1.5, len = 100)
## > y <- sapply(x, nLL)
## > plot(x, exp(-(y - min(y))), type = "l")