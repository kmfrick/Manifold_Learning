#!/usr/bin/env Rscript
set.seed(50321)

a <- 10
b <- 50
c <- 80

x <- seq(0.1, 100, length.out=1052)
fx <- 0.5 * sin(x - a) / (x - a) + 0.8 * sin(x - b) / (x - b) + 0.3 * sin(x - c) / (x - c)
f <- fx + rnorm(length(x), mean=0, sd=0.05^2)
f - fx
dev.new()
plot(x, f, type='l');
dat <- data.frame(x, f)
# 11-grade polynomial
linreg.11 <- lm (f ~ poly(x,11), dat)
points(x, predict(linreg.11), col="blue", type="l")

# RBF
n <- length(x)
sigma <- 1 # this is _very_ sensitive, but there are heuristics for it
kk <- tcrossprod(x)
dd <- diag(kk)

myRBF.kernel <- exp(sigma*(-matrix(dd,n,n)-t(matrix(dd,n,n))+2*kk))

lambda <- 0.01
ident.n <- diag(rep(1,n))
alphas <- solve(myRBF.kernel + lambda*ident.n)
alphas <- alphas %*% f
lines(x,myRBF.kernel %*% alphas,col="magenta")

dev.off()
