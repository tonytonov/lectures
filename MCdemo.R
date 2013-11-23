if (!require(VGAM)) {install.packages("VGAM"); require(VGAM)}

### Law of Large Numbers ###

set.seed(42)
N <- 1e6
Np <- 5e3

lln.dice <- sample(1:6, N, replace = T)
lln.dice <- data.frame(dice = lln.dice, mean = cumsum(lln.dice) / 1:length(lln.dice))
plot(lln.dice$mean[1:Np], type='l')
abline(h=3.5, col='red', lwd=2)
plot(lln.dice$mean, type='l', ylim=c(3.45, 3.55))
abline(h=3.5, col='red', lwd=2)

lln.exp <- rexp(N, rate = 2)
lln.exp <- data.frame(exp = lln.exp, mean = cumsum(lln.exp) / 1:length(lln.exp))
plot(lln.exp$mean[1:Np], type='l')
abline(h=1/2, col='red', lwd=2)
plot(lln.exp$mean, type='l', ylim=c(0.49, 0.51))
abline(h=1/2, col='red', lwd=2)

lln.pareto <- rpareto(N, 1, 3/2)
lln.pareto <- data.frame(pareto = lln.pareto, mean = cumsum(lln.pareto) / 1:length(lln.pareto))
plot(lln.pareto$mean[1:Np], type='l')
abline(h=3, col='red', lwd=2)
plot(lln.pareto$mean, type='l', ylim=c(2.8, 3.2))
abline(h=3, col='red', lwd=2)

lln.cauchy <- rcauchy(N)
lln.cauchy <- data.frame(pareto = lln.cauchy, mean = cumsum(lln.cauchy) / 1:length(lln.cauchy))
plot(lln.cauchy$mean[1:Np], type='l')
plot(lln.cauchy$mean, type='l')

### Central Limit Theorem ###

cumvar <- function(x) {
  n <- 1 : length(x)
  (cumsum(x^2)) / (n-1) - cumsum(x)^2 / n / (n-1) 
}

clt.dice <- data.frame(dice = lln.dice$dice, mean = lln.dice$mean, var = cumvar(lln.dice$dice))
clt.dice$c1 <- clt.dice$mean - 3 * sqrt(clt.dice$var / 1:N)
clt.dice$c2 <- clt.dice$mean + 3 * sqrt(clt.dice$var / 1:N)
plot(sqrt(clt.dice$var / 1:N), type='l', log='y')
plot(clt.dice$mean, type='l', ylim=c(3.45, 3.55))
abline(h=3.5, col='red')
lines(clt.dice$c1, col='blue'); lines(clt.dice$c2, col='blue')

clt.exp <- data.frame(dice = lln.exp$exp, mean = lln.exp$mean, var = cumvar(lln.exp$exp))
clt.exp$c1 <- clt.exp$mean - 3 * sqrt(clt.exp$var / 1:N)
clt.exp$c2 <- clt.exp$mean + 3 * sqrt(clt.exp$var / 1:N)
plot(sqrt(clt.exp$var / 1:N), type='l', log='y')
plot(clt.exp$mean, type='l', ylim=c(0.49, 0.51))
abline(h=0.5, col='red')
lines(clt.exp$c1, col='blue'); lines(clt.exp$c2, col='blue')

clt.pareto <- data.frame(dice = lln.pareto$pareto, mean = lln.pareto$mean, var = cumvar(lln.pareto$pareto))
clt.pareto$c1 <- clt.pareto$mean - 3 * sqrt(clt.pareto$var / 1:N)
clt.pareto$c2 <- clt.pareto$mean + 3 * sqrt(clt.pareto$var / 1:N)
plot(sqrt(clt.pareto$var / 1:N), type='l', log='y')
plot(clt.pareto$mean, type='l', ylim=c(2.8, 3.2))
abline(h=3, col='red')
lines(clt.pareto$c1, col='blue'); lines(clt.pareto$c2, col='blue')

### Monte-Carlo Integration ###

mcint.pi <- cbind(x=runif(N), y=runif(N))
mcint.pi.fun <- function(x) {sqrt(1 - x^2)}
mcint.pi.tf <- function(v) {v[2] < mcint.pi.fun(v[1])}
mcint.pi.res <- sapply(1:N, function(i) mcint.pi.tf(mcint.pi[i, ]))
sum(mcint.pi.res) / N * 4

mcint.f <- function(x) cos(pi * x / 2) * pi /2
mcint.val <- integrate(mcint.f, lower=0, upper=1)
mcint.x <- runif(N)
mcint.f.x <- mcint.f(mcint.x)
mcint.res <- data.frame(f.x = mcint.f.x, mean = cumsum(mcint.f.x) / 1:N, var =  cumvar(mcint.f.x))
mcint.res$c1 <- mcint.res$mean - 3 * sqrt(mcint.res$var / 1:N)
mcint.res$c2 <- mcint.res$mean + 3 * sqrt(mcint.res$var / 1:N)
plot(mcint.res$mean, type='l', ylim=c(0.99, 1.01))
abline(h=1, col='red')
lines(mcint.res$c1, col='blue'); lines(mcint.res$c2, col='blue')

### Rejection Sampling ###

hist(mcint.pi[mcint.pi.res, 1], breaks=100, prob=T)
plot.pts <-(0:1e3)/1e3
lines(x = plot.pts, y = mcint.pi.fun(plot.pts) * 4 / pi, col = 'red')

### Inverse Transform Sampling ###

mcinv.pts <- runif(N)
mcinv.res <- - log(1 - mcinv.pts) / 2
hist(mcinv.res, breaks=100, prob=T, xlim=c(0, 3))
lines(x = plot.pts * 3, y = dexp(plot.pts * 3, rate=2), col='red')
