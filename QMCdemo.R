if (!require(randtoolbox)) {install.packages("randtoolbox"); require(randtoolbox)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}

set.seed(42)
N <- 1e4

### Monte-Carlo Integration ###

mcint.pi <- cbind(x=runif(N), y=runif(N))
mcint.pi.fun <- function(x) {sqrt(1 - x^2)}
mcint.pi.tf <- function(v) {v[2] < mcint.pi.fun(v[1])}
mcint.pi.res <- sapply(1:N, function(i) mcint.pi.tf(mcint.pi[i, ]))
sum(mcint.pi.res) / N * 4

### Quasi Monte-Carlo Integration ###

qmcint.pi <- rbind(c(0,0), sobol(N, 2))
colnames(qmcint.pi) <-c('x', 'y')
qmcint.pi.res <- sapply(1:N, function(i) mcint.pi.tf(qmcint.pi[i, ]))
sum(qmcint.pi.res) / N * 4

### Sobol Sequence ###

ggplot(data=data.frame(mcint.pi), aes(x=x, y=y)) + geom_point()
ggplot(data=data.frame(qmcint.pi), aes(x=x, y=y)) + geom_point()
ggplot(data=data.frame(qmcint.pi[1:64, ]), aes(x=x, y=y)) + geom_point() + 
  theme(panel.grid.minor = element_line(colour = "red", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "red", linetype = "dotted"))
