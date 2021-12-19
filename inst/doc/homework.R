## -----------------------------------------------------------------------------
# A program to calculate sum of Fibonacci sequence
x <- c() 
x[1] <- 1
x[2] <- 1
s <- x[1] + x[2]
for (i in 3:10) {
  x[i] <- x[i-1] + x[i-2]
  s <- s + x[i]
  print(s)
}

## -----------------------------------------------------------------------------
# A program to give a scatter diagram of a linear regression equation
x <- c(1,6,5,8,4,2,9,11) 
y <- c(11,68,59,79,36,22,95,120)
plot(x,y)

## -----------------------------------------------------------------------------
# Using knitr & kable to produce a table of R data "mtcars"
library(knitr) 
data <- head(mtcars)
kable(data)

## -----------------------------------------------------------------------------
# sigma = 2
f <- function(x, sigma = 2){
  (x/sigma^2)*exp(-x^2/(2*sigma^2))
  }
x <- seq(0, 5, .01)
dim(x) <- length(x) 
y <- apply(x, 1, f)
hist(y, breaks = 40, freq = FALSE, main = "hist", xlab = "function value", ylab = "frequence/function value")
lines(density(y), col = "red")

## -----------------------------------------------------------------------------
# sigma = 4
f <- function(x, sigma = 4){
  (x/sigma^2)*exp(-x^2/(2*sigma^2))
  }
x <- seq(0, 5, length=100)
dim(x) <- length(x) 
y <- apply(x, 1, f)
hist(y, breaks = 40, main = "hist", freq = FALSE, xlab = "function value", ylab = "frequence/function value")
lines(density(y), col = "red")

## -----------------------------------------------------------------------------
# sigma = 6
f <- function(x, sigma = 6){
  (x/sigma^2)*exp(-x^2/(2*sigma^2))
  }
x <- seq(0, 5, length=100)
dim(x) <- length(x) 
y <- apply(x, 1, f)
hist(y, breaks = 20, main = "hist", freq = FALSE, xlab = "function value", ylab = "frequence/function value")
lines(density(y), col = "red")

## -----------------------------------------------------------------------------
f <- function(x){
  m <- 10000
  t <- runif(m, min = 0, max = x)
  theta_hat  = x * mean((gamma(3 + 3)/(gamma(3) * gamma(3))) * t^2 * (1-t)^2)
}
for (i in seq(.1, .9, length = 9)){
  print(c(f(i), pbeta(i, 3, 3)))
}

## -----------------------------------------------------------------------------
#define function MC.anti() to withdraw samples from distribution Rayleigh with "antithetic" as the optional option
MC.anti <- function(x, sigma = 2, R = 10000, antithetic = TRUE){
  u <- runif(R/2)
  if (!antithetic)
    v <- runif(R/2)
  else
    v <- 1 - u
  u <- c(u, v)
  cdf <- numeric(length(x))
  for (i in 1:length(x)){
    g <- x[i]^2 / sigma^2 * u * exp(- x^2 * u^2 / (2 * sigma^2))
    cdf[i] = mean(g) 
  }
  cdf
}

x <- seq(.1, 2.5, length = 5)
MC1 <- MC.anti(x, anti = FALSE)
MC2 <- MC.anti(x)

m <- 1000
MC1 <- MC2 <- numeric(m)
x <- 2
for (i in 1:m){
  MC1[i] <- MC.anti(x, R = 10000, anti = FALSE)
  MC2[i] <- MC.anti(x, R = 10000)
}
print(sd(MC1))
print(sd(MC2))
print((var(MC1)-var(MC2))/var(MC1))

## -----------------------------------------------------------------------------
m <- 10000
theta.hat <- se <- numeric(3)

#define function g(x)
g <- function(x){
  1 / (x**4 * sqrt(2 * pi)) * exp(-1 / (2 * x**2)) * (x>0) * (x<1)
}

#without importance function
x <- runif(m)
f0g <- g(x)
theta.hat[1] <- mean(f0g)
se[1] <- sd(f0g)

#when f1 is selected as the importance function
x <- runif(m)
f1g <- g(x) * exp(x**5/10)
theta.hat[2] <- mean(f1g)
se[2] <- sd(f1g)

#when f2 is selected as the importance function
x <- runif(m)
f2g <- g(x) * (1/x)
theta.hat[3] <- mean(f2g)
se[3] <- sd(f2g)

#show the estimation & se of each situation
rbind(theta.hat, se)

## -----------------------------------------------------------------------------
m <- 10000
theta.hat <- se <- numeric(1)

#define function g(x)
g <- function(x){
  1 / (x**4 * sqrt(2 * pi)) * exp(-1 / (2 * x**2)) * (x>0) * (x<1)
}

#when f is selected as the importance function
x <- runif(m)
f2g <- g(x) * exp(x**5/20)
theta.hat[1] <- mean(f2g)
se[1] <- sd(f2g)

#show the estimation & se of both situation
rbind(theta.hat, se)

## -----------------------------------------------------------------------------
n <- 20
alpha <- .05
x <- rchisq(n,2)
UCL <- mean(x) + sd(x)*qt(1-alpha/2,n-1)
LCL <- mean(x) - sd(x)*qt(1-alpha/2,n-1)
UCL;LCL

## -----------------------------------------------------------------------------
n <- 1000 
alpha <- 0.05
mu0 <- 1
m <- 10000 #number of replicates
p <- numeric(m) #storage for p-values
for (j in 1:m) 
  {
  x <- rchisq(n, 1)
  ttest <- t.test(x, mu = mu0)
  p[j] <- ttest$p.value
  }
p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))

## -----------------------------------------------------------------------------
n <- 1000
alpha <- .05
m <- 10000 #number of replicates
p <- numeric(m) #storage for p-values
for (j in 1:m) 
  {
  x <- runif(n, min=0,max=2)
  ttest <- t.test(x, mu = 1)
  p[j] <- ttest$p.value
  }
p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))

## -----------------------------------------------------------------------------
n <-1000
alpha <- .05
m <- 10000 #number of replicates
p <- numeric(m) #storage for p-values
for (j in 1:m) 
  {
  x <- rexp(n, rate = 1)
  ttest <- t.test(x, mu = 1)
  p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))

## ----fig.height=3-------------------------------------------------------------
x1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460, -0.937, 0.779, -1.409, 0.027, -1.569)
x2 <- c(1.608, 1.009, 0.878, 1.600, -0.263, 0.680, 2.280, 2.390, 1.793, 8.091, 1.468)
B <- 10000
set.seed(12345)
thetastar1 <- thetastar2 <- thetastar3 <- numeric(B)
theta1 <- mean(x1)
theta2 <- mean(x2)
n <- 10
m <- 11
for(b in 1:B){
  xstar1 <- sample(x1,replace=TRUE)
  xstar2 <- sample(x2,replace=TRUE)
  thetastar1[b] <- mean(xstar1)
  thetastar2[b] <- mean(xstar2)
  thetastar3[b] <- thetastar1[b]-thetastar2[b]
}
x1_sd=sd(x1)
x2_sd=sd(x2)
round(c(x1_mean=mean(x1),x1_sd=sd(x1),x2_mean=mean(x2),x2_sd=sd(x2),SE.boot=sd(thetastar3),se_sample=sqrt(((n-1)*x1_sd^2+(m-1)*x2_sd)/(m+n-2))),6)

## -----------------------------------------------------------------------------
library(MASS)
n <- c(10, 20, 30, 50, 100, 500) #different sample size 
d <- 2
cv_r <- qchisq(.975, d*(d+1)*(d+2)/6)
cv_l <- qchisq(.025, d*(d+1)*(d+2)/6)
mu <- numeric(d)
sigma0 <- diag(d)
b_1d <- 0

#define function sk to calculate the sample skewness
sk <- function(x,k){
  x <- mvrnorm(k, mu, sigma0)
  x_bar <- colMeans(x)
  xigma <- matrix(0, d, d)
  
#define the maximum likelihood estimator  
  for (i in 1:k){
    xigma <-  xigma + (1/k) * (x[i,]-x_bar) %o% (x[i,]-x_bar)
  }
  xigma <- solve(xigma)

#define the skewness statistic
  for (i in 1:k){
    for (j in 1:k){
      b_1d <- b_1d + ((x[i,]-x_bar) %*% xigma %*% (x[j,]-x_bar))^3
    }
  }
  
  chi_test <- b_1d / k^2 * k/6
  return(chi_test)
}

p.reject <- numeric(length(n))
m <- 100

for (i in 1:length(n)){
  sktests <- numeric(m)
  for (j in 1:m){
    x <- rnorm(n[i])
    y <- rnorm(n[i])
    sktests[j] <- as.integer( sk(x,n[i]) >= cv_r || sk(x,n[i]) <= cv_l ) 
  }
  p.reject[i] <- mean(sktests) #calculate the proportion of samples located in reject area
}
p.reject

## -----------------------------------------------------------------------------
library(MASS)
d <- 2
k <- 30
m <- 500
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
#critical value for the skewness test
cv_r <- qchisq(.975, d*(d+1)*(d+2)/6)
cv_l <- qchisq(.025, d*(d+1)*(d+2)/6)

#define function sk to calculate the sample skewness
sk <- function(x, k){
  x <- mvrnorm(k, mu, sigma0)
  x_bar <- colMeans(x)
  xigma <- matrix(0, d, d)
  
#define the maximum likelihood estimator  
  for (i in 1:k){
    xigma <-  xigma + (1/k) * (x[i,]-x_bar) %o% (x[i,]-x_bar)
  }
  xigma <- solve(xigma)

#define the skewness statistic
  for (i in 1:k){
    for (j in 1:k){
      b_1d <- b_1d + ((x[i,]-x_bar) %*% xigma %*% (x[j,]-x_bar))^3
    }
  }
  
  chi_test <- b_1d / k^2 * k/6
  return(chi_test)
}

for (j in 1:N) { 
  #for each epsilon
  e <- epsilon[j]
  sktests <- numeric(m)
  for (i in 1:m) { 
    #for each replicate
    sigma <- sample(c(1, 10), replace = TRUE, size = k, prob = c(1-e, e))
    x <- rnorm(n, 0, sigma)
    sktests[i] <- as.integer(sk(x, k) >= cv_r || sk(x, k) <= cv_l)
  }
  pwr[j] <- mean(sktests)
}
#plot power vs epsilon
plot(epsilon, pwr, type = "b", xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
data(scor,package = "bootstrap")

#define sample size & number of replicates
n <- 1000
sample.size <- dim(scor)[1]
theta_hat <- numeric(n)

#get samples from dataset "scor" using bootstrap
for (i in 1:n){
  index <- sample(1:sample.size, size = sample.size, replace = T)
  data_cor <- cor(scor[index,])
  data_ev <- eigen(data_cor)$value
  data_ev_sum <- sum(data_ev)
  data_lambda <- numeric(length(data_ev))
  data_lambda_1 <- max(data_ev)
  theta_hat[i] <- data_lambda_1/data_ev_sum
}
theta_mean <- mean(theta_hat)
theta_sd <- sd(theta_hat)

#present the conclusion
knitr::kable(cbind(theta_mean,theta_sd))

## -----------------------------------------------------------------------------
data(scor,package = "bootstrap")

#define sample size & number of replicates
n <- 1000
sample.size <- dim(scor)[1]
theta_hat <- numeric(n)

#get samples from dataset "scor" using bootstrap
for (i in 1:n){
  index <- sample(1:sample.size, size = sample.size, replace = T)
  data_cor <- cor(scor[index,])
  data_ev <- eigen(data_cor)$value
  data_ev_sum <- sum(data_ev)
  data_lambda <- numeric(length(data_ev))
  data_lambda_1 <- max(data_ev)
  theta_hat[i] <- data_lambda_1/data_ev_sum
}

theta.jack <- numeric(n)
for (i in 1:n){
  theta.jack[i] = mean(theta_hat[-i])
}
bias.jack <- (n-1)*(mean(theta.jack)-theta_hat)
sd_jack <- sqrt((n-1)*mean((theta.jack-mean(theta_hat))^2))
theta_bias <- mean(bias.jack)

#present the conclusion
knitr::kable(cbind(theta_bias, sd_jack))

## -----------------------------------------------------------------------------
library(boot)
data(scor,package = "bootstrap")

#define sample size & number of replicates
n <- 1000
sample.size <- dim(scor)[1]
theta_hat <- numeric(n)

#get samples from dataset "scor" using bootstrap
for (i in 1:n){
  index <- sample(1:sample.size, size = sample.size, replace = T)
  data_cor <- cor(scor[index,])
  data_ev <- eigen(data_cor)$value
  data_ev_sum <- sum(data_ev)
  data_lambda <- numeric(length(data_ev))
  data_lambda_1 <- max(data_ev)
  theta_hat[i] <- data_lambda_1/data_ev_sum
}

c1 = quantile(theta_hat,0.025)
c2 = quantile(theta_hat,0.975)
print(rbind(c(c1,c2)))

## -----------------------------------------------------------------------------
library(boot)
data(scor,package = "bootstrap")

#define sample size & number of replicates
n <- 1000
sample.size <- dim(scor)[1]
theta_hat <- numeric(n)

#get samples from dataset "scor" using bootstrap
for (i in 1:n){
  index <- sample(1:sample.size, size = sample.size, replace = T)
  data_cor <- cor(scor[index,])
  data_ev <- eigen(data_cor)$value
  data_ev_sum <- sum(data_ev)
  data_lambda <- numeric(length(data_ev))
  data_lambda_1 <- max(data_ev)
  theta_hat[i] <- data_lambda_1/data_ev_sum
}

c1 = quantile(theta_hat,0.025)
c2 = quantile(theta_hat,0.975)
print(rbind(c(c1,c2)))

## -----------------------------------------------------------------------------
#define a function to compute the sample skewness
skewness <- function(x){
  xbar <- mean(x)
  m3 <- mean((x-xbar)^3)
  m2 <- mean((x-xbar)^2)
  return( m3 / m2^1.5 )
}

library(boot)
mu <- 0  # the skewness of normal populations
n <- 20  # the sample size in bootstrap
m <- 1e3 # replicate time in Monte Carlo experiments

boot.skew <- function(x,i) {
  skewness(x[i])
}

ci.norm <- ci.basic <- ci.perc <- matrix(NA,m,2)
for(i in 1:m){
  X <- rnorm(n,mean = 0,sd = 1)
  de <- boot(data = X, statistic = boot.skew, R = 1e3)
  ci <- boot.ci(de,type = c("norm","basic","perc"))
  ci.norm[i,] <- ci$norm[2:3]
  ci.basic[i,] <- ci$basic[4:5]
  ci.perc[i,] <- ci$percent[4:5]
}

cover.prob <- c(mean(ci.norm[,1]<= mu & ci.norm[,2]>= mu),
                mean(ci.basic[,1]<= mu & ci.basic[,2]>= mu),
                mean(ci.perc[,1]<= mu & ci.perc[,2]>= mu))

left.omit <- c(mean(ci.norm[,1]>= mu),
               mean(ci.basic[,1]>= mu),
               mean(ci.perc[,1]>= mu))

right.omit <- c(mean(ci.norm[,2]<= mu),
               mean(ci.basic[,2]<= mu),
               mean(ci.perc[,2]<= mu))
cover.norm <- matrix(data = c(cover.prob,left.omit,right.omit),nrow = 3,byrow = TRUE,)
rownames(cover.norm) <- c("cover probability","miss on the left","miss on the right")
colnames(cover.norm) <- c("standard normal bootstrap confidence interval","basic bootstrap confidence interval","percentile bootstrap confidence interval")

knitr::kable(t(cover.norm))

library(boot)
mu <- sqrt(8/5)  # the skewness of chi-squared distribution
n <- 20  # the sample size in bootstrap
m <- 1e3 # replicate time in Monte Carlo experiments

boot.skew <- function(x,i){
  skewness(x[i])
}

ci.norm <- ci.basic <- ci.perc <- matrix(NA,m,2)

for(i in 1:m){
  X <- rchisq(n,df = 5)
  de <- boot(data = X, statistic = boot.skew, R = 1e3)
  ci <- boot.ci(de,type = c("norm","basic","perc"))
  ci.norm[i,] <- ci$norm[2:3]
  ci.basic[i,] <- ci$basic[4:5]
  ci.perc[i,] <- ci$percent[4:5]
}


cover.prob <- c(mean(ci.norm[,1]<= mu & ci.norm[,2]>= mu),
                mean(ci.basic[,1]<= mu & ci.basic[,2]>= mu),
                mean(ci.perc[,1]<= mu & ci.perc[,2]>= mu))
left.omit <- c(mean(ci.norm[,1]>= mu),
               mean(ci.basic[,1]>= mu),
               mean(ci.perc[,1]>= mu))
right.omit <- c(mean(ci.norm[,2]<= mu),
               mean(ci.basic[,2]<= mu),
               mean(ci.perc[,2]<= mu))

cover.chisq <- matrix(data = c(cover.prob,left.omit,right.omit),nrow = 3,byrow = TRUE,)
rownames(cover.chisq) <- c("cover probability","miss on the left","miss on the right")
colnames(cover.chisq) <- c("standard normal bootstrap confidence interval","basic bootstrap confidence interval","percentile bootstrap confidence interval")

knitr::kable(t(cover.chisq))

## ----warning=FALSE------------------------------------------------------------
# part A: cor.test
#randomly generating x & y as the samples
x <- c(70, 78, 78, 87, 84, 87, 90)
y <- c(71, 77, 79, 86, 84, 90, 78)

#calculate Spearman rank correlation and return the p-value
a <- cor.test(x, y, method = "spearman")
p <- a$p.value


# part B: permutation.test
# form the data
n <- 10000
z <- c(x,y)

rho <- function(s,t){
  m <- length(s)
  Rx <- rank(s, ties.method = "average")
  Ry <- rank(t, ties.method = "average")

  for (i in 1:m) {
    rho1 = sum(Rx[i] * Ry[i]) - m * ((m+1)/2)^2
    rho2 = sum(Rx[i]^2) - m * ((m+1)/2)^2
    rho3 = sum(Ry[i]^2) - m * ((m+1)/2)^2
  }
  rho1/sqrt(rho2 * rho3)
}


rho_init <- rho(x,y)

# randomly choosing data from z
S <- S1 <- S2 <- rho_per <- numeric(n)
for (i in 1:n) {
  S <- sample(z, size = 14,replace = FALSE)
  S1 <- S[1:7]
  S2 <- S[8:14]
  rho_per[i] <- rho(S1,S2)
}

p_perm <- mean(rho_per < rho_init)

knitr::kable(cbind(p, p_perm))

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
library(MASS)

#define functions to calculate the power using different methods
tnn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  if(is.vector(z)) 
    z <- data.frame(z,0);
    z <- z[ix, ];
    NN <- nn2(data=z, k=k+1)
  
  block1 <- NN$nn.idx[1:n1, -1]
  block2 <- NN$nn.idx[(n1+1):n, -1]
  i_1 <- sum(block1 < n1 + .5)
  i_2 <- sum(block2 > n1+.5)
  (i_1 + i_2) / (k * n)
}

eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic = tnn,R = R, sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}


set.seed(10101)
k <- 2
R <- 1500
m <- 100
n_1 <- n_2 <- 20
n <- n_1 + n_2 
N <- c(n_1, n_2)

#giving the parameters of two different distributions
mu_1 <- mu_2 <- c(0,0)
sigma_1 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
sigma_2 <- matrix(c(1,0,0,2), nrow = 2, ncol = 2)

p.values <- matrix(NA,m,3)

for(i in 1:m){
  data1 <- mvrnorm(n_1, mu_1, sigma_1)
  data2 <- mvrnorm(n_2, mu_2, sigma_2)
  data <- rbind(data1, data2)
  
  # using nn method
  p.values[i,1] <- eqdist.nn(data,N,k)$p.value
  # using energy method
  p.values[i,2] <- eqdist.etest(data,sizes=N,R=R)$p.value
  # using ball method
  p.values[i,3] <- bd.test(x=data1, y=data2, num.permutations=R, seed=i*2846)$p.value
}
alpha <- 0.05;
power <- colMeans(p.values < alpha)
power

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
library(MASS)

#define functions to calculate the power using different methods
tnn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  if(is.vector(z)) 
    z <- data.frame(z,0);
    z <- z[ix, ];
    NN <- nn2(data=z, k=k+1)
  
  block1 <- NN$nn.idx[1:n1, -1]
  block2 <- NN$nn.idx[(n1+1):n, -1]
  i_1 <- sum(block1 < n1 + .5)
  i_2 <- sum(block2 > n1+.5)
  (i_1 + i_2) / (k * n)
}

eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic = tnn,R = R, sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}


set.seed(10101)
k <- 2
R <- 1500
m <- 100
n_1 <- n_2 <- 20
n <- n_1 + n_2 
N <- c(n_1, n_2)

#giving the parameters of two different distributions
mu_1 <- c(0,0)
mu_2 <- c(1,-1)
sigma_1 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
sigma_2 <- matrix(c(1,0,0,2), nrow = 2, ncol = 2)

p.values <- matrix(NA,m,3)

for(i in 1:m){
  data1 <- mvrnorm(n_1, mu_1, sigma_1)
  data2 <- mvrnorm(n_2, mu_2, sigma_2)
  data <- rbind(data1, data2)
  
  # using nn method
  p.values[i,1] <- eqdist.nn(data,N,k)$p.value
  # using energy method
  p.values[i,2] <- eqdist.etest(data,sizes=N,R=R)$p.value
  # using ball method
  p.values[i,3] <- bd.test(x=data1, y=data2, num.permutations=R, seed=i*2846)$p.value
}
alpha <- 0.05;
power <- colMeans(p.values < alpha)
power

## -----------------------------------------------------------------------------
n1 <- n2 <- 20
n <- n1+n2 
N = c(n1,n2)
k=3
R=1500
m=100
set.seed(10101)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  mydata1 <- as.matrix(rt(n1,1,2),ncol=1)
  mydata2 <- as.matrix(rt(n2,2,5),ncol=1)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*2846)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
mu_1 <- c(0,0)
mu_2 <- c(1,-1)
sigma_1 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
sigma_2 <- matrix(c(1,0,0,2), nrow = 2, ncol = 2)
n_1 = 10
n_2 = 100
n <- n_1+n_2 
N = c(n_1,n_2)
k <- 3
R <- 1500
m <- 100
set.seed(1234)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  mydata1 <- mvrnorm(n_1,mu_1,sigma_1)
  mydata2 <- mvrnorm(n_2,mu_2,sigma_2)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1, y=mydata2, num.permutations=R, seed=i*2846)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## ----warning=FALSE------------------------------------------------------------
set.seed(10101)
rw.Metropolis <- function(mu,lambda,sigma,x0,N){
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1,x[i-1],sigma)
    
      #用dcauchy来计算cauchy分布在给定点处的密度函数值
      if(u[i] <= (dcauchy(y,mu,lambda)/dcauchy(x[i-1], mu, lambda)))
        x[i] <- y
      else{
        x[i] <- x[i-1]
        k <- k+1
      }
  }
  return(list(x=x,k=k))
}

#给柯西分布的参数、提议分布的方差以及初始值x0赋值
mu <- 0; lambda <- 1
N <- 4000
sigma <- c(0.05,0.5,1,16)
x0 <- 15

#生成不同方差下的四个链
rw1 <- rw.Metropolis(mu,lambda, sigma[1], x0, N)
rw2 <- rw.Metropolis(mu,lambda, sigma[2], x0, N)
rw3 <- rw.Metropolis(mu,lambda, sigma[3], x0, N)
rw4 <- rw.Metropolis(mu,lambda, sigma[4], x0, N)

#计算每个链中待选点的拒绝率
print(c(rw1$k,rw2$k,rw3$k,rw4$k)/N)

#绘制四个链的轨迹图
#par(mfrow = c(2,2))
refline <- qcauchy(c(.025,.975), mu, lambda)
rw <- cbind(rw1$x,rw2$x,rw3$x,rw4$x)
for (j in 1:4) {
  plot(rw[,j],type='l',xlab=bquote(sigma==.(round(sigma[j],3))),
       ylab='X',ylim=range(rw[,j]))
  abline(h=refline)
}
par(mfrow=c(1,1))

#丢弃链的前1000个值
rw_new1 <- rw1$x[1001:4000]
rw_new2 <- rw2$x[1001:4000]
rw_new3 <- rw3$x[1001:4000]
rw_new4 <- rw4$x[1001:4000]

#计算理论及丢弃前1000个值后的四组链值的十分位数
probs <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
df <- qcauchy(probs,mu,lambda)
df1 <- quantile(rw_new1,probs)
df2 <- quantile(rw_new2,probs)
df3 <- quantile(rw_new3,probs)
df4 <- quantile(rw_new4,probs)

#绘制理论十分位数折线及四个链的十分位数点
plot(df~probs, pch = 21, col="black",xlab = "分位数",
     xlim = c(0.1,0.9), ylab="十分位数值", ylim = c(-5,5), 
     main="理论及四组链值的十分位数折线图",type = "l")
points(probs, df1, pch= 22, col="blue", cex=1)
points(probs, df2, pch= 23, col="red", cex=1)
points(probs, df3, pch= 24, col="pink", cex=1)
points(probs, df4, pch= 25, col="green", cex=1)

legend("topleft", inset=.05, c("理论","方差0.05","方差0.5","方差1","方差16"), 
       col=c("black","blue","red","pink","green"), 
       text.col=c("black","blue","red","pink","green"), 
       pch=c(21,22,23,24,25))

## -----------------------------------------------------------------------------
#initialize constants
set.seed(10101)
N <- 4000 #length of chain
burn_period <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
x <- y <- numeric(N)
#initialize the parameters
a <- 100
b <- 100
n <- 1000

#initialize the variate's value
X[1, 1] <- 100
X[1, 2] <- 0.5

#generate the chain
for (i in 2:N){
  x[i] <- X[i-1, 1]
  y[i] <- X[i-1, 2]
  X[i, 1] <- rbinom(1, n, y[i])
  X[i, 2] <- rbeta(1, x[i]+a, n-x[i]+b)
}
b <- burn_period + 1
m <- X[b:N,]

plot(m, main="", cex=.4, xlab="X",
ylab="Y", xlim=range(m[,1]), ylim=range(m[,2]))

## ----warning=FALSE------------------------------------------------------------
set.seed(10101)

Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var") #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/n) #upper variance est.
  r.hat <- v.hat / W #G-R statistic
  return(r.hat)
}

normal.chain <- function(mu, lambda, sigma, N, X1) {
  #generates a Metropolis chain for Normal(0,1)
  #with Normal(X[t], sigma) proposal distribution
  #and starting value X1
  x <- numeric(N)
  x[1] <- X1
  u <- runif(N)
  for (i in 2:N) {
    y <- rnorm(1,x[i-1],sigma)
      #用dcauchy来计算cauchy分布在给定点处的密度函数值
      if(u[i] <= (dcauchy(y,mu,lambda)/dcauchy(x[i-1], mu, lambda)))
        x[i] <- y
      else{
        x[i] <- x[i-1]
      }
  }
  return(x)
}

mu <- 0      #parameter of cauchy distribution
lambda <- .01  #parameter of cauchy distribution
sigma <- 1   #parameter of proposal distribution
k <- 4       #number of chains to generate
n <- 20000   #length of chains
b <- 2000    #burn-in length


#choose overdispersed initial values
x0 <- c(-5, 0, 10, 25)
#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- normal.chain(mu, lambda, sigma, n, x0[i])

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))

#plot psi for the four chains
#par(mfrow=c(2,2))
for (i in 1:k)
  plot(psi[i, (b+1):n], type="l", xlab=i, ylab=bquote(psi))
par(mfrow=c(1,1)) #restore default

#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")

#give the line of R_hat = 1.2
abline(h=1.2, lty=2)

## ----warning=FALSE,eval=FALSE-------------------------------------------------
#  library(R2OpenBUGS)
#  library(coda)
#  
#  set.seed(10101)
#  
#  N <- 4000 #length of chain
#  burn_period <- 1000 #burn-in length
#  X <- matrix(0, N, 2) #the chain, a bivariate sample
#  x <- y <- numeric(N)
#  #initialize the parameters
#  a <- 100
#  b <- 100
#  n <- 1000
#  
#  #initialize the variate's value
#  X[1, 1] <- 100
#  X[1, 2] <- 0.5
#  
#  #generate the chain
#  for (i in 2:N){
#    x[i] <- X[i-1, 1]
#    y[i] <- X[i-1, 2]
#    X[i, 1] <- rbinom(1, n, y[i])
#    X[i, 2] <- rbeta(1, x[i]+a, n-x[i]+b)
#  }
#  b <- burn_period + 1
#  m <- X[b:N,]
#  
#  R <- 10000
#  y <- m[,2]
#  data <- list ("N", "R", "y")
#  
#  inits <- function(){
#    list(theta = rnorm(N, 0, 100), mu.theta = rnorm(1, 0, 100),
#      sigma.theta = runif(1, 0, 100))
#  }
#  
#  model{
#    for (j in 1:R)
#    {
#      y[j] ~ dnorm (theta[j], tau.y[j])
#      theta[j] ~ dnorm (mu.theta, tau.theta)
#      tau.y[j] <- pow(sigma.y[j], -2)
#    }
#    mu.theta ~ dnorm (0.0, 1.0E-6)
#    tau.theta <- pow(sigma.theta, -2)
#    sigma.theta ~ dunif (0, N)
#  }
#  
#  
#  lattice::xyplot(myModel.coda[,2:4])
#  
#  plot(myModel.coda[,2:4])
#  
#  acfplot(myModel.coda[,2:4], lag.max=200)
#  gelman.plot(myModel.coda[,2:4])
#  

## -----------------------------------------------------------------------------

Euclid <- function(x){
  sqrt(sum(x^2))
}


term_k <- function(a, k){
  d <- length(a)
  (-1)^k/(factorial(k)*2^k) * (Euclid(a))^(2*k+2)/((2*k+1)*(2*k+2)) * gamma((d+1)/2)*gamma(k+1.5)/gamma(k+d/2+1) 
}




S <- function(a){
  sum <- 0
  k <- 0
  c <- 1
  while(abs(c) > 1e-100){
    #print(term_k(a, k))
    sum <- sum + term_k(a, k)
    k = k + 1
    c <- term_k(a, k)
  }
  return(sum)
}




a <- c(1,2)
S(a)

## -----------------------------------------------------------------------------

c_k <- function(a, k){
  sqrt(a^2*k / (k+1-a^2))
}


inted = function(k,u){
  (1+u^2/k)^(-(k+1)/2)
}


equation11_5 <- function(a, k){
   (log(k) - log(k-1))/2 + (2*lgamma(k/2) - lgamma((k+1)/2)- lgamma((k-1)/2)) + log(integrate(inted, 0, c_k(a, k-1), k = k-1)$value) - log(integrate(inted, 0, c_k(a, k), k = k)$value)
}


root11_5 <- sapply(c(4:25,100,500,1000), function(k){uniroot(equation11_5, interval = c(1, 2), k=k)$root})

#root11_5 <- sapply(c(4:25,100,500,1000),function(k){uniroot(equation11_5, interval = c(0, sqrt(k)))$root})


equation11_4 <- function(a, k){
  pt(c_k(a, k-1), df = k-1) - pt(c_k(a, k), df = k)
}

root11_4 <- sapply(c(4:25,100,500,1000), function(k){uniroot(equation11_4, interval = c(1, 2), k=k)$root})

#root11_4 <- sapply(c(4:25,100,500,1000),function(k){uniroot(equation11_5, interval = c(0, sqrt(k)))$root})


root <- cbind(root11_5, root11_4)
colnames(root) <- c("root in 11.5", "root in 11.4")
rownames(root) <- as.character(c(4:25,100,500,1000))

knitr::kable(root)

## -----------------------------------------------------------------------------
#初始化参数为1
lambda <- 1
i <- 1
while (i<10) {
  lambda <- (10*exp(lambda)-10)/(3.75*exp(lambda)+10)
  i = i+1
}
lambda 

## -----------------------------------------------------------------------------
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)
lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x = x)

## -----------------------------------------------------------------------------
#模型P204-3
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
#模型P204-4
rsq <- function(mod) summary(mod)$r.squared
#模型P204-5
a <- lapply(lapply(formulas, lm, data=mtcars), rsq)
#输出各个R平方
a

## -----------------------------------------------------------------------------
x<-data.frame(cbind(x1=seq(5,20,5),x2=c(1:4)))
vapply(x, sd, FUN.VALUE = c('a'=0))

## -----------------------------------------------------------------------------
x <- data.frame(a=seq(5,25,5), b=runif(5), c=c(TRUE,FALSE,TRUE,FALSE,TRUE))
sd_1 <- vapply(x, is.numeric, logical(1))
sd_2 <- vapply(x[sd_1], sd, FUN.VALUE = c('a'=0))
knitr::kable(sd_2)

## -----------------------------------------------------------------------------
#define the fuction of mcvapply
mcvapply <- function (cl = NULL, X, fun, ..., chunk.size = NULL) {
    cl <- defaultCluster(cl)
    nchunks <- staticNChunks(length(X), length(cl), chunk.size)
    do.call(c, clusterApply(cl = cl, x = splitList(X, nchunks), 
        fun = sapply, FUN = fun, ...), quote = TRUE)
}

