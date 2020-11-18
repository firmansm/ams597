#Firman Firmansyah  110945596

rm(list=ls())

#1
my_rand <- function(n){
  k <- 0 #counter for number of accepted
  j <- 0 #counter for number of iterations
  x_ran <- rep(NA, n/2)
  y_ran <- rep(NA, n/2)
  
  while(k<(n/2)){
    u1 <- runif(1) ###g
    u2 <- runif(1) ###g
    j <- j+1
    v1 <- 2*u1-1
    v2 <- 2*u2-1
    s <- v1^2+v2^2
    if(s<=1){
      k <- k+1
      x_ran[k] <- sqrt(-2*log(s)/s)*v1
      y_ran[k] <- sqrt(-2*log(s)/s)*v2
    }
  }
  z <- c(x_ran, y_ran)
  return(z)
}

ran_1 <- my_rand(10000)
length(ran_1)
plot(density(ran_1))
qqplot(ran_1, rnorm(10000))


#2
#using the previous random normal generator
Z <- my_rand(100)

#create function to generate chi2 distribution
my_chi2 <- function(n, k){
  x <- my_rand(k*n)
  w_mat <- matrix(x[1:(k*n)], ncol=k)
  w <- rowSums(w_mat^2)
  return(w)
}

#testing the chi2 distribution generator
w1 <- my_chi2(1000, 7)
w2 <- rchisq(1000, 7)
qqplot(w1, w2)
hist(w1)
hist(w2)

#create random t distribution generator
my_t <- function(n, df){
  Z <- my_rand(n)
  W <- my_chi2(n, df)
  t <- Z/sqrt(W/df)
  return(t)
}

#testing the random t distribution function
t1 <- my_t(1000, 10)
t2 <- rt(1000, 10)
hist(t1)
hist(t2)
qqplot(t1,t2)

#generate mixture t ditribution
Y <- sample(1:3,100,prob=c(0.3,0.35,0.35),replace=TRUE)
X <- rep(NA,100) ### this will store the random variable from the mixture distribution
X[Y==1] <- my_t(length(which(Y==1)), 3)
X[Y==2] <- my_t(length(which(Y==2)), 5)
X[Y==3] <- my_t(length(which(Y==3)), 7)
plot(density(X))

#3
rmultivarNorm <- function(n, mu=c(0,0,0), Sigma=c(1,1,1,1,1,1,1,1,1)){
  Sig <- matrix(Sigma,ncol=length(mu), nrow=length(mu))
  evD <- eigen(Sig,symmetric=TRUE)
  Lamda.mat <- diag(evD$values)
  P <- evD$vectors
  Q <- P%*%sqrt(Lamda.mat)%*%t(P)
  
  d <- length(mu)
  Z <- matrix(my_rand(n*d),nrow=n)
  J <- matrix(1,nrow=n,ncol=1)
  X <- Z%*%Q+J%*%mu
  return(X)
}

XYZ <- rmultivarNorm(1000, c(0,3,0), c(3,1,1,1,3,1,1,1,3))
XYZ2 <- rmultivarNorm(1000, c(0,3), c(2,0,0,2))
### checking the estimated mean ###
apply(XYZ,2,mean)
cov(XYZ)
apply(XYZ2,2,mean)
cov(XYZ2)

#4
my.ls<- function(y=rnorm(100), num_pred=rnorm(100), cat_pred=gl(4,25)){
#create dummy variables  
  x_cat <- cat_pred
  dummy <- matrix(0, nrow=length(x_cat), ncol=length(unique(x_cat)))
  for (i in 1: ncol(dummy)){
    dummy[,i] <- as.numeric(x_cat==i)
  }
  colnames(dummy) <- paste(deparse(substitute(cat_pred)),levels(x_cat), sep="")
#create intercept
  Intercept <- rep(1, length(num_pred))
#create the matrix and solve it
  matX <- cbind(Intercept, num_pred, dummy[,-1])
  colnames(matX)[colnames(matX)=="num_pred"] <- deparse(substitute(num_pred))
  coefficient <- solve(t(matX)%*%matX)%*%t(matX)%*%y
  coef <- as.matrix(coefficient)
  return(t(coef))
  }

my.fit <- my.ls(ChickWeight$weight, ChickWeight$Time, ChickWeight$Diet)
my.fit

fit <- lm(ChickWeight$weight ~ ChickWeight$Time + ChickWeight$Diet)
fit$coef

#5
#with the help of the TA 
p0 <- 0
x <- c(6544, 2008, 1448)
p1 <- 1
while(p0!=p1){
    p0 <- p1
    z2 <- x[1]*(p0/(p0+2))
    p1 <- (z2+x[3])/(z2+x[3]+x[2])
  }
p = p1
p1
