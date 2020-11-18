rm(list=ls())

# 1 

#(a) Use sample function to generate a random vector that follows a multinomial distribution with probability (0.1, 0.2, 0.4, 0.3).
vA <- sample(c("a", "b", "c", "d"), size=10000, replace=T, prob = c(0.1,0.2,0.4,0.3))
vA
prop.table(table(vA))

#(b) Using only random uniform generator (DO NOT use sample), generate a random vector that follows a multinomial distribution with probability (0.1, 0.2, 0.4, 0.3).
n <- 10000
vB <- character(n)
u <- runif(n)
vB[u<=0.1] <- "a"
vB[u>0.1 & u<=0.3] <- "b"
vB[u>0.3 & u<=0.7] <- "c"
vB[u>0.7] <- "d"
vB
prop.table(table(vB))

# 2 Generate 100 exponentially distributed random variables with rate 2, and plot their empirical distribution function.

v3 = rexp(n=100, rate=2)
round(v3,3)
lv3 = length(v3)
plot(sort(v3), (1:lv3)/lv3, type='s', ylim=c(0,1), col="red")

# 3 Use the following dataset to answer the questions. http://www.ams.sunysb.edu/???pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt

data_log_stock = read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header=T)
data_log_stock
attach(data_log_stock)
names(data_log_stock)
colMeans(data_log_stock[,2:6])

#(a) Perform a t-test for American Express with the null hypothesis that the mean of its log return is zero.
t.test(AmerExp)
#p-value (0.85) is higher than 0.05, therefore we failed to reject the null hypothesis. 

#(b) Perform a Wilcoxon signed-rank test for American Express with the null hypothesis that the mean of its log return is zero.
wilcox.test(AmerExp)
#p-value (0.32) is higher than 0.05, therefore we failed to reject the null hypothesis.

#(c) Perform a two-sample t-test to conclude if the mean log return of Pfizer and American Express are same or not.
t.test(Pfizer, AmerExp)
#p-value (0.32) is higher than 0.05, therefore we failed to reject the null hypothesis.

#(d) Compare the variance of log returns for Pfizer and American Express.
var.test(Pfizer, AmerExp)
#p-value (0.04) is lower than 0.05, therefore we can reject the null hypothesis, and conclude that there was a significant diffence in the variance of log return for Pfizer and American Express.

#(e) Perform a two-sample Wilcoxon test to conclude if the mean log returns of Pfizer and American Express are same or not.
wilcox.test(Pfizer, AmerExp)
#p-value (0.17) is higher than 0.05, therefore we failed to reject the null hypothesis.

# 4 Write your own function my.t.test which can perform both one and two sample t-test. For two sample t-test, it can perform both equal and unequal variance version. Your my.t.test will take the following argument (1) the vector x, (2) optional vector y if it is two sample t-test, (3) type of alternative hypothesis alternative, (4) the mean or mean difference that you are testing mu. Your function my.t.test should contain a routine to check for equal variance assumption using the F test. If the p-value of the F test is <= 0.05, then it will perform two sample t-test with unequal variance assumption. Your function my.t.test should return the test statistic stat, degrees of freedom df and p-value p.value.

my.t.test = function(x, y = NULL, my.alternative = c("two.sided", "less", "greater"), my.mu = 0) {
  if (!is.null(y)) {
    p.value = var.test(x, y)$p.value
    if (p.value >0.05) {
      my.test = t.test(x,y,alternative = my.alternative,mu = my.mu,var.equal = T)
      return(c(round(my.test$stat,4), round(my.test$parameter,4), round(my.test$p.value,4)))
    } else {
      my.test = t.test(x, y, alternative = my.alternative, mu = my.mu)
      return(c(round(my.test$stat,4), round(my.test$parameter,4), round(my.test$p.value,4)))
    }
  } else {
    my.test = t.test(x, alternative = my.alternative, mu = my.mu)
    return(c(round(my.test$stat,4), round(my.test$parameter,4), round(my.test$p.value,4)))
  }
}

#testing my.t.test function
my.t.test(Pfizer, AmerExp)
t.test(Pfizer, AmerExp)

my.t.test(Pfizer)
t.test(Pfizer)

# 5 A regression through the origin model may be used when specific knowledge about the problem at hand suggests that the response variable is zero if and only if the predictor variable is zero. For such problems, the model can be written as Yi = ??Xi + i; i = 1; ... n where  i's are iid N(0; ??2) random noise.

#(a) Derive the least-squares estimate of beta
# beta = sum(x*y)/sum(x^2)

#(b)
set.seed(123)
x <- rnorm(50)
y <- 2*x+rnorm(50)
beta = sum(x*y)/sum(x^2)
round(beta,3)
plot(x,y, type="p", pch=20)
lines(x,beta*x, col="red")

#(c)
fit <- lm(y~x-1)
fit
