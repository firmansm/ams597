#Firman Firmansyah    110945596 
#HW 6

rm(list=ls())

#1 
#Compute a Monte Carlo estimate of
n <- 1000000
ul <- pi/3
ll <- 0
sin_v <- sin(runif(n, min = ll, max = ul))
mean(sin_v)*ul

#compare your estimate with the exact value of the integral
(-cos(ul))-(-cos(ll))

#2
#a Compute a Monte Carlo estimate (^ !) of ! by sampling from Uniform(0, 0.5), and estimate the variance of ^ !.
n <- 1000000
ul <- 0.5
ll <- 0
exp_v <- exp(-runif(n, min =ll, max = ul))
omega_hat <- mean(exp_v)*ul
var_omega_hat <- var(exp_v*ul)*(n-1)/n

#b Compute a Monte Carlo estimate (!???) of ! by sampling from the exponential distribution, and estimate the variance of !???.
exp_b <- rexp(n)
exp_b <- exp_b[exp_b <= ul]
omega_star <- rep(1-exp(ul), n)
var_omega_star <- var(omega_star)

#compare it with the exact value of the integral
exp(ll) - (1/exp(ul))
omega_hat

#c
#the omega star's variance is smaller
var_omega_hat > var_omega_star

#3
#a Generate X1; : : : ; X20 from N(0; 1). Consider testing H0 : µ = 0 vs H1 : µ 6= 0. Compute the p-value from (1) one sample t-test and (2) exact wilcoxon signed rank test. Repeat this process 1000 times. Estimate the empirical Type I error for both tests at ?? = 0:05.

x_norm <- rnorm(20)
pv_t <- mean(x_norm)*sqrt(20)/sd(x_norm)
pv_w <- wilcox.test(x_norm)$statistic

n <- 1000
X_norm <- matrix(rnorm(20*n), ncol=20)
t_v <- apply(X_norm, MARGIN = 1, function(x) mean(x)*sqrt(20)/sd(x))
w_v <- apply(X_norm, MARGIN = 1,  function(x) wilcox.test(x)$statistic)

t_pv <- sum(abs(t_v) >= abs(pv_t))/n
w_pv <- sum(abs(w_v-20*(20+1)/4) >= abs(pv_w-20*(20+1)/4))/n

t_pv
w_pv

#p value bases on distribution of stats
pt(abs(pv_t), lower.tail = FALSE, df=19)*2
wilcox.test(x_norm)$p.value

#b Now generate X1; : : : ; X20 from N(0:5; 1). Consider testing H0 : µ = 0 vs H1 : µ 6= 0. Compute the p-value from (1) one sample t-test and (2) exact wilcoxon signed rank test. Repeat this process 1000 times. Estimate the empirical power for both tests at ?? = 0:05.

y_norm <- rnorm(20, mean= 0.5, sd=1)
pv_tb <- mean(y_norm)*sqrt(20)/sd(y_norm)
pv_wb <- wilcox.test(y_norm)$statistic

n <- 1000
Y_norm <- matrix(rnorm(20*n), ncol=20)
tb_v <- apply(Y_norm, MARGIN=1, function(x) mean(x)*sqrt(20)/sd(x))
wb_v <- apply(Y_norm, MARGIN=1, function(x) wilcox.test(x)$statistic)

tb_pv <- sum(abs(tb_v) >= abs(pv_tb))/n
wb_pv <- sum(abs(wb_v-20*(20+1)/4) >= abs(pv_wb-20*(20+1)/4))/n

tb_pv
wb_pv

#theoretical pvalue
pt(abs(pv_tb), lower.tail=FALSE, df=19)*2
wilcox.test(y_norm)$p.value

#power
cL_tt <- quantile(tb_v, 0.025)
cU_tt <- quantile(tb_v, 0.975)
cL_wt <- quantile(wb_v, 0.025)
cU_wt <- quantile(wb_v, 0.975)
pow_tt = rep(NA, n)
pow_wt = rep(NA, n)

for (i in 1:n){
  z = rnorm(20, 0.5, 1)
  pow_tt[i] <- mean(z)*sqrt(20)/sd(z)
  pow_wt[i] <- wilcox.test(z)$statistic
}

#power t-test
sum(pow_tt >= cU_tt | pow_tt <= cL_tt)/n
#power wilcoxon test
sum(pow_wt >= cU_wt | pow_wt <= cL_wt)/n

#4 Estimate the standard error and bootstrap t-interval of the sample median for x where,  set.seed(123),  x <- rnorm(50)

set.seed(123)
x <- rnorm(50)
median(x)

library(boot)

#estimate standar error

median_f <- function(x,i){median(x[i])}
b_median <- boot(x,median_f,R=50)
b_median

#t interval
median_f2 <- function(x,i){
  m <- median(x[i])
  v <- var(boot(data=x[i],statistic=median_f,R=200)$t)
  c(m,v)
}
B <- 1000
my_boot_out <- boot(data=x,statistic=median_f2,R=B)
b_ci_median <- boot.ci(my_boot_out,type='stud')
b_ci_median


#5 Implement the bivariate Spearman rank correlation test as a permutation test. Compare the achieved significance level of the permutation test with the p-value reported by or.test on the following samples: set.seed(123), x <- rnorm(50), y <- 0.2*x+rnorm(50)

set.seed(123)
x <- rnorm(50)
y <- 0.2*x+rnorm(50)

r_base <- cor(x,y, method= 'spearman')
xy <- c(x,y)
M <- 1000
cor_v <- rep(NA, M)
for (m in 1:M){
  x_i <- sample(1:100, 50, replace=F)
  cor_v[m] <- cor(xy[x_i], xy[-x_i], method='spearman')
}

#the achieved significant level of the permutation test
sum(abs(cor_v)>= abs(r_base))/M

#p value reported by cor test
cor.test(x, y, method = 'spearman', exact=T)$p.value
