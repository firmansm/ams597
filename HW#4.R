#HW 4 - Firman Firmansyah, 110945596

rm(list=ls())

#1 
#The leukemia gene expression dataset available in the url below consists of 72 subjects/patients and 3571 genes. http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/leukemiaDataSet.txt Each patient is of either type ALL (Acute lymphocytic leukemia) or type AML (Acute myelogenous leukemia). Using the genes as covariates, we will construct a model that can predict the two types of leukemia as follows: 1) First split the data randomly into two subsets containing 50 (trainData) and 22 (testData) subjects, respectively as follows:

dat <- read.delim('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/leukemiaDataSet.txt' ,header=T,sep='\t')

### please read this as a single line in R, I break this into
### 3 lines to avoid overflowing outside paper margin
str(dat)
set.seed(123)
trainID <- sample(1:72,round(0.7*72))
trainData <- dat[trainID,]
testData <- dat[-trainID,]

# b or 2) Build your best model on trainData using the Group variable as response and the genes as predictors/covariates.

#creating a loop for logistic regression for each predictor
pval <- list()
for(i in 2: length(trainData[1,])){
  allcandidates <- glm(Group ~ trainData[,i], data=trainData, family=binomial("logit"))
  pval[i] <- summary(allcandidates)$coefficients[8]
}

#retain genes with the smallest (N-1) p-value  or 50-1 = 49
pval_only <- as.data.frame(cbind(1:length(unlist(pval)), unlist(pval)))
colnames(pval_only) <- c("gene", "p_value")
selected_genes <- pval_only[order(pval_only$p_value),][1:49,]

#create a newTrainData with only selected genes
new_trainData <- trainData[,c(1,selected_genes$gene+1)]

#logistic regression with all best possible predictors
fitcandidates <- glm(Group ~ . , data=new_trainData, family=binomial("logit"))
summary(fitcandidates)

#step wise regression with BIC criteria
bestModel <- step(fitcandidates, direction="both", k=log(nrow(new_trainData)))
summary(bestModel)

#3) Evaluate your model from (2) on the testData by computing the percentage of AML correctly predicted, the percentage of ALL correctly predicted and the overall percentage of AML and ALL correctly predicted.
#current validation
proba <-  predict(bestModel, newdata=testData, type="response")
proba.pred <- rep("ALL", dim(testData)[1])
proba.pred[proba > .5] <- "AML"
conf_mat <- table(proba.pred, testData$Group)
conf_mat

#ALL correctly predicted
conf_mat[1,1]/sum(conf_mat[,1])
#87.5%

#AML correctly predicted
conf_mat[2,2]/sum(conf_mat[,2])
#100%

#overall AML and ALL correctly predicted
mean(proba.pred == testData$Group)
#90.9%

#2 Write a function that will generate and return a random sample of size n from the two parameter exponential distribution Exp(lambda, f) for arbitrary n, lambda, and v. 

#PDF for Exp (lambda, v)
#f(x) = lambda*e^-lambda(x-v)

#PDF for EXp (2,1)
#f(x) = 2e^(-2x+2)
#CDF = integral of f(x) = -e^(2-2x)
#inverse for the CDF = 1/2(2-log(-x)) or 1/2(2-log(1-x))

my_Exp <- function(lambda, v, n){
  uniexp <- runif(n)
  inv_funct <- 1/2*(2-log(1-uniexp))
  return(inv_funct)
}

tryExp <- my_Exp(2, 1, 1000)
par(mfrow=c(1,2))
hist(tryExp, main="Exp(2,1) distribution")
plot(density(tryExp), main="Exp(2,1) distribution")

#3 Write a function to generate a random sample of size n from the Beta(a; b) distribution by the acceptance-rejection method. Generate a random sample of size 1000 from the Beta(3; 2) distribution.

#pdf for beta (3,2) = 12x^2 - 12x^3, maksimal value when 24x-36x^2=0, is 16/9
#c = 16/9

#function beta(3,2) <- 12*x^2-12*x^3
#c <- x*16/9
#g(x) = 1
#threshold = (12*x^2-12*x^3) / (16/9) = 9/16*(12*x^2-12*x^3)

my_Bta <- function(n, a=3, b=2){
  n <- 1000
  k <- 0 #counter for number of accepted
  j <- 0 #counter for number of iterations
  x <- rep(NA,n)
  while(k<n){
    y <- runif(1) ###g, proposal distribution
    j <- j+1
    u <- runif(1) ###random number
    if(u<=9/16*(12*y^2-12*y^3)){
      k <- k+1
      x[k] <- y
    }
  }
  result <-  list("sample" = x, "accepted" = k, "iterations" = j)
  return (result)
}

my_B32 <- my_Bta(1000, 3, 2) #generate 1000 random samples from Beta (3,2) distribution
hist(my_B32$sample)

#compare with built-in function R
comp_B32 <- rbeta(1000,3,2)
hist(comp_B32)
qqplot(my_B32$sample, comp_B32)
abline(0,1,col='red')

# 4. Write a function to generate a random sample of size n from the Gamma(??; 1) distribution by the acceptance-rejection method. Generate a random sample of size 1000 from the Gamma(3; 1) distribution. (Hint: you may use g(x) ??? Exp(?? = 1=??) as your proposal distribution, where ?? is the rate parameter. Figure out the appropriate constant c).

#pdf for Gamma (3,1) = (x^2*e^-x)/2
#maksimal value for pdf when x = 2, 2/e^2
#c = 2/e^2

#g(x) = Exp(lambda = 1/alpha)
#rexp(1, 1/3)

my_Gam <- function(size, alpha, beta=1){
    x <- rep(NA, size)
    k = 0 
    m = alpha*(alpha-1)/(alpha+1)
    M = dgamma(m, alpha, 1)/dexp(m, rate = 1/alpha)
    j = 0 #iteration
    
    while (k < size) {
      y = runif(1)
      j = j + 1
      exp.point <- -alpha*log(runif(1))
      if (y*M < dgamma(exp.point, alpha, 1)/dexp(exp.point, rate = 1/alpha)){
        k = k+1
        x[k] <- exp.point
      }
    } 
    result <-  list("sample" = x, "accepted" = k, "iterations" = j)
    return (result)
}

Gam31 <- my_Gam(1000,3, 1)
hist(Gam31$sample)

#5. Using only random uniform generator, generate a random sample of size 1000 from the F distribution with 5 and 10 degrees of freedom.

#modify my gamma function

my_Gam_ext <- function(n, alpha){
  x <- rep(NA, n)
  if (alpha != 1){
    k = 1 
    m = alpha*(alpha-1)/(alpha+1)
    M = dgamma(m, alpha, 1)/dexp(m, rate = 1/alpha)
    j = 0
    while (k <= n) {
      y = runif(1)
      exp.point <- -alpha*log(runif(1))
      if (y*M < dgamma(exp.point, alpha, 1)/dexp(exp.point, rate = 1/alpha)){
        x[k] <- exp.point
        k = k+1
      }
      j = j + 1
    } 
  } 
  if (alpha == 1) {
    x = -alpha*log(runif(n))
  }
  return(x)
}

#generate F with df 5, 10
n_size = 1000
n_iter = 0
k = 1
my_F <- rep(NA, n_size)
while (k <= n_size) {
  x = runif(1)
  from_ga <- my_Gam_ext(1, 5/2)
  m = 1/2
  M = df(m, 5, 10)/dgamma(m, shape = 5/2, scale = 1)
  if (x*M < df(from_ga, 5, 10)/dgamma(from_ga, shape = 5/2, scale = 1)){
    my_F[k] <- from_ga
    k = k+1
  }
  n_iter = n_iter + 1
}

my_F
par(mfrow=c(1,2))
hist(my_F)
#compare with rf
hist(rf(1000, 5, 10))

