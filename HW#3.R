#HW 3 - Firman Firmansyah, 110945596

rm(list=ls())

# 1 Use the following dataset to answer the questions. http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt

dat1 <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header=T)
View(dat1)
names(dat1)

#(a) Regress the return of Pfizer on the returns of Exxon and Citigroup (with intercept). Report the estimated coefficients

reg1a <- lm(dat1$Pfizer ~ dat1$Exxon + dat1$Citigroup)
summary(reg1a)
reg1a$coefficients
reg1a[[1]]

#(b) Use matlines to plot the fitted values and the corresponding confidence bands
conf_reg1a <- predict(reg1a, interval = "confidence")

par(mfrow=c(2,1))

plot(dat1$Exxon, dat1$Pfizer, xlab ="Returns of Exxon", ylab="Returns of Pfizer")
matlines(sort(dat1$Exxon), conf_reg1a[order(dat1$Exxon),], lty=c(1,3,3), col=c("blue", "red", "red"))

plot(dat1$Citigroup, dat1$Pfizer, xlab ="Returns of Citigroup", ylab="Returns of Pfizer")
matlines(sort(dat1$Citigroup), conf_reg1a[order(dat1$Citigroup),], lty=c(1,3,3), col=c("green", "red", "red"))

#if not using sort, it will begin with the first data point, which is 0.01
#if not using order, it will begin with the first data point. order will return the index of a vector

#(c) Generate an ANOVA table to conclude if regression effects are significant.
anova(reg1a)

#p values for both returns of Exxon and Citigroup are less than .05, therefore We can reject the null hypothesis and conclude that the regression effects are significant.

#(d) Regress the return of Pfizer on the returns of Exxon and Citigroup (without intercept). Report the estimated coefficients.
reg1d <- lm(dat1$Pfizer ~ dat1$Exxon + dat1$Citigroup - 1)
summary(reg1d)
reg1d$coefficients

#(e) Compute the correlation of Pfizer and Exxon, and test if their correlation is zero.
cor.test(dat1$Pfizer, dat1$Exxon)

#the correlation of Pfizer and Exxon is .35 and the p-value is less than .01. Therefore, we can reject the null hypothesis and conclude that the correlation is not equal to zero.

# 2 Consider the dataset in Problem 1, we now ignore the time series features of all returns, and consider them independent. We also treat all returns of 'Citigroup' and 'AmerExp' as Group 1, returns of 'Exxon' and 'GenMotor' as Group 2, and returns of 'Intel' as Group 3.

tail(dat1)
return <- c(dat1$Citigroup, dat1$AmerExp, dat1$Exxon, dat1$GenMotor, dat1$Intel)
groupre <- as.factor(rep(1:3, c(64*2,64*2,64)))
str(groupre)
typeof(groupre)
dat2 <-as.data.frame(cbind(return, groupre))
dat2$groupre <- as.factor(dat2$groupre)
str(dat2$groupre)
typeof(dat2)
dat2

#(a) Perform one-way ANOVA for Groups 1 and 2
ano2a <- aov(dat2$return[1:256] ~ dat2$groupre[1:256])
summary(ano2a)

#the p value is higher than .05, we failed to reject the null hypothesis and conclude that means of group 1 and 2 are not different significantly.

#(b) Perform one-way ANOVA for Groups 1-3
ano2b <- aov(dat2$return[c(1:128,257:320)] ~ dat2$groupre[c(1:128, 257:320)])
summary(ano2b)

#the p value is higher than .05, we failed to reject the null hypothesis and conclude that means of group 1 and 3 are not different significantly.

# 3 Using the ChickWeight dataset in R,
head(ChickWeight)
dat3 <- ChickWeight
str(dat3)
unique(dat3$Time)
unique(dat3$Diet)

#(a) Perform a two way ANOVA comparing the weights to Time and Diet.
ano3a <- aov(dat3$weight ~ as.factor(dat3$Time) + dat3$Diet)
summary(ano3a)

#the p values for both time and diet are less than .01. We can reject the null hypothesis and conclude that at least one mean for groups based on the time and diet is different signigicantly

#(b) For subset of Time 2, perform a one way ANOVA comparing the weights to Diet. If necessary, perform the post hoc analysis (all pairwise comparisons) to identify which diet is different.
dat3$weight[dat3$Time==2]
dat3$Diet[dat3$Time==2]

ano3b <- aov(dat3$weight[dat3$Time==2] ~ dat3$Diet[dat3$Time==2])
summary(ano3b)

#the p value for the one anova is less than .05, we can reject the null hypothesis and conclude that at least one mean of the weights based on diet category is different signficantly

pairwise.t.test(dat3$weight[dat3$Time==2], dat3$Diet[dat3$Time==2], p.adj="bonf")
TukeyHSD(ano3b)

#the post hoc analysis showed that the only different (p < .05) is between type 1 and type 4 diet

#4. Consider the dataset in Problem 1, perform the following test.
dat4 <- dat1
dat4

#(a) Test if the proportion of positive returns of Pfizer is 0.55.

pro4a <- binom.test(length(dat4$Pfizer[dat4$Pfizer > 0]), length(dat4$Pfizer), p=0.55)
pro4a

#based on the binomial test, the probability that the returns of Pfizer are positive is 0.55, is lower than <.05, therefore we can reject the null hypothesis and conclude that the proportion of the returs is not equal to 0.55
    
#(b) Test if the proportion of positive returns of Intel is larger than 0.55.
pro4b <- binom.test(length(dat4$Intel[dat4$Intel > 0]), length(dat4$Intel), p=0.55, alternative="greater")

pro4b

#the p value is higher than .05 so we failed to reject the null hypothesis (the proportion is not greater than 0.55) and did not have strong evidence to accept the alternative hypothesis (the proportion is greater than 0.55).

#(c) Test if the proportions of positive returns of Pfizer and Intel are same.
pro4c <- prop.test(x = c(length(dat4$Pfizer[dat4$Pfizer > 0]), length(dat4$Intel[dat4$Intel > 0])), n = c(length(dat4$Pfizer), length(dat4$Intel)))
pro4c

#the p value is higher than .05 so we failed to reject the null hypothesis that the proportion of positive returns of Pfizer and Intel are same / not significantly different

#(d) We treat all returns of Citigroup and AmerExp as Group 1, returns of Exxon and GenMotor as Group 2, and returns of Intel as Group 3. We also consider the following 4 ranges of their returns r: r < ???0.1, ???0.1 ??? r < 0, 0 ??? r < 0.1, r ??? 0.1. Use chi-square test to conclude if the group and return range effects are independent.
gr4d_1 <- c(dat4$Citigroup, dat4$AmerExp)
gr4d_2 <- c(dat4$Exxon, dat4$GenMotor)
gr4d_3 <- c(dat4$Intel)

d_Group1 <- c(length(gr4d_1[gr4d_1 < -.1]), length(gr4d_1[gr4d_1 >= -.1 & gr4d_1 <0]), length(gr4d_1[gr4d_1 >= 0 & gr4d_1 <0.1]), length(gr4d_1[gr4d_1 >= .1]))
d_Group2 <- c(length(gr4d_2[gr4d_2 < -.1]), length(gr4d_2[gr4d_2 >= -.1 & gr4d_2 <0]), length(gr4d_2[gr4d_2 >= 0 & gr4d_2 <0.1]), length(gr4d_2[gr4d_2 >= .1]))
d_Group3 <- c(length(gr4d_3[gr4d_3 < -.1]), length(gr4d_3[gr4d_3 >= -.1 & gr4d_3 <0]), length(gr4d_3[gr4d_3 >= 0 & gr4d_3 <0.1]), length(gr4d_3[gr4d_3 >= .1]))

matr4d <- rbind(d_Group1, d_Group2, d_Group3)
colnames(matr4d) <- c("(r<-.1)", "(-.1<=r<0)", "(0<=r<.1)", "(r>=.1)")
matr4d

chisq.test(matr4d)
#p value is less than .05. we can reject the null hypothesis that the group and return range are independent and conclude that the group and the return range are not independent

#5 We will use the mcycle data in the MASS package for this problem. The data set consists of two variables, namely acceleration and measurement times from a simulated motorcycle accident. Our objective is to investigate the relationship between this two variables. Using acceleration as dependent variable and times as covariate, fit your \best" polynomial regression model to describe the relationship between this two variables. Check for the assumptions and justify how you choose the final model.

library(MASS)
library("lmtest")
attach(mcycle)

head(mcycle)
par(mfrow=c(1,1))
plot(mcycle)

# MODEL A KEEPING ALL LOWER ORDER TERMS

modelnyaPOLY_A <- list()
for(i in 1:50){
  modelnyaPOLY_A[[i]] = lm(accel ~ poly(times, i, raw=TRUE))
}

summaryPOLY_A <- lapply(modelnyaPOLY_A, function(x) summary(x))

#BIC of all polynomial models
BIC_POLY_A <- lapply(modelnyaPOLY_A, function(x) BIC(x))

#Best models based on BIC
summaryPOLY_A[[which.min(BIC_POLY_A)]]
bestPOLY_A <- modelnyaPOLY_A[[which.min(BIC_POLY_A)]]

#Checking assumptions
par(mfrow=c(3,2))

plot(bestPOLY_A)
plot(times, accel, main="Observed Values")
plot(times, bestPOLY_A$fitted.values, ylab="predicted values (accel)", main="Predicted Values")
mtext("Type A - Keeping All Lower Order Terms", side=3, line=-2, outer=T)

shapiro.test(bestPOLY_A$residual)
#p value is lower than .05, we can reject the null hypothesis that the residual is normally distributed
bptest(bestPOLY_A)
#p value is lower than .05, we can reject the null hypothesis and concluded that the data does not have constant variance (heteroskedasticity)

## MODEL B - Excluding lower order terms

dataformodelB <- as.data.frame(accel)
for (i in 1: 50) {
  dataformodelB <- cbind(dataformodelB, (times)^i)
  colnames(dataformodelB)[i+1] = paste0("times^", i)
}
head(dataformodelB)

allmodel_B <- lm(accel~., data=dataformodelB)

#step wise regression with BIC criteria
bestPOLY_B <- step(allmodel_B, direction="both", k=log(nrow(dataformodelB)))

#Best models based on BIC
summary(bestPOLY_B)

#Checking assumptions
par(mfrow=c(3,2))

plot(bestPOLY_B)
plot(times, accel, main="Observed Values")
plot(times, bestPOLY_B$fitted.values, ylab="predicted values (accel)", main="Predicted Values")
mtext("Type B - Excluding Lower Order Terms", side=3, line=-2, outer=T)

shapiro.test(bestPOLY_B$residual)
#p value is higher than .05, we failed to reject the null hypothesis that the residual is normally distributed
bptest(bestPOLY_B)
#p value is lower than .05, we can reject the null hypothesis and concluded that the data does not have constant variance (heteroskedasticity)

anova(bestPOLY_B, bestPOLY_A)

#I came up with two models using BIC criteria, A (keeping all lower order terms) and B (excluding lower order terms). Model A is more intepretable. Model B is more accurate with higher adjusted R squared even though these two models are not different significantly (p for anova > .05). For the assumptions, model B is better because its residual is normally distributed (based on shapiro test). 

#6 Read the following data into R http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/HW3Qn6Data.txt The HW3Qn6Data.txt dataset consists of n = 200 (sample size), a response variable y and 6 covariates x1,...,x6. Using the best subset selection method, i.e., consider all possible combination of covariates (y???1, y???x1, y???x2, ..., y???x1+x2, ..., y???x1+x2+x3+x4+x5+x6), choose the best model based on the BIC criterion. Write down your final model.

dat6 <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/HW3Qn6Data.txt", header=T)
dat6
head(dat6)
tail(dat6)

#each variable has 2 possibilities: TRUE -> included, FALSE -> excluded
v <- c(T, F)
#make all possible combination of 6 variables
dataV <- as.data.frame(replicate(6,v))
PossComb <- expand.grid(dataV)

#change the columns names to x1: x6
names(PossComb) <- paste("x", 1:6, sep="")
#set the columns names as the predictors
predictors <- names(PossComb)

#create all possible model using apply
AllPossibleModel <- apply(PossComb, MARGIN=1, function(x) as.formula(paste(c("y ~ 1", predictors[x]), collapse=" + ")))

#implement regression using lapply and assign to results
results_APM <- lapply(AllPossibleModel, function(x) lm(x, data=dat6))

#summary of the all models
summaryAPM <- lapply(results_APM, function(x) summary(x))

#BIC of all models
BIC_APM <- lapply(results_APM, function(x) BIC(x))

#Best models based on BIC
summaryAPM[[which.min(BIC_APM)]]

#So the final model based on the BIC criterion is y~x3
bestMod6 <- lm(dat6$y ~ dat6$x3)
summary(bestMod6)

#checking assumptions
#it's not being asked, but I think worth doing
par(mfrow=c(2,2))
plot(bestMod6)
shapiro.test(bestMod6$residual)
#p value is lower than .05, we can reject the null hypothesis that the residual is normally distributed
bptest(bestMod6)
#p value is lower than .05, we can reject the null hypothesis and concluded that the data does not have constant variance (heteroskedasticity)

#unfortunately the best model based on the BIC criterion only seems not meeting the regression assumptions (such as normality of residuals, homoscedaticity)