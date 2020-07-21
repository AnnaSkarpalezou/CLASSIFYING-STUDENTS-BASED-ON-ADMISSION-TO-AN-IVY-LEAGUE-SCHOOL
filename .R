View(df)
library(readr)
df <- read_csv("~/Desktop/datahw6.csv")
#
boxplot(GPA ~ Binary,xlab="Offered a place",ylab="GPA",data=df)
boxplot(SAT ~ Binary,xlab="Offered a place",ylab="SAT score",data=df)
boxplot(ACT ~ Binary,xlab="Offered a place",ylab="ACT score",data=df)
boxplot(RANK ~ Binary,xlab="Offered a place",ylab="Class rank, top %",data=df)

SAT<-factor(df$SAT)
ACT<-factor(df$ACT)
GPA<-factor(df$GPA)
RANK<-factor(df$RANK)
#
# Logistic regression is fit using the glm() function, telling R that the family is binomial. 
# R gives Wald test which using the drop1() command give likelihood ratios
#
R1 <- glm(Binary ~ GPA + SAT + ACT + RANK, family=binomial, maxit=500, data=df)
summary(R1)
cc <- R1$coefficients
(eqn <- paste("Y =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e"))
#
# This is the likelihood ratio test for ACI
#
gstat <- R1$null.deviance - deviance(R1)
cbind(gstat, 1-pchisq(gstat,length(coef(R1))-1))
#
# Odds ratios
#
exp(coef(R1)[-1])
#
# This is to get the 95% confidence interval for the odds ratio (exponentiating the ends of a confidence interval for the slope)
exp(confint.default(R1))
# The drop1() command gives the likelihood ratio tests for each slope, along with AIC values for the model that omits that variable
#
drop1(R1, test="LRT")
#
# The car package gives the (approximate) VIF values
library(car)
vif(R1)
#
AIC(R1)
#
# The ResourceSelection package gives the Hosmer-Lemeshow test.
#
library(ResourceSelection)
hoslem.test(df$Binary, fitted(R1))
#
# The rms library this test, appropriate for binary response data [0/1] without replications. 
# The function gives the summary measures of association, with Somers D being represented by Dxy
#
library(rms)
R1.lrm <- lrm(Binary ~ GPA + SAT + ACT + RANK, x=T, y=T)
residuals(R1.lrm, type="gof")
R1.lrm$stats
library(leaps)
leaps(cbind(GPA,SAT,ACT,RANK),Binary,nbest=2)
#
# Below we perform best subsets for generalized linear models (including logistic regression, allows for categorical data).
# The default criterion used is BIC, but AIC is requested instead.
# "family=binomial" specification 
#
library(bestglm)
logitbest <- bestglm(data.frame(cbind(df$GPA,df$SAT,df$ACT,df$RANK),df$Binary), IC="AIC", family=binomial)
logitbest$Subsets
residuals(R1.lrm, type="gof")
library(boot)
R1diag <- glm.diag(R1)
spearson1 <- residuals(R1, type="pearson")/sqrt(1-R1diag$h)
cbind(spearson1,R1diag$cook,R1diag2$h)
cbind(spearson1,R1diag$cook,R1diag$h)
library(bestglm)
logitbest <- bestglm(data.frame(cbind(df$GPA,df$SAT,df$ACT,df$RANK),df$Binary), IC="AIC", family=binomial)
logitbest$Subsets
#
R2 <- glm(Binary ~ GPA + SAT, family=binomial, maxit=500, data=df)
summary(R2)
gstat <- bank2$null.deviance - deviance(bank2)
cbind(gstat, 1-pchisq(gstat,length(coef(bank2))-1))
exp(coef(R2)[-1])
drop1(R2, test="LRT")
vif(R2)
hoslem.test(Bankrupt, fitted(R2))
R2.lrm <- lrm(Binary ~ GPA + SAT, x=T, y=T)
residuals(R2.lrm, type="gof")
R2.lrm$stats
#
# The glm.diag()[boot package] produces diagnostics, while the glm.diag.plots() function produces diagnostic plots. 
# The rp, h, and cook values are based on standardized Pearson residuals, leverage values, and Cook's distances, respectively. 
#
library(boot)
diag2 <- glm.diag(R2)
spearson2 <- residuals(R2, type="pearson")/sqrt(1-R2$h)
cbind(spearson2,R2$cook,R2$h)
plot(fitted(R2), rstandard(R2, type="pearson"), xlab="Estimated probabilities", ylab="Standardized Pearson resids")
#
# AIC prefers the two-predictor model.
#
# Classification table; the model classifies perfectly on these values, but don't forget that it got the outlier wrong
#
df.predict <- as.numeric(fitted(R2) > .5)
table(Binary, df.predict)
#
# To get prospective probability estimates
#
prosplogit <- predict(R2) + log((.1*25)/(.9*24))
prospprob <- exp(prosplogit)/(1 + exp(prosplogit))
prospprob
