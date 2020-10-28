### import libraries requrired for analysis
library("MASS")
library("leaps")
library("car")
library("corrgram")
library("het.test")
library("stats")
library("coefplot")
library("lmtest")
library("lm.beta")
library("gvlma")
library("sjPlot")
library("apsrtable")
library("memisc")
library("calibrate")
library("stargazer")

### import data and bind variable names
setwd("~/Desktop/comps")
data<- read.csv("data.csv")
attach(data)
names(data)


### create data frames and descriptive statistics
yvals<- data.frame(diff1116, pctjobgrowth, jobsper10k)
xvals<- data.frame(diff1116, diffpart1116, expanded, ue2011, bachormore, catoregrank, stlocper10k, topcorp, topinctax)

stargazer(xvals, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")
stargazer(yvals, type = "text", title="Descriptive statistics", digits=1, out="table2.txt")

### create new variables, natural log of diff1116 and squared hsormore
logdiff1116<- log(diff1116)
hssq<- hsormore*hsormore

### correlations between dependent variables, tables and graphics
yvalcor<- cor(yvals)
sjt.corr(yvals)
corrplot(yvalcor, method="square")

### plot of independent variables against dependent variable
plot(diffpart1116, diff1116, main="Difference in UE vs Difference in Participation")
abline(lm(diff1116~diffpart1116))
plot(expanded, diff1116, main="Difference in UE vs Expanded")
abline(lm(diff1116~expanded))
plot(ue2011, diff1116, main="Difference in UE vs Unemployment 2011")
abline(lm(diff1116~ue2011))
plot(bachormore, diff1116, main="Difference in UE vs College Education")
abline(lm(diff1116~bachormore))
mtext("Independent Variables", outer=TRUE, cex =1.5)
plot(catoregrank, diff1116, main="Difference in UE vs Cato Regulator Rank")
abline(lm(diff1116~catoregrank))
plot(stlocper10k, diff1116, main="Difference in UE vs State and Local Employment")
abline(lm(diff1116~stlocper10k))
plot(topcorp, diff1116, main="Difference in UE vs Corporate Taxes")
abline(lm(diff1116~topcorp))
plot(topinctax,diff1116, main="Difference in UE vs Income Taxes")
abline(lm(diff1116~topinctax))
mtext("Independent Variables", outer=TRUE, cex =1.5)

### final models, tables and plots  --- other models omitted
mod1<-lm(diff1116 ~ diffpart1116 + expanded + ue2011 + bachormore + catoregrank + stlocper10k + topcorp + topinctax)
summary(mod1)
plot(mod1)

mod2<-lm(diff1116 ~ diffpart1116 + expanded + ue2011 + stlocper10k + topcorp + topinctax + pctparticipateavg)
summary(mod2)
plot(mod2)

mod3<-lm(diff1116 ~ diffpart1116 + expanded + ue2011 + stlocper10k + topcorp + pctparticipateavg)
summary(mod3)
plot(mod3)

mod4<-lm(diff1116 ~ expanded + ue2011 + stlocper10k + topcorp + topinctax +pctparticipateavg)
summary(mod4)
plot(mod4)


###model statistics, combined and seperate
stargazer(mod1,mod3,mod2,mod4, type = "text", title="Descriptive statistics", digits=1, out="gazerdesc.txt")
sjt.lm(mod1,mod2, depvar.labels = c("Model 1", "Model 2"), show.aic = TRUE)
sjt.lm(mod3,mod4, depvar.labels = c("Model 3", "Model 4"), show.aic = TRUE)
sjp.lm(mod1)
sjp.lm(mod2)
sjp.lm(mod3)
sjp.lm(mod4)

###plot comparing models
multiplot(mod1,mod2,mod3,mod4, intercept=FALSE)

###fitted values
fit1<- fitted(mod1)
fit2<- fitted(mod2)
fit3<- fitted(mod3)
fit4<- fitted(mod4)

###plot of fitted values vs actual from all models

fva1<- plot(fit1,diff1116, main="Fitted vs Actual Model 1")
abline(lm(diff1116~fit1))
fva1<- plot(fit2,diff1116, main="Fitted vs Actual Model 2")
abline(lm(diff1116~fit2))
fva1<- plot(fit3,diff1116, main="Fitted vs Actual Model 3")
abline(lm(diff1116~fit3))
fva1<- plot(fit4,diff1116, main="Fitted vs Actual Model 4")
abline(lm(diff1116~fit4))
mtext("Fitted vs. Actual", outer=TRUE, cex =1.5)

###comparison of model fit using AIC and BIC
aicscores<- AIC(mod1, mod2, mod3, mod4)
bicscores<- BIC(mod1, mod2, mod3, mod4)


###residuals, calculated and plotted
res1<- residuals(mod1)
plot(res1, main="Residuals Model 1")
abline(h=0)
res2<- residuals(mod2)
plot(res2, main="Residuals Model 2")
abline(h=0)
res3<- residuals(mod3)
plot(res3, main="Residuals Model 3")
abline(h=0)
res4<- residuals(mod4)
plot(res4, main="Residuals Model 4")
abline(h=0)

### histograms of residuals, all models (10 and 15 bins)
par(mfrow=c(2,2), oma=c(0,0,2,0))
hist(res1, breaks=10, main="Mod1")
hist(res2, breaks=10, main="Mod2")
hist(res3, breaks=10, main="Mod3")
hist(res4, breaks=10, main="Mod4")
mtext("Histograms of Residuals, 10 Bins", outer=TRUE, cex =1.5)

hist(res1, breaks=15, main="Mod1")
hist(res2, breaks=15, main="Mod2")
hist(res3, breaks=15, main="Mod3")
hist(res4, breaks=15, main="Mod4")
mtext("Histograms of Residuals, 15 Bins", outer=TRUE, cex =1.5)

### shapiro-wilks test of normality, applied to residuals

shapiro.test(res1)
shapiro.test(res2)
shapiro.test(res3)
shapiro.test(res4)

### plots of residuals from mod1 and mod2 on x vlaues

par(mfrow=c(3,1))
plot(res1, diffpart1116, main="Residuals vs Difference in Participation")
plot(res1, expanded, main="Residuals vs Expanded")
plot(res1, ue2011, main="Residuals vs Unemployment 2011")
plot(res1, bachormore, main="Residuals vs College Education")
plot(res1, catoregrank, main="Residuals vs Cato Reglatory Rank")
plot(res1, stlocper10k, main="Residuals vs State and Local Employment")
plot(res1, topcorp, main="Residuals vs Corporate Taxes")
plot(res1, topinctax, main="Residuals vs Income Taxes")
plot(res1, pctparticipateavg, main="Residuals vs Average Participation")
plot(res2, diffpart1116)
plot(res2, expanded)
plot(res2, ue2011)
plot(res2, bachormore)
plot(res2, catoregrank)
plot(res2, stlocper10k)
plot(res2, topcorp)
plot(res2, topinctax)
plot(res2, pctparticipateavg)

### plot of studentized residuals and squared residuals from mod1 and 2, testing for outliers
par(mfrow=c(2,2), oma=c(0,0,2,0))
stres1<- studres(mod1)
plot(stres1, main="Mod1")
stres2<- studres(mod2)
plot(stres2, main="Mod2")
stres3<- studres(mod3)
plot(stres3, main="Mod3")
stres4<- studres(mod4)
plot(stres4, main="Mod4")
mtext("Studentized Residuals", outer=TRUE, cex =1.5)


ressq1<- res1*res1
plot(ressq1, main="Squared Residuals Model 1")
ressq2<- res2*res2
plot(ressq2, main="Squared Residuals Model 2")
ressq3<- res3*res3
plot(ressq3, main="Squared Residuals Model 3")
ressq4<- res4*res4
plot(ressq4, main="Squared Residuals Model 4")

### residual plot
par(mfrow(2,2))
plot(fit1, res1, main="Residual Plot Model 1")
abline(h=0)
plot(fit2, res2, main="Residual Plot Model 2")
abline(h=0)
plot(fit3, res2, main="Residual Plot Model 3")
abline(h=0)
plot(fit4, res2, main="Residual Plot Model 4")
abline(h=0)
mtext("Fitted vs Residuals", outer=TRUE, cex =1.5)


###qqplot testing for nomrality of residuals, with and without 95% confidence bands
q1<-qqPlot(mod1, envelope = .95)
title("Mod1")
q2<-qqPlot(mod2, envelope = .95)
title("Mod2")
q5<-qqPlot(mod3, envelope = .95)
title("Mod3")
q6<-qqPlot(mod4, envelope = .95)
title("Mod4")
mtext("Q-Q Plots", outer=TRUE, cex =1.5)
q1<-qqPlot(mod1, envelope=FALSE)
title("Mod1")
q2<-qqPlot(mod2, envelope=FALSE)
title("Mod2")
q5<-qqPlot(mod3, envelope=FALSE)
title("Mod3")
q6<-qqPlot(mod4, envelope=FALSE)
title("Mod4")
mtext("Q-Q Plots", outer=TRUE, cex =1.5)

### correlation between independent variables, inspected for multicollinearity
cor1<- cor(xvalsreduced)
corrplot(cor1, method="color")

### variance inflation factor test for multicollinearity, printed and plotted
vif1<-vif(mod1)
vif2<-vif(mod2)
vif3<-vif(mod3)
vif4<-vif(mod4)
vif1
vif2
vif3
vif4
par(mfrow=c(2,2), oma=c(0,0,2,0))

plot(v1)
title("Mod1")
plot(v2)
title("Mod2")
plot(v3)
title("Mod3")
plot(v3)
title("Mod4")
mtext("Variance Inflation Factor", outer=TRUE, cex =1.5)

### Breusch-Pagan test for heteroskedasticity
bptest(mod1)
bptest(mod2)
bptest(mod3)
bptest(mod4)

### RESET test for functional form
reset(mod1)
reset(mod2)
reset(mod3)
reset(mod4)

### hat values, testing for leverage, models 1 and 2
### decision rule for small sample is 3x mean of hat
### indicates 2(AK), 32(NY), and 50(WY) have high leverage

hatmod1<- hatvalues(mod1)
hatmod1
summary(hatmod1)
meanhatmod1<- mean(hatmod1)
meanhatmod1
meanhatmod13<- meanhatmod1*3
meanhatmod13
plot(hatmod1, main="Hat Values Mod1")
text(index, hatmod1,labels=state)
abline(h=meanhatmod13)
hatmod2<- hatvalues(mod2)
hatmod2
summary(hatmod2)
meanhatmod2<- mean(hatmod2)
meanhatmod2
meanhatmod23<- meanhatmod2*3
meanhatmod23
plot(hatmod2, main="Hat Values Mod2")
text(index, hatmod2, labels=state)
abline(h=meanhatmod23)

### cooks d, testing for influence
### decision rule is 4/(n-p) 8=.095,  7=.093, 6= .091, 5= .889
### no influence from outliers and high leverage instances
par(mfrow=c(2,2), oma=c(0,0,2,0))
cookmod1<-cooks.distance(mod1)
plot(cookmod1, main="Mod1")
abline(h=.889)
cookmod2<-cooks.distance(mod2)
plot(cookmod2, main="Mod2")
abline(h=.889)
cookmod3<-cooks.distance(mod3)
plot(cookmod3, main="Mod3")
abline(h=.889)
cookmod4<-cooks.distance(mod4)
plot(cookmod4, main="Mod4")
abline(h=.889)
mtext("Cook's Distance, Cutoff = 0.889", outer=TRUE, cex =1.5)

###plotted coefficients
coefplot(mod1, intercept=FALSE, Main="Coefficients Mod1")
coefplot(mod2, intercept=FALSE, Main="Coefficients Mod2")
coefplot(mod3, intercept=FALSE, Main="Coefficients Mod3")
coefplot(mod4, intercept=FALSE, Main="Coefficients Mod4")
multiplot(mod1,mod2,mod3,mod4, intercept=FALSE, main="All Coefficients Compared")

###beta / standardized coefficients, printed and plotted
mod1beta<- lm.beta(mod1)
mod2beta<- lm.beta(mod2)
mod3beta<- lm.beta(mod3)
mod4beta<- lm.beta(mod4)
par(mfrow=c(2,2))

sjt.lm(mod1beta, show.ci=FALSE, depvar.labels = "Beta Values")
sjt.lm(mod2beta, show.ci=FALSE)
sjt.lm(mod3beta, show.ci=FALSE)
sjt.lm(mod4beta, show.ci=FALSE)
multiplot(mod1beta, mod2beta, mod3beta, mod4beta, intercept=FALSE, main="All Models Beta Coefficients")


###leaps, automated model selection test
leapsmod<- regsubsets(diff1116 ~ diffpart1116 + expanded + ue2011 + hsormore + bachormore + righttowork + catoregrank + labormktfreedom + pubsafeusnews + fbiprop + murderpercap + stlocper10k + pcturban + snpcredrnk + topcorp + topinctax + infrstrrankusnews + avglocaltaxratesales + combinedratestatelocaltx + avgrdspend0813 + badroads + manufactpct + milpercap + pctparticipateavg, data=data, nvmax = 15)
plot(leapsmod, scale="adjr2", main="Diff1116 Optimized by ML")
leapsmodb<-regsubsets(pctjobgrowth ~ diffpart1116 + expanded + ue2011 + hsormore + bachormore + righttowork + catoregrank + labormktfreedom + pubsafeusnews + fbiprop + murderpercap + stlocper10k + pcturban + snpcredrnk + topcorp + topinctax + infrstrrankusnews + avglocaltaxratesales + combinedratestatelocaltx + avgrdspend0813 + badroads + manufactpct + milpercap + pctparticipateavg, data=data, nvmax = 15)
plot(leapsmodb, scale="adjr2", main="PctJobGrowth Optimized by ML")
leapsmodc<-regsubsets(pctjobgrowth ~ diffpart1116 + expanded + ue2011 + hsormore + bachormore + righttowork + catoregrank + labormktfreedom + pubsafeusnews + fbiprop + murderpercap + stlocper10k + pcturban + snpcredrnk + topcorp + topinctax + infrstrrankusnews + avglocaltaxratesales + combinedratestatelocaltx + avgrdspend0813 + badroads + manufactpct + milpercap + pctparticipateavg, data=data, nvmax = 15)
plot(leapsmodc, scale="adjr2", main="Pctjobgrowth Optimized by ML")

### simulated normal data, histograms and qqplots
### used to compare model residuals with normally distributed data of same n

set.seed(42)
x1<- rnorm(50)
x2<- rnorm(50)
x3<- rnorm(50)
x4<- rnorm(50)
hist(x1, breaks=10, main="Mod1")
hist(x2, breaks=10, main="Mod2")
hist(x3, breaks=10, main="Mod3")
hist(x4, breaks=10, main="Mod4")
mtext("Simulated Histograms of Normal Residuals, 10 Bins", outer=TRUE, cex =1.5)
hist(x1, breaks=15, main="Mod1")
hist(x2, breaks=15, main="Mod2")
hist(x3, breaks=15, main="Mod3")
hist(x4, breaks=15, main="Mod4")
mtext("Simulated Histograms of Normal Residuals, 15 Bins", outer=TRUE, cex =1.5)
qqPlot(x1)
qqPlot(x2)
qqPlot(x3)
qqPlot(x4)

mtext("Q-Q Plots Simulated Normal Distribution, n=50", outer=TRUE, cex =1.5)

qqnorm(x1); qqline(x1)
qqnorm(x2); qqline(x2)
qqnorm(x3); qqline(x3)
qqnorm(x4); qqline(x4)

shapiro.test(x1)
shapiro.test(x2)
shapiro.test(x3)
shapiro.test(x4)
