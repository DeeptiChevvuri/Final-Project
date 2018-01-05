#Final Project
InvDataSet<-read.table("G:/MIS/Sem 2/Stat 526/Project/Final Project/InvDataSet2.csv",header=T,sep=",",quote = "\"")
summary(InvDataSet)
head(InvDataSet)
plot(InvDataSet)
plot(X6~X5,data = InvDataSet, main = "$ On Hand vs. Item Cost",pch=20,xlab = "Item Cost",ylab = "$ On Hand")
plot(X6~X7,data = InvDataSet, main = "$ On Hand vs. Annual Usage Qty",pch=20,xlab = "Annual Usage Qty",ylab = "$ On Hand")
plot(X6~X8,data = InvDataSet, main = "$ On Hand vs. Average Daily Usage Qty",pch=20,xlab = "Average Daily Usage Qty",ylab = "$ On Hand")
plot(X6~X9,data = InvDataSet, main = "$ On Hand vs. Minimum Order Qty",pch=20,xlab = "Minimum Order Qty",ylab = "$ On Hand")
plot(X6~X10,data = InvDataSet, main = "$ On Hand vs. Box Size Qty",pch=20,xlab = "Box Size Qty",ylab = "$ On Hand")
plot(X6~X11,data = InvDataSet, main = "$ On Hand vs. Lead Time (days)",pch=20,xlab = "Lead Time (days)",ylab = "$ On Hand")
plot(X6~X12,data = InvDataSet, main = "$ On Hand vs. Annual Usage $",pch=20,xlab = "Annual Usage $",ylab = "$ On Hand")
plot(X6~X13,data = InvDataSet, main = "$ On Hand vs. Manufacturing Site",pch=20,xlab = "Manufacturing Site",ylab = "$ On Hand")
plot(X6~X14,data = InvDataSet, main = "$ On Hand vs. Planner Code",pch=20,xlab = "Planner Code",ylab = "$ On Hand")
plot(X6~X16,data = InvDataSet, main = "$ On Hand vs. Safety Stock Qty",pch=20,xlab = "Safety Stock Qty",ylab = "$ On Hand")
plot(X6~X17,data = InvDataSet, main = "$ On Hand vs. On Time Delivery",pch=20,xlab = "% Received On Time",ylab = "$ On Hand")
plot(X6~X18,data = InvDataSet, main = "$ On Hand vs. Delivery Frequency",pch=20,xlab = "Delivery Frequency",ylab = "$ On Hand")
plot(X6~X19,data = InvDataSet, main = "$ On Hand vs. Average Daily Usage $",pch=20,xlab = "Average Daily Usage $",ylab = "$ On Hand")
plot(X6~X20,data = InvDataSet, main = "$ On Hand vs. Minimum Order ($)",pch=20,xlab = "Minimum Order ($)",ylab = "$ On Hand")
plot(X6~X21,data = InvDataSet, main = "$ On Hand vs. Box Size ($)",pch=20,xlab = "Box Size ($)",ylab = "$ On Hand")
plot(X6~X22,data = InvDataSet, main = "$ On Hand vs. Safety Stock ($)",pch=20,xlab = "Safety Stock ($)",ylab = "$ On Hand")

#estimating the corelation coef - not sure if this is correct????
cor(InvDataSet$X6,InvDataSet$X5)

#a few examples of using just simple linear modeling
#is the daily usage in dollars significant?
mod0=lm(X6~X19,data=InvDataSet)
mod0
summary(mod0)

#is on-time delivery significant?
mod1=lm(X6~X17,data=InvDataSet)
mod1
summary(mod1)

#is the delivery frequency significant?
mod2=lm(X6~X18,data=InvDataSet)
mod2
summary(mod2)

#is the planning method significant?
mod3=lm(X..on.Hand~Planning.Method,data=InvDataSet)
mod3
summary(mod3)

#is the planner code significant?
mod4=lm(X..on.Hand~Planner.Code,data=InvDataSet)
mod4
summary(mod4)

#is the manufacturing site significant?
mod5=lm(X..on.Hand~Mfg_Site,data=InvDataSet)
mod5
summary(mod5)

#is the supplier significant?
mod6=lm(X..on.Hand~Supplier,data=InvDataSet)
mod6
summary(mod6)

#is the daily usage in dollars + Planner.Code significant?
mod10=lm(X..on.Hand~Average.Daily.Usage.Qty*Item.Cost+Planner.Code,data=InvDataSet)
mod10=lm(X6~X5*X8+X14,data=InvDataSet)
mod10
summary(mod10)

#From HW6 analysis
library(car)
install.packages("leaps")
library(leaps)
library(MASS)

############################
#First analysis was to review the data without any categorical variables to see what the model would look like
#This first set of models is only using the annual data to help build a model for current inventory $ on hand
#Step AIC
mod.simple<-lm(X6~1,data=InvDataSet)
mod.full<-lm(X6~X5+X7+X9+X10+X11+X12+X16+X17+X20+X21+X22,data=InvDataSet)
summary(mod.full)
summary(mod.simple)
stepmod<-stepAIC(mod.full)
vif(mod.full)
AIC(mod.full)

#Step backward
stepback<-stepAIC(mod.full,scope=list(upper=mod.full,lower=mod.simple),direction="backward")
stepback$anova
mod.back<-lm(X6 ~ X7 + X12 + X16 + X17 + X20 + X21 + X22,data=InvDataSet)
summary(mod.back)
vif(mod.back)
AIC(mod.back)

#Step forward
stepforward<-stepAIC(mod.simple,scope=list(upper=mod.full,lower=mod.simple),direction="forward")
stepforward$anova
mod.forward<-lm(X6 ~ X22 + X12 + X21 + X20 + X17,data=InvDataSet)
summary(mod.forward)
vif(mod.forward)
AIC(mod.forward)

#Step Mixed
stepmixed<-stepAIC(mod.simple,scope=list(upper=mod.full,lower=mod.simple),direction="both")
stepmixed$anova
mod.mixed<-lm(X6 ~ X22 + X12 + X21 + X20 + X17,data=InvDataSet)
summary(mod.mixed)
vif(mod.mixed)
AIC(mod.mixed)

#1.g
sub<-regsubsets(X6~X11+X20+X12+X21+X17,data=InvDataSet,nbest=1,nvmax=8)
plot(sub,scale="bic",main="Model Selection using BIC Criterion")
sub.mod<-lm(X6~X11+X20+X12+X21+X17,data=InvDataSet)
summary(sub.mod)
AIC(sub.mod)
stepmod<-stepAIC(sub.mod,scope=list(upper=sub.mod,lower=sub.mod),direction="both")
stepmod<-stepAIC(sub.mod)
vif(stepmod)

############################
#Second analysis was to review the data without any categorical variables to see what the model would look like
#This second set of models is only using the daily data to help build a model for current inventory $ on hand
#Step AIC
mod.simple<-lm(X6~1,data=InvDataSet)
mod.full<-lm(X6~X5+X8+X9+X10+X11+X16+X17+X19+X20+X21+X22,data=InvDataSet)
mod.full
summary(mod.full)
summary(mod.simple)
stepmod<-stepAIC(mod.full)
vif(mod.full)
AIC(mod.full)

#Step backward
stepback<-stepAIC(mod.full,scope=list(upper=mod.full,lower=mod.simple),direction="backward")
stepback$anova
mod.back<-lm(X6 ~ X17 + X19 + X20 + X21 + X22,data=InvDataSet)
summary(mod.back)
vif(mod.back)
AIC(mod.back)

#Step forward
stepforward<-stepAIC(mod.simple,scope=list(upper=mod.full,lower=mod.simple),direction="forward")
stepforward$anova
mod.forward<-lm(X6 ~ X22 + X19 + X21 + X20 + X17,data=InvDataSet)
summary(mod.forward)
vif(mod.forward)
AIC(mod.forward)

#Step Mixed
stepmixed<-stepAIC(mod.simple,scope=list(upper=mod.full,lower=mod.simple),direction="both")
stepmixed$anova
mod.mixed<-lm(X6 ~ X22 + X19 + X21 + X20 + X17,data=InvDataSet)
summary(mod.mixed)
vif(mod.mixed)
AIC(mod.mixed)

#1.g
sub<-regsubsets(X6~X11+X20+X19+X21+X17,data=InvDataSet,nbest=1,nvmax=8)
plot(sub,scale="bic",main="Model Selection using BIC Criterion")
sub.mod<-lm(X6~X11+X20+X19+X21+X17,data=InvDataSet)
summary(sub.mod)
AIC(sub.mod)
stepmod<-stepAIC(sub.mod,scope=list(upper=sub.mod,lower=sub.mod),direction="both")
stepmod<-stepAIC(sub.mod)
vif(stepmod)


############################
#Third analysis was to review the data including the categorical variables to see what the model would look like
#Includes both daily & annual data to allow the model to select the most appropriate values
#We chose not to include the categorical variables of Supplier, Part Number, Item UOM, Planner Code, and Planning Method
#Step AIC
mod.simple<-lm(X6~1,data=InvDataSet)
mod.full<-lm(X6~X5+X7+X8+X9+X10+X11+X12+X13+X16+X17+X18+X19+X20+X21+X22,data=InvDataSet)
summary(mod.full)
mod.full
summary(mod.simple)
stepmod<-stepAIC(mod.full)
vif(mod.full)
AIC(mod.full)

#Step backward
stepback<-stepAIC(mod.full,scope=list(upper=mod.full,lower=mod.simple),direction="backward")
stepback$anova
mod.back<-lm(X6 ~ X12 + X13 + X20 + X21 + X22,data=InvDataSet)
summary(mod.back)
vif(mod.back)
AIC(mod.back)

#Step forward
stepforward<-stepAIC(mod.simple,scope=list(upper=mod.full,lower=mod.simple),direction="forward")
stepforward$anova
mod.forward<-lm(X6 ~ X22 + X19 + X21 + X13 + X20,data=InvDataSet)
summary(mod.forward)
vif(mod.forward)
AIC(mod.forward)

#Step Mixed
stepmixed<-stepAIC(mod.simple,scope=list(upper=mod.full,lower=mod.simple),direction="both")
stepmixed$anova
mod.mixed<-lm(X6 ~ X22 + X19 + X21 + X13 + X20,data=InvDataSet)
summary(mod.mixed)
vif(mod.mixed)
AIC(mod.mixed)

#All combinations
mod.all<-lm(X6 ~ X12 + X13 + +X19 + X20 + X21 + X22,data=InvDataSet)
summary(mod.all)
vif(mod.all)
AIC(mod.all)

#1.g
sub<-regsubsets(X6~X11+X20+X19+X21+X17,data=InvDataSet,nbest=1,nvmax=8)
plot(sub,scale="bic",main="Model Selection using BIC Criterion")
sub.mod<-lm(X6~X11+X20+X19+X21+X17,data=InvDataSet)
summary(sub.mod)
AIC(sub.mod)
stepmod<-stepAIC(sub.mod,scope=list(upper=sub.mod,lower=sub.mod),direction="both")
stepmod<-stepAIC(sub.mod)
vif(stepmod)

############################
#4th analysis was to review the data including the categorical variables to see what the model would look like
#Includes both daily & annual data to allow the model to select the most appropriate values
#We chose not to include the categorical variables of Supplier, Part Number, Item UOM, Planner Code, and Planning Method
#Removed variables that were "internally set" vs. "externally set"
#Step AIC
mod.simple<-lm(X6~1,data=InvDataSet)
mod.full<-lm(X6~X5+X7+X8+X9+X10+X11+X12+X17+X18+X19+X20+X21,data=InvDataSet)
mod.full
summary(mod.full)
summary(mod.simple)
stepmod<-stepAIC(mod.full)
vif(mod.full)
AIC(mod.full)

#Step backward
stepback<-stepAIC(mod.full,scope=list(upper=mod.full,lower=mod.simple),direction="backward")
stepback$anova
mod.back<-lm(X6 ~ X11 + X17 + X19 + X20 + X21,data=InvDataSet)
summary(mod.back)
vif(mod.back)
AIC(mod.back)

#Step forward
stepforward<-stepAIC(mod.simple,scope=list(upper=mod.full,lower=mod.simple),direction="forward")
stepforward$anova
mod.forward<-lm(X6 ~ X19 + X21 + X20 + X17 + X11,data=InvDataSet)
mod.forward
summary(mod.forward)
vif(mod.forward)
AIC(mod.forward)

#Step Mixed
stepmixed<-stepAIC(mod.simple,scope=list(upper=mod.full,lower=mod.simple),direction="both")
stepmixed$anova
mod.mixed<-lm(X6 ~ X19 + X21 + X20 + X17 + X11,data=InvDataSet)
mod.mixed
summary(mod.mixed)
vif(mod.mixed)
AIC(mod.mixed)

#1.g
sub<-regsubsets(X6~X11+X20+X19+X21+X17,data=InvDataSet,nbest=1,nvmax=8)
plot(sub,scale="bic",main="Model Selection using BIC Criterion")
sub.mod<-lm(X6~X11+X20+X19+X21+X17,data=InvDataSet)
summary(sub.mod)
AIC(sub.mod)
stepmod<-stepAIC(sub.mod,scope=list(upper=sub.mod,lower=sub.mod),direction="both")
stepmod<-stepAIC(sub.mod)
vif(stepmod)


#Final Dataset Review
InvDataSet<-read.table("G:/MIS/Sem 2/Stat 526/Project/Final Project/InvDataSet2.csv",header=T,sep=",",quote = "\"")
summary(InvDataSet)
head(InvDataSet)
plot(InvDataSet)
mod.final<-lm(X6 ~ X19 + X21 + X20 + X17 + X11,data=InvDataSet)
summary(mod.final)
mod.final
AIC(mod.final)
vif(mod.final)

#Check fit
fit.lm<-lm(X6~X11,data=InvDataSet)
plot(fit.lm$fitted.values,fit.lm$residuals,pch=20,xlab = "Predicted", ylab = "Residuals")
abline(h=0)

fit.lm<-lm(X6~X17,data=InvDataSet)
plot(fit.lm$fitted.values,fit.lm$residuals,pch=20,xlab = "Predicted", ylab = "Residuals")
abline(h=0)

fit.lm<-lm(X6~X19,data=InvDataSet)
plot(fit.lm$fitted.values,fit.lm$residuals,pch=20,xlab = "Predicted", ylab = "Residuals")
abline(h=0)

fit.lm<-lm(X6~X20,data=InvDataSet)
plot(fit.lm$fitted.values,fit.lm$residuals,pch=20,xlab = "Predicted", ylab = "Residuals")
abline(h=0)

fit.lm<-lm(X6~X21,data=InvDataSet)
plot(fit.lm$fitted.values,fit.lm$residuals,pch=20,xlab = "Predicted", ylab = "Residuals")
abline(h=0)


library(car)
qqplot(fit.lm$resid,ylab = "Residuals")

fit.lm<-lm(X6~X22,data=InvDataSet)
fit.lm
abline(fit.lm,col='red')
qqplot(fit.lm$resid,ylab = "Residuals")
summary(fit.lm)


# check for outliers

RMSE<-1153
lev <- c(hatvalues(mod.final))> 2*(k+1)/n
plot(c(5,summary(mod.final)$residuals/RMSE)~c(.3,hatvalues(mod.final)),
     pch = 19, ylab = "Standardized Residuals", 
     xlab = "Leverage",col = 0)
abline(a = -3,b=0 , col = 1)
abline(a = 3,b=0 , col = 1)
outlier <- (abs(summary(mod.final)$residuals/RMSE))>3
points((summary(mod.final)$residuals/RMSE)[outlier]~hatvalues(mod.final)[outlier],col = 1,pch = 19)
notoutlier <- (abs(summary(mod.final)$residuals/RMSE))<3
points((summary(mod.final)$residuals/RMSE)[notoutlier]~hatvalues(mod.final)[notoutlier],pch = 19)
points((summary(mod.final)$residuals/RMSE)[lev]~hatvalues(mod.final)[lev],pch = 19,col = 5)
abline(v = 2*(k+1)/n,col = 5) #Line to Identify high leverage obs
un<- (abs(summary(mod.final)$residuals/RMSE))>3&c(hatvalues(mod.final))> 2*(k+1)/n
points((summary(mod.final)$residuals/RMSE)[un]~hatvalues(mod.final)[un],pch = 19,col = 'red')


#Cook's Distance--Effect of Deleting an Individual Observation on the Model Fit
plot(cooks.distance(mod.final), ylab='Cook Distance', xlab='Observation number',pch=19)
cooksdistaceall<-cooks.distance(mod.final)
top5<-sort(cooksdistaceall, decreasing = TRUE)
top5[1:5]# its shows top 3 have cooks distance >1 hence outliers

non_influ <- cooks.distance(mod.final)<1  ### Non - Influential points. 
fast_small <- InvDataSet[non_influ,]

#Fit Model on Subset
out_small <-lm(X6 ~ X19 + X21 + X20 + X17 + X11, data = fast_small)
summary(out_small)



