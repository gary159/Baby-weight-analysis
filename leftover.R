```{r}
attach(bwt.grams)
par(mfrow = c(2,2))
boxplot(baby.grams ~ race, col = c(4,5,6))
points(sapply(levels(race), function(x) {mean(baby.grams[race == x])}), pch = "+")
boxplot(baby.grams ~ smoke, col = c(4,5))
points(sapply(c(T, F), function(x) {mean(baby.grams[smoke == x])}), pch = "+")
boxplot(baby.grams ~ prem.labor, col = c(4,5))
points(sapply(c(T, F), function(x) {mean(baby.grams[prem.labor == x])}), pch = "+")
boxplot(baby.grams ~ physician.visits, col = c(4,5,6))
points(sapply(levels(physician.visits), function(x) {mean(baby.grams[physician.visits == x])}), pch = "+")
```

```{r}
library (leaps)
regfit.full=regsubsets(baby.grams~., bwt.grams, nvmax =19)
reg.summary = summary(regfit.full)
reg.summary$rsq
par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS", type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type="l")
max.adjr2=which.max (reg.summary$adjr2)
max.adjr2
points (max.adjr2, reg.summary$adjr2[max.adjr2], col ="red",cex =2, pch =20)


plot(reg.summary$cp ,xlab =" Number of Variables ", ylab="Cp", type='l')
min.cp= which.min (reg.summary$cp )
min.cp
points (min.cp, reg.summary$cp[min.cp], col ="red",cex =2, pch =20)


min.bic = which.min(reg.summary$bic)
min.bic
plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC", type='l')
points (min.bic, reg.summary$bic [min.bic], col =" red",cex =2, pch =20)

plot(regfit.full ,scale ="r2")
plot(regfit.full ,scale ="adjr2")
plot(regfit.full ,scale ="Cp")
plot(regfit.full ,scale ="bic")

coef(regfit.full, max.adjr2) 
coef(regfit.full, min.cp)
coef(regfit.full, min.bic)

classfit.full=regsubsets(below.2500~., bwt, nvmax =19)
class.summary = summary(classfit.full)
class.summary$rsq
par(mfrow =c(2,2))
plot(class.summary$rss ,xlab=" Number of Variables ",ylab=" RSS", type="l")
plot(class.summary$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type="l")
max.adjr2=which.max (class.summary$adjr2)
max.adjr2
points (max.adjr2, class.summary$adjr2[max.adjr2], col ="red",cex =2, pch =20)


plot(class.summary$cp ,xlab =" Number of Variables ", ylab="Cp", type='l')
min.cp= which.min (class.summary$cp )
min.cp
points (min.cp, class.summary$cp[min.cp], col ="red",cex =2, pch =20)


min.bic = which.min(class.summary$bic)
min.bic
plot(class.summary$bic ,xlab=" Number of Variables ",ylab=" BIC", type='l')
points (min.bic, class.summary$bic [min.bic], col =" red",cex =2, pch =20)

plot(classfit.full ,scale ="r2")
plot(classfit.full ,scale ="adjr2")
plot(classfit.full ,scale ="Cp")
plot(classfit.full ,scale ="bic")

coef(classfit.full, max.adjr2) 
coef(classfit.full, min.cp)
coef(classfit.full, min.bic)



#Let's compare classification and regression 
par(mfrow =c(1,2))
plot(classfit.full ,scale ="Cp")
plot(regfit.full ,scale ="Cp")

#Linear regression with the predictors selected by best subset
lm.fit = lm( baby.grams~ mother.weight+race+smoke+hypertension+uterine, data=bwt.grams)
summary(lm.fit)
confint(lm.fit)
par(mfrow = c(2, 2))
plot(lm.fit)
```