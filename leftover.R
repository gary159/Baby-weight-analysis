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