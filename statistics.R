sum(labour$fincome)
mean(labour$fincome)

hist(labour$wage)
hist(labour$age)
hist(labour$youngkids)
hist(labour$oldkids)

mean(labour$wage)
mean(labour$hours)

median(labour$age)
boxplot(labour$age)
median(labour$hours)
boxplot(labour$hours)
median(labour$wage)
boxplot(labour$age,labour$wage)

mode(labour$hours)

cor(labour$wage,labour$tax)
cor(labour$wage, labour$experience)
cor(labour$experience, labour$education)

