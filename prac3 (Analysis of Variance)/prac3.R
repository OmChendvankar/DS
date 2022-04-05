"F-test"
ftest = read.csv(file.choose(),header = T)
ftest
var.test(ftest$time_g1,ftest$time_g2,alternative="two.sided")

"One way anova"
d1 = read.csv(file.choose(),header = T)
names(d1)
summary(d1)
head(d1)
anv = aov(formula=satindex~dept,data=d1)
summary(anv)

"Two way anova"
d2 = read.csv(file.choose(),header = T)
names(d2)
summary(d2)
head(d2)
anv1 = aov(formula=satindex~dept+exp+dept*exp,data=d2)
summary(anv1)
