"practical of decision tree"
titanic = read.csv(file.choose(),header = T)
summary(titanic)
names(titanic)


install.packages("partykit")
install.packages("CHAID", repos = "http://R-Forge.R-project.org",type = "source")
library(partykit)
library(CHAID)
titanic$Survived = as.factor(titanic$Survived)
summary(titanic$Survived)
names(titanic)

library(rpart)
fit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=titanic,method="class")
plot(fit)
text(fit)

install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("RGtk2")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit = rpart (Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=titanic,method="class")
fancyRpartPlot(fit)
Prediction = predict(fit,titanic, type= "class")
Prediction
