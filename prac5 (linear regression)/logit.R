"Linear Regression"
"Scatter Plot"
house = read.csv(file.choose(), header = T)
summary(house)
names(house)
pairs(~death_rate+doctor_avail+annual_income+density_per_capita,data = house)

"Fit a Model"
housemodel= lm(density_per_capita~death_rate+doctor_avail+hosp_avail+annual_income,data = house)
summary(housemodel)

"check for global testing"
index = read.csv(file.choose(), header = T)
names(index)
pairs(~index+written+langauge+tech+gk,data=index)
model1= lm(index~.,data = index)
summary(model1)
index$pred = fitted(mode1)
head(index)
index$res = residuals(model1)
head(index)

"check the multicollinearity"
library(car)
vif(model1)

"Check Heteroscedasticity"
plot(index$pred,index$res,col="red")

"Shapiro test to check normality of errors"
shapiro.test(index$res)

"detecting autocorreleation using durbin watson test d=2(1-r)"
library(car)
durbinWatsonTest(model1)

"influence plot"
influencePlot(model1)
index = index[-33,]

"Validation using Hold-Out method"
library("caret")
library("lattice")
library("ggplot2")
indexx = read.csv(file.choose(), header = T)
summary(indexx)
d = createDataPartition(index$empid,p=0.8,list=F)
head(d)
dim(d)
traindata = indexx[d,]
testdata= indexx[-d,]
dim(traindata)
dim(testdata)

"Validation using k fold method"
"repetative k fold"
kfolds= trainControl(method= "CV", number = 4)
modelkfold = train(index~written+langauge+tech+gk,data = index,method="lm",trControl)
modelkfold

"validation using leave one out"
kfoldsloocv<-trainControl(method="LOOCV")
kfoldsloocvmodel<-train(index~written+langauge+tech+gk,data = index,method="lm",trControl)
kfoldsloocvmodel

"Model selection forward method"
null<-lm(index~1,data=index)
full<-lm(index~.,data = index)
names(index)
step(null,scope = list(lower=null,upper=full),direction = "forward")

"Model selection backward method"
step(full,scope = list(lower=null,upper=full),direction = "backward")