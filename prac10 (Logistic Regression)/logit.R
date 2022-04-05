loan<-read.csv(file.choose(),header=T,sep=",")
head(loan)
summary(loan)
str(loan)
loan$AGE<-as.factor(loan$AGE)
str(loan)
names(loan)
"creating model"
model1<-glm(DEFAULTER~.,family = binomial,data = loan)
summary(model1)
"global testing for the acceptance of the model"
null<-glm(DEFAULTER~1,family = binomial,data=loan)
anova(null,model1,test = "Chisq")
"predicting the probilities"
loan$predprob<-round(fitted(model1),2)
head(loan)
"classification and misclassification analysis "
library(gmodels)
table(loan$DEFAULTER,fitted(model1)>0.5)
sens<-95/(88+95)*100
sens
spc<-478/(478+39)*100
spc

"check the trade off between sensivity and specificity using different cut off values"
table(loan$DEFAULTER,fitted(model1)>0.1)
table(loan$DEFAULTER,fitted(model1)>0.2)
table(loan$DEFAULTER,fitted(model1)>0.3)
table(loan$DEFAULTER,fitted(model1)>0.4)
table(loan$DEFAULTER,fitted(model1)>0.5)

"goodness of fit using receiver Operational Curve "
pred<-predict(model1,loan,type="response")
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(pred,loan$DEFAULTER)
rocrperf<-performance(rocrpred,"tpr","fpr")
"to check proper cut off point"
plot(rocrperf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

"to check coeficients"
coef(model1)
exp(coef(model1))

"as credit to debit ratio of person incerases by 1 unit ,odds of the event increses by 77%"
"model validation same as linear regression"
"variable selection same as linear regresiion"