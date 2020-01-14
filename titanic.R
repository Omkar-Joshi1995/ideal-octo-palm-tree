#Data mining
titanic<-read.csv("D:/Rstudio/Datasets/Titanic.csv", na.strings="")
summary(titanic)
nrow(titanic)
ncol(titanic)
View(titanic)


length(table(titanic$body))
length(table(titanic$cabin))
length(table(titanic$ticket))

#Data Cleaning
table(titanic$pclass)
titanic$pclass[is.na(titanic$pclass)]<-3
table(titanic$survived)
titanic$survived[is.na(titanic$survived)]<-0
titanic$sex[is.na(titanic$sex)]<-'male'
titanic$age[is.na(titanic$age)]<-28
table(titanic$sibsp)
titanic$sibsp[is.na(titanic$sibsp)]<-0
table(titanic$parch)
titanic$parch[is.na(titanic$parch)]<-0
table(titanic$fare)
titanic$fare[is.na(titanic$fare)]<-14.54
titanic$embarked[is.na(titanic$embarked)]<-'S'


View(titanic)
summary(titanic)

titanic<-titanic[,c(-3,-8,-10,-12,-13,-14)]
View(titanic)
dim(titanic)

summary(titanic)
titanic<-na.omit(titanic)
dim(titanic)

titanic$sex<-as.numeric(titanic$sex)
titanic$embarked<-as.numeric(titanic$embarked)
View(titanic)

set.seed(1600)
titanic_sample<-sample(2,nrow(titanic),replace=TRUE,prob=c(.8,.2))
titanic_train<-titanic[titanic_sample==1,]
titanic_test<-titanic[titanic_sample==2,]

#building the model
model_titanic<-glm(survived~., family=binomial, data=titanic_train)
summary(model_titanic)
View(titanic_train)
#prediction of modelling
titanic_pred<-predict(model_titanic, titanic_test, type="response")
titanic_pred

#making data frame of predicted vs actual
pred_act<-data.frame(titanic_pred,titanic_test$survived)
pred_act
colnames(pred_act)[c(1,2)]=c("Predicted","Actual")
pred_act
pred_act=mutate(pred_act, Predicted=ifelse(Predicted>0.5,1,0))
pred_act

table1<-table(pred_act$Predicted, pred_act$Actual)
table1

sum(diag(table1))/sum(table1)

#--------------------------------------------------------------------

#Data mining
titanic1<-read.csv("D:/Rstudio/Datasets/Titanic.csv", na.strings="")
#summary(titanic)
#nrow(titanic)
#ncol(titanic)
#View(titanic)

#Data Cleaning
#table(titanic$pclass)
titanic1$pclass[is.na(titanic1$pclass)]<-3
#table(titanic$survived)
titanic1$survived[is.na(titanic1$survived)]<-0
titanic1$sex[is.na(titanic1$sex)]<-'male'
titanic1$age[is.na(titanic1$age)]<-28
#table(titanic$sibsp)
titanic1$sibsp[is.na(titanic1$sibsp)]<-0
#table(titanic$parch)
titanic1$parch[is.na(titanic1$parch)]<-0
#table(titanic$fare)
titanic1$fare[is.na(titanic1$fare)]<-14.54
titanic1$embarked[is.na(titanic1$embarked)]<-'S'


#View(titanic)
#summary(titanic)

titanic1<-titanic1[,c(-3,-8,-10,-12,-13,-14)]
#View(titanic)
#dim(titanic)

#summary(titanic)
titanic1<-na.omit(titanic1)
#dim(titanic)

titanic1$sex<-as.numeric(titanic1$sex)
titanic1$embarked<-as.numeric(titanic1$embarked)
#View(titanic)

#building the model
model_titanic1<-glm(survived~., family=binomial, data=titanic1)
summary(model_titanic1)
View(titanic1)
#prediction of modelling
titanic1_pred<-predict(model_titanic1, titanic1, type="response")
titanic1_pred

#making data frame of predicted vs actual
pred_act1<-data.frame(titanic1_pred,titanic1$survived)
#pred_act1
colnames(pred_act1)[c(1,2)]=c("Predicted","Actual")
#pred_act1
pred_act1=mutate(pred_act1, Predicted=ifelse(Predicted>0.5,1,0))
pred_act1

table2<-table(pred_act1$Predicted, pred_act1$Actual)
table2

sum(diag(table2))/sum(table2)


############################################  only with boat

#Data mining
titanic2<-read.csv("D:/Rstudio/Datasets/Titanic.csv", na.strings="")
#summary(titanic)
#nrow(titanic)
#ncol(titanic)
View(titanic2)

titanic2<-titanic2[,c(2,12)]
summary(titanic2)
table(titanic2$boat)

titanic2$boat[is.na(titanic2$boat)]<-13
titanic2<-na.omit(titanic2)
titanic2$boat<-as.numeric(titanic2$boat)
model_titanic2<-glm(survived~., family=binomial, data=titanic2)
summary(model_titanic2)
View(titanic2)
#prediction of modelling
titanic2_pred<-predict(model_titanic2, titanic2, type="response")
titanic2_pred

#making data frame of predicted vs actual
pred_act2<-data.frame(titanic2_pred,titanic2$survived)
#pred_act1
colnames(pred_act2)[c(1,2)]=c("Predicted","Actual")
#pred_act1
pred_act2=mutate(pred_act2, Predicted=ifelse(Predicted>0.5,1,0))
pred_act2

table3<-table(pred_act2$Predicted, pred_act2$Actual)
table3

sum(diag(table3))/sum(table3)

#-----------------------------------4 variables


#Data mining
titanic3<-read.csv("D:/Rstudio/Datasets/Titanic.csv", na.strings="")
#summary(titanic)
#nrow(titanic)
#ncol(titanic)
View(titanic3)

titanic3<-titanic3[,c(1,2,4,5,9)]
summary(titanic3)


titanic3$fare[is.na(titanic3$fare)]<-14.54
titanic3<-na.omit(titanic3)
titanic3$sex<-as.numeric(titanic3$sex)
model_titanic3<-glm(survived~., family=binomial, data=titanic3)
summary(model_titanic3)
View(titanic3)
#prediction of modelling
titanic3_pred<-predict(model_titanic3, titanic3, type="response")
titanic3_pred

#making data frame of predicted vs actual
pred_act3<-data.frame(titanic3_pred,titanic3$survived)
#pred_act1
colnames(pred_act3)[c(1,2)]=c("Predicted","Actual")
#pred_act1
library(dplyr)
pred_act3=mutate(pred_act3, Predicted=ifelse(Predicted>0.5,1,0))
pred_act3

table4<-table(pred_act3$Predicted, pred_act3$Actual)
table4

sum(diag(table4))/sum(table4)
