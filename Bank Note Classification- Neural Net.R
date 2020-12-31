# Importing Libraries----

library(Amelia)
library(neuralnet)
library(caTools)
library(MASS)
library(ggplot2)

# Reading the csv file

df<-read.csv("D:\\Udemy - Data Science and Machine Learning Bootcamp with R\\R-Course-HTML-Notes\\R-for-Data-Science-and-Machine-Learning\\Training Exercises\\Machine Learning Projects\\CSV files for ML Projects\\bank_note_data.csv")

df<-as.data.frame(df)

str(df)

# Normalising the data----

maxs<-apply(df,2,max)
mins<-apply(df,2,min)

df.scale<-as.data.frame(scale(df,center=mins,scale=maxs-mins))
df.scale

split<-sample.split(df.scale$Class,SplitRatio = 0.7)
train<-subset(df.scale,split=T)
test<-subset(df.scale,split=F)

# Neural Network----

n<-names(df)

f<-as.formula(paste("Class~",paste(n[!n %in% 'Class'],collapse = '+')))

nn<-neuralnet(f,data=train,hidden=c(14,7,4),linear.output = F)

plot(nn)

# Predicting the values-------
preds.nn<-compute(nn,test)
y.pred<-apply(preds.nn$net.result,2,round)

cm<-table(y.pred,test$Class,dnn = c("Predicted Values","Actual Values"))

fourfoldplot(cm,main = "Confusion Matrix",color = c("Red","Green"))

