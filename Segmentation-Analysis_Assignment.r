#Assignment_Session18

#a Create classification model using different decision trees.
#b Verify model goodness of fit.
#c Apply all the model validation techniques.
#d Make conclusions

#Answers
#reading the dataset cs2m and viewing

cs2m <- read.csv("D:\\acadgild\\cs2m.csv")
View(cs2m)

names(cs2m)
nrow(cs2m)
ncol(cs2m)
str(cs2m)

#decision tree
select_rows<- sample(1:nrow(cs2m),round(0.2*nrow(cs2m)),replace = F)
cs2mTest<- cs2m[select_rows,]
cs2mTest
cs2mTrain<- cs2m[-(select_rows),]
cs2mTrain

library(tree)
modelRegTree<- tree(BP~DrugR+Chlstrl+Age+Prgnt+AnxtyLH,data = cs2mTrain)
plot(modelRegTree)

text(modelRegTree,pretty = 0 ,cex=0.75)

pred<- predict(modelRegTree,newdata= cs2mTest)
head(pred,3)

ME<- sum(cs2mTest$BP - pred)/nrow(cs2mTest)
ME
RSS<- sum(cs2mTest$BP-pred)^2
RSS
RMSE<- sqrt(RSS/nrow(cs2mTest))
RMSE
MAPE<- sum(abs(cs2mTest$BP-pred)/cs2mTest$BP)*100
MAPE

library(tree)
modelRegTree1<- tree(DrugR~BP+Chlstrl+Age+Prgnt+AnxtyLH,data = cs2mTrain)
plot(modelRegTree1)

text(modelRegTree1,pretty = 0 ,cex=0.75)

pred<- predict(modelRegTree1,newdata= cs2mTest)
head(pred,3)

ME<- sum(cs2mTest$DrugR - pred)/nrow(cs2mTest)
ME
RSS<- sum(cs2mTest$DrugR-pred)^2
RSS
RMSE<- sqrt(RSS/nrow(cs2mTest))
RMSE
MAPE<- sum(abs(cs2mTest$DrugR-pred)/cs2mTest$DrugR)*100
MAPE

#classification 
library(caTools)
library(tree)
#splitting
set.seed(1)
split<- sample.split(cs2m$DrugR,SplitRatio = 0.70)
cs2mTrain <- subset(cs2m,split == TRUE)
cs2mTest<- subset(cs2m, split == FALSE)

table(cs2m$DrugR)
table(cs2mTrain$DrugR)
table(cs2mTest$DrugR)

prop.table(table(cs2mTest$DrugR))
table(cs2mTest$DrugR)
prop.table(table(cs2mTrain$DrugR))

modelClassTree<- tree(DrugR~BP+Chlstrl+Age+Prgnt+AnxtyLH,data = cs2mTrain)
plot(modelClassTree)

text(modelClassTree,pretty = 0 ,cex=0.75)

pred<- predict(modelClassTree,newdata= cs2mTest)
head(pred,3)
cs2m$predict <- predict
cs2m$predictROUND<- round(predict,digits = 0)

#confusion matrix
table(cs2m$DrugR,predict>= 0.5)

sum<- sum(table(cs2m$DrugR,predict>= 0.5))

#Accuracy and model goodness
#accuracy of our model
accuracy<- (13+12)/(30)
accuracy
#0.8333333333

#model goodness
library(verification)
predictTrain<- predict(model,cs2m,type="response")
table(cs2m$DrugR,predictTrain >=0.5)
head(predictTrain,3)
auc(cs2m$DrugR,predictTrain)

#Conclusions
#Area under the curve is 0.9333333 & accuracy of the model is 0.8333333333
#By visualising the results of various measures like ME,RSS,RMSE,MAPE of the tree it can be concluded that the model is good and fit.