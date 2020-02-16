require(ISLR)
require(MASS)
require(faraway)
require(ISLR)
require(DAAG)
require(leaps)
require(glmnet)
require(dplyr)
require(pls)
require(rpart)
require(randomForest)
require(caret)
require(dplyr)
require(gbm)

data=read.csv("bikes.csv")
pred.sbs=function(obj,new,id,...){
  form=as.formula(obj$call[[2]])
  mat=model.matrix(form,new)
  coefi=coef(obj,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
#Categorical Data
data$season1=as.factor(data$season)
data$yr1=as.factor(data$yr)
data$workingday1=as.factor(data$workingday)
data$weathersit1=as.factor(data$weathersit)

#Creating data set for response-Casual
data.casual=data[,c(-1:-6,-11)]
pairs(data.casual) #Scatterplot to check correlation in data
cor(data.casual[,-5:-8])

#Creating data set for response-Registered
data.registered=data[,c(-1:-6,-10)]
pairs(data.registered) #Scatterplot to check correlation in data
cor(data.registered[,-5:-8])

#linear regression Model
#For Casual
lmod.casual=lm(casual~.,data.casual)
summary(lmod.casual)
par(mfrow=c(2,2))
plot(lmod.casual)

# perform CV
k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:5,nrow(data.casual),replace=T) 
# create dummy matrix to store CV error estimates
cv.errL=matrix(NA,k,7,dimnames=list(NULL,paste(1:7)))

for (j in 1:k){
  # pick models with lowest RSS with 1-7 predictors fit without kth fold
  best.modsL=regsubsets(casual~.,data=data.casual[folds!=j,],
                       nvmax=7,method="exhaustive")
  #estimate test error for all seven models by predicting kth fold 
  for (i in 1:7){
    predL=pred.sbs(best.modsL,data.casual[folds==j,],id=i)
    cv.errL[j,i]=mean((data.casual$casual[folds==j]-predL)^2)  # save error est
  }
}

mse.cvL=apply(cv.errL,2,mean) # compute mean MSE for each number of predictors #2-apply it to rows
min=which.min(mse.cvL)  # find minimum mean MSE
min(mse.cvL)

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:7,mse.cvL,type="b",xlab="no. of predictors)",ylab="est. test MSE")
points(min,mse.cvL[min],cex=2,col="red",lwd=2)

#For Registered
lmod.registered=lm(registered~.,data.registered)
summary(lmod.registered)
par(mfrow=c(2,2))
plot(lmod.registered)
#perform CV 

k=5  # set number of folds
set.seed(123)
folds=sample(1:5,nrow(data.registered),replace=T)
cv.errR=matrix(NA,k,7,dimnames=list(NULL,paste(1:7)))

for (j in 1:k){
  # pick models with lowest RSS with 1-7 predictors fit without kth fold
  best.modsR=regsubsets(registered~.,data=data.registered[folds!=j,],
                        nvmax=7,method="exhaustive")
  #estimate test error for all seven models by predicting kth fold 
  for (i in 1:7){
    predR=pred.sbs(best.modsR,data.registered[folds==j,],id=i)
    cv.errR[j,i]=mean((data.registered$registered[folds==j]-predR)^2)  # save error est
  }
}

mse.cvR=apply(cv.errR,2,mean) # compute mean MSE for each number of predictors #2-apply it to rows
min=which.min(mse.cvR)  # find minimum mean MSE
min(mse.cvR)

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:7,mse.cvR,type="b",xlab="no. of predictors)",ylab="est. test MSE")
points(min,mse.cvR[min],cex=2,col="red",lwd=2)

#Centering the predictors for the complete second order model
#CASUAL
temp_c=data.casual$temp-mean(data.casual$temp)
hum_c=data.casual$hum-mean(data.casual$hum)
windspeed_c=data.casual$windspeed-mean(data.casual$windspeed)

data.casual$temp2=temp_c*temp_c
data.casual$hum2=hum_c*hum_c
data.casual$windspeed2=windspeed_c*windspeed_c
data.casual$tempxhum=temp_c*hum_c
data.casual$tempxwindspeed=temp_c*windspeed_c
data.casual$humxwindspeed=hum_c*windspeed_c

ModelS=lm(casual~.,data.casual)
summary(ModelS)
par(mfrow=c(2,2))
plot(ModelS)

#Perform CV
k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:5,nrow(data.casual),replace=T) 
# create dummy matrix to store CV error estimates
cv.errS=matrix(NA,k,13,dimnames=list(NULL,paste(1:13)))

# perform CV
for (j in 1:k){
  # pick models with lowest RSS with 1-13 predictors fit without kth fold
  best.modsS=regsubsets(casual~.,data=data.casual[folds!=j,],
                       nvmax=13,method="exhaustive")
  #estimate test error for all thirteen models by predicting kth fold 
  for (i in 1:13){
    predS=pred.sbs(best.modsS,data.casual[folds==j,],id=i)
    cv.errS[j,i]=mean((data.casual$casual[folds==j]-predS)^2)  # save error est
  }
}

mse.cvS=apply(cv.errS,2,mean) # compute mean MSE for each number of predictors #2-apply it to rows
min=which.min(mse.cvS)  # find minimum mean MSE
min(mse.cvS)
# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:13,mse.cvS,type="b",xlab="no. of predictors)",ylab="est. test MSE")
points(min,mse.cvS[min],cex=2,col="red",lwd=2)
summary(best.modsS)

#REGISTERED
temp_c=data.registered$temp-mean(data.registered$temp)
hum_c=data.registered$hum-mean(data.registered$hum)
windspeed_c=data.registered$windspeed-mean(data.registered$windspeed)

data.registered$temp2=temp_c*temp_c
data.registered$hum2=hum_c*hum_c
data.registered$windspeed2=windspeed_c*windspeed_c
data.registered$tempxhum=temp_c*hum_c
data.registered$tempxwindspeed=temp_c*windspeed_c
data.registered$humxwindspeed=hum_c*windspeed_c

ModelSR=lm(registered~.,data.registered)
summary(ModelSR)
par(mfrow=c(2,2))
plot(ModelSR)
#Perform CV
k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:5,nrow(data.registered),replace=T) 
# create dummy matrix to store CV error estimates
cv.errS_R=matrix(NA,k,13,dimnames=list(NULL,paste(1:13)))

for (j in 1:k){
  # pick models with lowest RSS with 1-13 predictors fit without kth fold
  best.modsS_R=regsubsets(registered~.,data=data.registered[folds!=j,],
                        nvmax=13,method="exhaustive")
  #estimate test error for all thirteen models by predicting kth fold 
  for (i in 1:13){
    predS_R=pred.sbs(best.modsS_R,data.registered[folds==j,],id=i)
    cv.errS_R[j,i]=mean((data.registered$registered[folds==j]-predS_R)^2)  # save error est
  }
}

mse.cvS_R=apply(cv.errS_R,2,mean) # compute mean MSE for each number of predictors 
min=which.min(mse.cvS_R)  # find minimum mean MSE
min(mse.cvS_R)
# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:13,mse.cvS_R,type="b",xlab="no. of predictors)",ylab="est. test MSE")
points(min,mse.cvS_R[min],cex=2,col="red",lwd=2)
summary(best.modsS_R)

#Reduced second order Model
#Casual 
#We choose model with 8 predictors as suggested by K-Cross CV
data.reduced=data.casual[,c(-8,-11:-14)]
Model3=lm(casual~.,data.reduced)
summary(Model3)
par(mfrow=c(2,2))
plot(Model3)
vif(Model3)



# Box-Cox transformation on data
par(mfrow=c(1,1)) 
boxC=boxcox(Model3) 
lambdaC=boxC$x[which.max(boxC$y)]
lambdaC
data.reduced$casual.box=(data.reduced$casual^lambdaC-1)/lambdaC #new column in data.reduced dataset

Model4=lm(casual.box~temp+hum+windspeed+season1+yr1+workingday1+temp2+hum2,data.reduced)
summary(Model4)
par(mfrow=c(2,2))
plot(Model4)
vif(Model4)

pairs(data.reduced[c(1:3,8:9)])
cor(data.reduced[c(1:3,8:9)])

#Finding MSE for test sample
set.seed(1)
z=data.reduced[,-4]
indx=sample(1:dim(z)[1],50,replace=F)
predC=predict(Model4,newdata=z[indx,])
pred.casual=(predC*lambdaC+1)^(1/lambdaC)
mse.boxcox.casual=mean((data.reduced$casual[indx]-pred.casual)^2)
mse.boxcox.casual

#Reduced Second Model-Registered
#For interpretability we choose Model with 10 predictors even though a model with 13 predictors is suggested
data.reduced_R=data.registered[,c(-8,-11:-14)]
Model3_R=lm(registered~.,data.reduced_R)
summary(Model3_R)
par(mfrow=c(2,2))
plot(Model3_R)
vif(Model3_R)

#Perform CV
k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:5,nrow(data.reduced_R),replace=T) 
# create dummy matrix to store CV error estimates
cv.errR_R=matrix(NA,k,10,dimnames=list(NULL,paste(1:10)))

for (j in 1:k){
  # pick models with lowest RSS with 1-10 predictors fit without kth fold
  best.modsS_R=regsubsets(registered~.,data=data.reduced_R[folds!=j,],
                          nvmax=10,method="exhaustive")
  #estimate test error for all thirteen models by predicting kth fold 
  for (i in 1:10){
    predS_R=pred.sbs(best.modsS_R,data.reduced_R[folds==j,],id=i)
    cv.errR_R[j,i]=mean((data.reduced_R$registered[folds==j]-predS_R)^2)  # save error est
  }
}

mse.cvR_R=apply(cv.errR_R,2,mean) # compute mean MSE for each number of predictors 
min=which.min(mse.cvR_R)  # find minimum mean MSE
min(mse.cvR_R)


# Box-Cox transformation on data
par(mfrow=c(1,1)) 
boxR=boxcox(Model3_R) 
lambdaR=boxR$x[which.max(boxR$y)]
lambdaR
data.reduced_R$registered.box=(data.reduced_R$registered^lambdaR-1)/lambdaR #new column in data.reduced_R dataset

Model4_R=lm(registered.box~temp+hum+windspeed+season1+yr1+workingday1+temp2+hum2,data.reduced_R)
summary(Model4_R)
par(mfrow=c(2,2))
plot(Model4_R)
vif(Model4_R)

pairs(data.reduced_R[c(1:3,8:9)])
cor(data.reduced_R[c(1:3,8:9)])


#Finding MSE for test sample
a=data.reduced_R[,-4]
set.seed(12)
indx=sample(1:dim(a)[1],50,replace=F)
predR=predict(Model4_R,newdata=a[indx,])
pred.registered=(predR*lambdaR+1)^(1/lambdaR)
mse.boxcox.registered=mean((data.reduced_R$registered[indx]-pred.registered)^2)
mse.boxcox.registered

##Ridge Model
#  create predictor matrix and vector for response
data=read.csv("bikes.csv")
data$season1=as.factor(data$season)
data$yr1=as.factor(data$yr)
data$workingday1=as.factor(data$workingday)
data$weathersit1=as.factor(data$weathersit)


#Casual
data.casual=data[,c(-1:-6,-11)]
x=model.matrix(casual~.,data.casual)[,-1] #predictor matrix
y=data.casual$casual #response vector

# create grid for lambda, fit model using all lambdas
grid=10^seq(8,-5,length=100) 
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)  


# plot coefficent values as we change lambda
par(mfrow=c(1,1))
plot(ridge.mod,xlab="L2 Norm")  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.ridge=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.ridge)
bestlam.r=cv.ridge$lambda.min
mse.r=min(cv.ridge$cvm)
bestlam.r
mse.r

# get coefficents for best model
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam.r)
ridge.coef

# fitted values for ridge
fit.ridge=predict(ridge.mod,s=bestlam.r,x)

# R2 for the model
R2.ridge=cor(fit.ridge,data.casual$casual)^2
R2.ridge

#For registered
data.registered=data[,c(-1:-6,-10)]
x=model.matrix(registered~.,data.registered)[,-1] 
y=data.registered$registered  

# create grid for lambda, fit model using all lambdas
grid=10^seq(5,-3,length=100) 
ridge.mod_R=glmnet(x,y,alpha=0,lambda=grid)

# plot coefficent values as we change lambda
par(mfrow=c(1,1))
plot(ridge.mod_R,xlab="L2 Norm")  
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.ridgeR=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.ridgeR)
bestlam.rR=cv.ridgeR$lambda.min
mse.rR=min(cv.ridgeR$cvm)
bestlam.rR
mse.rR

# get coefficents for best model
ridge.coef_R=predict(ridge.mod_R,type="coefficients",s=bestlam.rR)
ridge.coef_R

# fitted values for ridge
fit.ridgeR=predict(ridge.mod_R,s=bestlam.r,x)

# R2 for Model
R2.ridgeR=cor(fit.ridgeR,data.registered$registered)^2
R2.ridgeR


#Lasso Model
#casual
x=model.matrix(casual~.,data.casual)[,-1] #predictor matrix
y=data.casual$casual #response vector

# create grid for lambda, fit model using all lambdas
grid=10^seq(3,-1,length=100)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)  

# plot coefficent values as we change lambda
par(mfrow=c(1,1))
plot(lasso.mod,xlab="L2 Norm")  
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.lasso=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
mse.l=min(cv.lasso$cvm)
bestlam.l
mse.l

# get coefficents for best model
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef

# plotfitted values for the Model
fit.lasso=predict(lasso.mod,s=bestlam.l,x)

# R2 for the Model
R2.lasso=cor(fit.lasso,data.casual$casual)^2
R2.lasso


#For registered
x=model.matrix(registered~.,data.registered)[,-1] #predictor matrix
y=data.registered$registered #response 

# create grid for lambda, fit model using all lambdas
grid=10^seq(3,-2,length=100)  
lasso.modR=glmnet(x,y,alpha=1,lambda=grid)  

# plot coefficent values as we change lambda
par(mfrow=c(1,1))
plot(lasso.modR,xlab="L2 Norm")  
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.lassoR=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.lassoR)
bestlam.lR=cv.lassoR$lambda.min
mse.lR=min(cv.lassoR$cvm)
bestlam.lR
mse.lR

# get coefficents for best model
lasso.coef_R=predict(lasso.modR,type="coefficients",s=bestlam.lR)
lasso.coef_R

# plotfitted values for Lasso
fit.lassoR=predict(lasso.modR,s=bestlam.lR,x)

# R2 for the Model
R2.lassoR=cor(fit.lassoR,data.registered$registered)^2
R2.lassoR

##PCR

#For Casual
data.reduced=data.reduced[,-10] #Removing the box-cox predictor
pcr.mod=pcr(casual~.,data=data.reduced,scale=T,validation="CV")
summary(pcr.mod)
par(mfrow=c(1,1))
validationplot(pcr.mod,val.type="MSEP")


# plotfitted values for OLS and PCR, compare with actual
fit.pcr=predict(pcr.mod,data=data.reduced,ncomp=8)
plot(lmod.casual$fitted.values,data.reduced$casual,pch=19,col="blue")
points(fit.pcr,data.reduced$casual,col="red",lwd=2)
abline(a=0,b=1)

# R2 for the Model
R2.pcr=cor(fit.pcr,data.reduced$casual)^2
R2.pcr

#We remove the categorical data for PCR
pca_casual=prcomp(data.reduced[,-4:-7],scale=T)
summary(pca_casual)

# view PCs and loadings
View(pca_casual$x)
View(pca_casual$rot)

# perform OLS using PC1-PC5
casual_pcr=data.frame(cbind(pca_casual$x,data.reduced$casual))
colnames(casual_pcr)=c("PC1","PC2","PC3","PC4","PC5","casual")
pcr.casualmod=lm(casual~.,data=casual_pcr[,c(1:5,6)])
summary(pcr.casualmod)
par(mfrow=c(2,2))
plot(pcr.casualmod)
vif(pcr.casualmod)

# set up for cross validation
k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:5,nrow(casual_pcr),replace=T) 
# create dummy matrix to store CV error estimates
cv.casual.err=matrix(NA,k,5,dimnames=list(NULL,paste(1:5)))

# perform CV
for (j in 1:k){
  # estimate test error for all five models by predicting kth fold 
  for (i in 1:5){
    lmod.casual.cv=lm(casual~.,data=casual_pcr[folds!=j,c(1:i,6)])
    pred=predict(lmod.casual.cv,casual_pcr[folds==j,])
    cv.casual.err[j,i]=mean((casual_pcr$casual[folds==j]-pred)^2)  # save error est
  }
}

mse.casual.cv=apply(cv.casual.err,2,mean) # compute mean MSE for each number of predictors
min=which.min(mse.casual.cv)  # find minimum mean MSE
min(mse.casual.cv)

par(mfrow=c(1,1))
plot(1:5,mse.casual.cv,type="b",xlab="no. of predictors)",ylab="est. test MSE")
points(min,mse.casual.cv[min],cex=2,col="red",lwd=2)

#Registered
pcr.mod_R=pcr(registered~.,data=data.registered,scale=T,validation="CV")
summary(pcr.mod_R)
par(mfrow=c(1,1))
validationplot(pcr.mod_R,val.type="MSEP")

# plotfitted values for OLS and PCR, compare with actual
fit.pcr_R=predict(pcr.mod_R,data=data.registered,ncomp=8)
plot(lmod.registered$fitted.values,data.registered$registered,pch=19,col="blue")
points(fit.pcr_R,data.registered$registered,col="red",lwd=2)
abline(a=0,b=1)

# R2 for the Model
R2.pcr_R=cor(fit.pcr_R,data.registered$registered)^2
R2.pcr_R

#We remove the categorical data for PCR
pca_registered=prcomp(data.registered[,-4:-8],scale=T)
summary(pca_registered)

# view PCs and loadings
View(pca_registered$x)
View(pca_registered$rot)

# perform OLS using PC1-PC3
registered_pcr=data.frame(cbind(pca_registered$x,data.registered$registered))
colnames(registered_pcr)=c("PC1","PC2","PC3","registered")
pcr.registeredmod=lm(registered~.,data=registered_pcr[,c(1:3,4)])
summary(pcr.registeredmod)
par(mfrow=c(2,2))
plot(pcr.registeredmod)
vif(pcr.registeredmod)

# use 5-fold CV to determine which number of PCs
k=5  
set.seed(123)

folds=sample(1:5,nrow(registered_pcr),replace=T) 

cv.registered.err=matrix(NA,k,3,dimnames=list(NULL,paste(1:3)))

for (j in 1:k){
  
  for (i in 1:3){
    lmod.registered.cv=lm(registered~.,data=registered_pcr[folds!=j,c(1:i,4)])
    pred=predict(lmod.registered.cv,registered_pcr[folds==j,])
    cv.registered.err[j,i]=mean((registered_pcr$registered[folds==j]-pred)^2)  
  }
}

mse.registered.cv=apply(cv.registered.err,2,mean) 
min=which.min(mse.registered.cv)  
min(mse.registered.cv)

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:3,mse.registered.cv,type="b",xlab="no. of predictors)",ylab="est. test MSE")
points(min,mse.registered.cv[min],cex=2,col="red",lwd=2)

# fit best model (using all the data)
pcr.registered.mod=lm(registered~.,data=registered_pcr[,c(1:3,4)])
summary(pcr.registered.mod)
par(mfrow=c(2,2))
plot(pcr.registered.mod)
vif(pcr.registered.mod)

##Regression Trees
#For Casual

require(rpart)
# grow tree
set.seed(123)
tree.mod.casual <- rpart(casual~.,method="anova", data=data.reduced,
                  minsplit=2,maxsurrogate=0)

# plot tree
par(mfrow=c(1,1))
plot(tree.mod.casual, uniform=T, main="Regression Tree for Casual ")
text(tree.mod.casual)
summary(tree.mod.casual) # detailed summary of splits


# display the cross-validation results
printcp(tree.mod.casual) 

# visualize cross-validation results
plotcp(tree.mod.casual)

# create additional plots
par(mfrow=c(1,2)) 
rsq.rpart(tree.mod.casual) # visualize cross-validation results  

# prune the tree
pfitC=prune(tree.mod.casual,
           cp=tree.mod.casual$cptable[which.min(tree.mod.casual$cptable[,"xerror"]),
                               "CP"])

# plot the pruned tree
par(mfrow=c(1,1))
plot(pfitC, uniform=T,
     main="Pruned Regression Tree for Casual")
text(pfitC)

# plotfitted values for OLS and RT, compare with actual
fit.tree.casual=predict(pfitC,data=data.reduced)
plot(lmod.casual$fitted.values,data.reduced$casual,pch=19,col="blue")
points(fit.tree.casual,data.reduced$casual,col="red",lwd=2)
abline(a=0,b=1)


##For Registered

# grow tree
set.seed(123)
tree.mod.registered <- rpart(registered~.,method="anova", data=data.registered,
                         minsplit=2,maxsurrogate=0)

# plot tree
par(mfrow=c(1,1))
plot(tree.mod.registered, uniform=T, main="Regression Tree for Registered ")
text(tree.mod.registered)
summary(tree.mod.registered) # detailed summary of splits


# display the cross-validation results
printcp(tree.mod.registered) 

# visualize cross-validation results
plotcp(tree.mod.registered)

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(tree.mod.registered) # visualize cross-validation results  

# prune the tree
pfitR=prune(tree.mod.registered,
            cp=tree.mod.registered$cptable[which.min(tree.mod.registered$cptable[,"xerror"]),
                                       "CP"])

# plot the pruned tree
par(mfrow=c(1,1))
plot(pfitR, uniform=T,
     main="Pruned Regression Tree for Registered")
text(pfitR)

# plotfitted values for OLS and RT, compare with actual
fit.tree.registered=predict(pfitR,data=data.registered)
plot(lmod.registered$fitted.values,data.registered$registered,pch=19,col="blue")
points(fit.tree.registered,data.registered$registered,col="red",lwd=2)
abline(a=0,b=1)


##Bagging

#For Casual
set.seed(123)
bag.mod.casual=randomForest(casual~.,data=data.reduced,mtry=8, importance=T)
bag.mod.casual
plot(bag.mod.casual)

varImpPlot(bag.mod.casual,type=1,pch=19)

# plotfitted values for OLS and Bagging, compare with actual
plot(lmod.casual$fitted.values,data.reduced$casual,pch=19,col="blue")
points(bag.mod.casual$predicted,data.reduced$casual,col="red",lwd=2)
abline(a=0,b=1)

#Registered
set.seed(123)
bag.mod.registered=randomForest(registered~.,data=data.registered,mtry=7, importance=T)
bag.mod.registered
plot(bag.mod.registered)

varImpPlot(bag.mod.registered,type=1,pch=19)

# plotfitted values for OLS and Bagging, compare with actual
plot(lmod.registered$fitted.values,data.registered$registered,pch=19,col="blue")
points(bag.mod.registered$predicted,data.registered$registered,col="red",lwd=2)
abline(a=0,b=1)

##Random Forest

#For Casual
# tune model parameter mtry using caret
control.c=trainControl(method="cv", number=5, search="grid")
set.seed(123)
tunegrid.c=expand.grid(mtry=c(1:8))
rf_gridsearch.c=train(casual~.,data=data.reduced, method="rf", metric="RMSE", 
                    tuneGrid=tunegrid.c, trControl=control.c)

print(rf_gridsearch.c)
plot(rf_gridsearch.c)

##For better interpretability we choose a model with 3 predictors instead of 4 suggested by RF
set.seed(123)
rf.mod.casual=randomForest(casual~.,data=data.reduced,mtry=3, ntree=1000, 
                    importance=T)
rf.mod.casual
par(mfrow=c(1,1))
plot(rf.mod.casual)

varImpPlot(rf.mod.casual,type=1,pch=19)

# plotfitted values for OLS and RF, compare with actual
plot(lmod.casual$fitted.values,data.reduced$casual,pch=19,col="blue")
points(rf.mod.casual$predicted,data.reduced$casual,col="red",lwd=2)
abline(a=0,b=1)

##Registered
# tune model parameter mtry using caret
control.r=trainControl(method="cv", number=5, search="grid")
set.seed(123)
tunegrid.r=expand.grid(mtry=c(1:7))
rf_gridsearch.r=train(registered~.,data=data.registered, method="rf", metric="RMSE", 
                      tuneGrid=tunegrid.r, trControl=control.r)

print(rf_gridsearch.r)
plot(rf_gridsearch.r)

##For better interpretability we choose a model with 3 predictors instead of 5 suggested by RF
set.seed(123)
rf.mod.registered=randomForest(registered~.,data=data.registered,mtry=3, ntree=1000, 
                           importance=T)
rf.mod.registered
plot(rf.mod.registered)

varImpPlot(rf.mod.registered,type=1,pch=19)

# plotfitted values for OLS and RF, compare with actual
plot(lmod.registered$fitted.values,data.registered$registered,pch=19,col="blue")
points(rf.mod.registered$predicted,data.registered$registered,col="red",lwd=2)
abline(a=0,b=1)

##GBM
#Casual
# create index for random sample of 50
set.seed(123)
indx=sample(1:dim(data.reduced)[1],50,replace=F)

# tune model parameter mtry using caret
control.c=trainControl(method="cv", number=5, search="grid")
set.seed(123)
tunegrid.c=expand.grid(n.trees=c(100,500,1000,2000,5000,7500),
                     interaction.depth=c(1,3,5),
                     shrinkage=c(0.001,0.005,0.01),
                     n.minobsinnode=c(1,3,5))
gb_gridsearch.c=train(casual~.,data=data.reduced[indx,], 
                    method="gbm", metric="RMSE",
                    tuneGrid=tunegrid.c, trControl=control.c)
print(gb_gridsearch.c)
plot(gb_gridsearch.c)

#choose n.tree=5000, int.depth=3, shrink=0.001, minobs=3
set.seed(123)
gb.mod.casual=gbm(casual~.,data=data.reduced[indx,],
           distribution = "gaussian",n.trees = 5000,
           shrinkage = 0.001, interaction.depth = 3, 
           n.minobsinnode=3)

summary(gb.mod.casual,cBars=10)

##Registered
# create index for random sample of 50
set.seed(123)
indx=sample(1:dim(data.registered)[1],50,replace=F)

# tune model parameter mtry using caret
control.r=trainControl(method="cv", number=5, search="grid")
set.seed(123)
tunegrid.r=expand.grid(n.trees=c(100,500,1000,2000,5000,7500),
                       interaction.depth=c(1,3,5),
                       shrinkage=c(0.001,0.005,0.01),
                       n.minobsinnode=c(1,3,5))
gb_gridsearch.r=train(registered~.,data=data.registered[indx,], 
                      method="gbm", metric="RMSE",
                      tuneGrid=tunegrid.r, trControl=control.r)
print(gb_gridsearch.r)
plot(gb_gridsearch.r)

#choose n.tree=7500, int.depth=5, shrink=0.01, minobs=3
set.seed(123)
gb.mod.registered=gbm(registered~.,data=data.registered[indx,],
                  distribution = "gaussian",n.trees = 7500,
                  shrinkage = 0.01, interaction.depth = 5, 
                  n.minobsinnode=3)

summary(gb.mod.registered,cBars=10)

##Comparing Bagging,RF,GBM using sample test MSE
# test set sample Casual
set.seed(12)
indx=sample(1:dim(data.reduced)[1],50,replace=F) #Creating an index for sample data

#Predicting values for response-Casual using different Models
pred.bag.casual=predict(bag.mod.casual,newdata=data.reduced[indx,])
pred.rf.casual=predict(rf.mod.casual,newdata=data.reduced[indx,])
pred.gb.casual=predict(gb.mod.casual,newdata=data.reduced[indx,],n.trees=500)

#Calculating test MSE for random Forest
mse.rf.casual=mean((data.reduced$casual[indx]-pred.rf.casual)^2)
mse.rf.casual


# test set sample Registered
set.seed(123)
indx=sample(1:dim(data.registered)[1],50,replace=F) #Creating an index for sample data

#Predicting values for response-Registered using different Models
pred.rf.registered=predict(rf.mod.registered,newdata=data.registered[indx,])

#Calculating test MSE for Random Forest
mse.rf.registered=mean((data.registered$registered[indx]-pred.rf.registered)^2)
mse.rf.registered


###Calculating Year on Year growth

#Creating a dataset for casual riders with values where yr1 = 0
data1=data.reduced[which(data.reduced$yr1==0),]
#Since the best model is random forest for predicting casual 
#riders, we use this model to predict values for yr1 = 0 
#and then find the mean 
yr0predict.casual=predict(rf.mod.casual,newdata=data1)
mean(yr0predict.casual)
#Creating a dataset with values where yr1 = 1
data2=data.reduced[which(data.reduced$yr1==1),]
#Using the random forest model to predict values for yr1 = 1
#and find the mean
yr1predict.casual=predict(rf.mod.casual,newdata=data2)
mean(yr1predict.casual)

##Creating a dataset for registered with values where yr1 = 0
data3=data.registered[which(data.registered$yr1==0),]
#Since the best model is random forest for predicting registered
#riders, we use this model to predict values for yr1 = 0 
#and then find the mean 
yr0predict.registered=predict(rf.mod.registered,newdata=data3)
mean(yr0predict.registered)
#Creating a dataset with values where yr1 = 1
data4=data.registered[which(data.registered$yr1==1),]
#Using the random forest model to predict values for yr1 = 1
#and find the mean
yr1predict.registered=predict(rf.mod.registered,newdata=data4)
mean(yr1predict.registered)


