import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE))
}

write.csv <- function(ob,filename){
  write.table(ob, filename, quote=FALSE, sep=",", row.names=FALSE)
}

house <- import.csv("house.csv")
#house[2:5] <- as.data.frame(apply(house[2:5],2,as.factor))
#house <- house[,c(1,3:5,7:31)]

energy <- import.csv("energy_consumption.csv")
#energy[2:3]<-as.data.frame(apply(energy[,2:3],2,as.factor))
#energy <- energy[,c(1:5,7)]

household <- import.csv("household.csv")
#household[c(2:5,7:9,24:27)]<-as.data.frame(apply(household[c(2:5,7:9,24:27)],2,as.factor))
#household <- household[,c(1:10,24:28)]



eh <- merge(house, energy, by.x = "DOEID", by.y = "DOEID")
ehh <- merge(eh, household, by.x = "DOEID", by.y = "DOEID")

ehh <- ehh[,-1]

library("rpart")
part <- rpart(BTUEL ~. , data = ehh, method = "anova")
summary(part)

plotcp(part)
plot(part, uniform=TRUE, main="Classification Tree")
text(part, use.n=TRUE, all=TRUE, cex=.8)


library(randomForest)
fit <- randomForest(BTUEL ~ .,  data=ehh, na.action = na.omit, 
                    importance = TRUE, ntree = 200)

varImpPlot(fit)
getTree(fit, 1)

library(party)


print(fit)
imp <- importance(fit)
imp <- imp[order(as.numeric("IncNodePurity"))]

draft1 <- droplevels(ehh[,c("BTUEL","CD65", "NHSLDMEM","HD65","LRGSTATE","TOTCSQFT","TYPEHUQ",
                 "RHMCSQFT","TOTRHMSQFT","RHMHSQFT","HHAGE","TOTUCSQFT","TOTHSQFT",
                 "MONEYPY", "TOTSQFT")])

l1 <- lm(BTUEL~., data = draft1)

as.data.frame(l1)
plot(l1, draft1$BTUEL)
library(ggplot2)

do_cv <- function(df,output,k,model){
  df <- df[sample(nrow(df)),] # Shuffle(randomize) the data row wise
  df.intr <- df[, -which(names(df) %in% output)] # Find output column and move it to the last 
  df.intr <- cbind(df.intr,df[output])
  #interval <- nrow(df.intr) / k
  
  
  # A vector of size = nrow of data frame, with numbers 1:k, which helps to fold our data frame.
  k.fold <- cut(seq(1,nrow(df.intr)),breaks=k,labels=FALSE)
  mse.vector = vector()
  for(i in (1:k)){
    test <- df.intr[which(k.fold==i,arr.ind=TRUE),] #extracting test data
    train <- df.intr[-which(k.fold==i,arr.ind=TRUE),] #extracting training data
    
    pred <- model(train,test) # call model function(dots or lr or default)
    error <- df.intr[which(k.fold==i,arr.ind=TRUE) ,output] - pred 
    mse.vector[i] <- mean(error*error,na.rm = TRUE) # Calculate MSE for the error
    qplot(x=test[,1],y=test[,48])
    
    #lines(x = test[,1], y = pred, col = 'green')
  }
  return(mse.vector)
}


get_pred_lr <- function(train,test){
  nf <- ncol(train) # find total columns in Training data
  model <- lm(paste(names(train)[nf], '~ .'),data = train) # linear regression on the last column using all the other ones
  return(predict.lm(model,newdata = test)) # predicting the values of test data using the model generated above
  
}

do_cv(ehh[-2187,],'BTUEL',10,get_pred_lr)

for(i in 1:30) {
  tuneRF(x = draft1,y = draft1[,1], ntreetry=15, mtryStart=15, 
         stepFactor=20, nodesize=i)
}

ntreeTry <- c(seq(1, 199, 10), seq(200, 399, 20), seq(400, 500, 30))
tuneRF(x = draft1[,-1],y = draft1[,1], ntreeTry, plot = TRUE, na.action = na.omit)

tuneRF(x = draft1,y = draft1[,1], ntreetry=ntreeTry, mtryStart=10, nodesize=3)

#///////
house[2:5] <- as.data.frame(apply(house[2:5],2,as.factor))
household[c(2:5,7:9,24:27)]<-as.data.frame(apply(household[c(2:5,7:9,24:27)],2,as.factor))
energy[2:3]<-as.data.frame(apply(energy[,2:3],2,as.factor))




## Merge to make one dataset
submerge <- merge(energy[,c(1:5,7)],house[,c(1,3:5,7:31)],by='DOEID')
final.merge <- merge(submerge,household[,c(1:10,12:14,23:28)],by='DOEID')

## Remove the first row of DOEID and the error row 2187
final.merge <- final.merge[-2187,-1] 



vec <- numeric()
for(i in 1:10){
  ab <- tuneRF(draft1[,-1],draft1[,1] , ntreeTry=10*i, plot = TRUE)
  vec <- rbind(vec, as.numeric(ab[which.min(ab[,2]),]['OOBError']))
}
for(i in 5:20){
  ab <- tuneRF(final.merge[,-5],final.merge[,5] , ntreeTry=20*i, plot = TRUE)
  vec <- rbind(vec, as.numeric(ab[which.min(ab[,2]),]['OOBError']))
}
for(i in 20:25){
  ab <- tuneRF(draft1[,-1],draft1[,1] , ntreeTry=20*i, plot = TRUE)
  vec <- rbind(vec, as.numeric(ab[which.min(ab[,2]),]['OOBError']))
}
plot(vec)




#######svm roc
library(e1071)
final.merge<-import.csv("final.csv")
final.merge <- transform(final.merge,BTUEL = as.factor(BTUEL))
nf<-ncol(final.merge)
num<-which(names(final.merge)=="BTUEL")
final.merge<- final.merge[, c(1:num-1,(num+1):(nf),num)]
get_pred_svm <- function(train,test){
  nf <- ncol(train)
  # change the output variable name to "y"
  colnames(train)<-c(colnames(train)[-nf],"y")
  colnames(test)<-c(colnames(test)[-nf],"y")
  # build a SVM model using the train data
  svmmodel <- svm(y ~ ., data = train,probability=TRUE )
  # use the model to get the predictions using the input variables of the test data
  prediction <- predict(svmmodel, test[-nf],probability=TRUE )
  pred <- attr(prediction,"probabilities")[,1]
  # the true output values
  true <- test[,nf]
  return(data.frame(pred,true))
}


do_cv_class<-function(df,num_folds,model_name){
  # randomize data
  set.seed(1234)
  df <- df[sample(nrow(df)),]
  kk <- num_folds
  folds <- cut(seq(1,nrow(df)),breaks=kk,labels=FALSE)
  finalresult <- vector()
  # sparse the modedl name if it is knn 

    for (i in 1:kk) {
      indexes <- which(folds==i,arr.ind=TRUE)
      itest <- df[indexes,]
      itrain <- df[-indexes,]
      total <- get_pred_svm(itrain, itest)
      finalresult <- rbind(finalresult,total)
    }
    return(finalresult)
  
}


library(arules)

svm.x <- do_cv_class(final.merge,10,"get_pred_svm")
pred <- prediction(svm.x$pred, svm.x$true)
ROC_perf <- performance(pred,"tpr","fpr")
ROC_perf2 <- performance(pred,"tnr","fnr")
auc <- performance(pred, "auc")
plot(ROC_perf)

final.merge$wt1 <- ifelse(final.merge$WALLTYPE==1, 1, 0)
final.merge$wt2 <- ifelse(final.merge$WALLTYPE==2, 1, 0)
final.merge$wt3 <- ifelse(final.merge$WALLTYPE==3, 1, 0)
final.merge$wt4 <- ifelse(final.merge$WALLTYPE==4, 1, 0)
final.merge$wt5 <- ifelse(final.merge$WALLTYPE==5, 1, 0)
final.merge$wt6 <- ifelse(final.merge$WALLTYPE==6, 1, 0)
final.merge$wt7 <- ifelse(final.merge$WALLTYPE==7, 1, 0)
final.merge$wt8 <- ifelse(final.merge$WALLTYPE==8, 1, 0)
final.merge$wt9 <- ifelse(final.merge$WALLTYPE==9, 1, 0)
final.merge$wt10 <- ifelse(final.merge$WALLTYPE==10, 1, 0)


dalist = list()
for(i in 1:10) {
  column <- paste(c("wt",i), collapse = "")
  index <- which(final.merge$column == 1)
  dalist[[i]] = mean(final.merge$BTUEL[index])
}
index <- which(final.merge$wt1 == 1)
tmp <- mean(final.merge$BTUEL[index])
