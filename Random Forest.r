import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE))
}

write.csv <- function(ob,filename){
  write.table(ob, filename, quote=FALSE, sep=",", row.names=FALSE)
}

house <- import.csv("house.csv")
energy <- import.csv("energy_consumption.csv")
household <- import.csv("household.csv")

house[2:5] <- as.data.frame(apply(house[2:5],2,as.factor))
household[c(2:5,7:9,24:27)]<-as.data.frame(apply(household[c(2:5,7:9,24:27)],2,as.factor))
energy[2:3]<-as.data.frame(apply(energy[,2:3],2,as.factor))

submerge <- merge(energy[,c(1:5,7)],house[,c(1,3:5,7:31)],by='DOEID')
final.merge <- merge(submerge,household[,c(1:10,12:14,23:28)],by='DOEID')

final.merge <- final.merge[-2187,-1]

#walltype
wt <- vector()
for(i in 1:10) {
  index <- which(final.merge$WALLTYPE == i)
  wt <- rbind(wt, mean(final.merge$BTUEL[index])) 
}
wt <- as.data.frame(wt)
wt[is.na(wt)] = 0

library(ggplot2)
library(Rmisc) 

wts <- summarySE(wt, measurevar = "V1")
ggplot(wts, aes(x=index, y=V1)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=V1-ci, ymax=V1+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p <- barplot(wt$V1)

#URBRUR
ur <- vector()
for(i in 1:4) {
  index <- which(final.merge$URBRUR == i)
  ur <- rbind(ur, mean(final.merge$BTUEL[index])) 
}
ur <- as.data.frame(ur)
ur[is.na(ur)] = 0

o <- barplot(ur$V1)


#NHSLDMEM
nh <- vector()
for(i in 1:15) {
  index <- which(final.merge$NHSLDMEM == i)
  nh <- rbind(nh, mean(final.merge$BTUEL[index])) 
}
nh <- as.data.frame(nh)
nh[is.na(nh)] = 0

n <- barplot(nh$V1)


get_pred_rf<-function(train,test){
  set.seed(10)
  nf <- ncol(train)
  test_val<-as.data.frame(test[,-nf])
  names(train)[nf]<-"out"
  #rf = randomForest(out ~ CD65+LRGSTATE+HD65+AGEHHMEMY+TOTCSQFT+RHMCSQFT+TYPEHUQ+TOTRHMSQFT+AGEHHMEM2+TOTSQFT+RHMHSQFT+RHMUCSQFT+TOTHSQFT+NHSLDMEM+HHAGE+WALLTYPE+TOTUCSQFT+TOTUSQFT+TOTBASESQFT+RHMUSQFT ,data=train, mtry=10, importance =TRUE, na.rm=TRUE,ntree = 400)
  rf = randomForest(out ~ .,
                    data=train, mtry=17,nodesize = 16, importance =TRUE, na.rm=TRUE,ntree = 101)
  pred = predict(rf, newdata=test_val)
  return(pred)
  
}


do_cv <- function(df,output,k){
  set.seed(10)
  #df <- df[sample(nrow(df)),] # Shuffle(randomize) the data row wise
  df.intr <- df[, -which(names(df) %in% output)] # Find output column and move it to the last 
  df.intr <- cbind(df.intr,df[output])
  #interval <- nrow(df.intr) / k
  
  
  # A vector of size = nrow of data frame, with numbers 1:k, which helps to fold our data frame.
  k.fold <- cut(seq(1,nrow(df.intr)),breaks=k,labels=FALSE)
  df.act <- data.frame()
  df.act.final <- data.frame()
  mse.vector = vector()
  for(i in (1:k)){
    test <- df.intr[which(k.fold==i,arr.ind=TRUE),] #extracting test data
    train <- df.intr[-which(k.fold==i,arr.ind=TRUE),] #extracting training data
    
    pred <- get_pred_rf(train,test) # call model function(dots or lr or default)
    error <- df.intr[which(k.fold==i,arr.ind=TRUE) ,output] - pred 
    mse.vector[i] <- mean(error*error,na.rm = TRUE) # Calculate MSE for the error
    df.act <- as.data.frame(cbind(df.intr[which(k.fold==i,arr.ind=TRUE),output],  pred)) 
    df.act.final <- as.data.frame(rbind(df.act.final,df.act))
  }
  #print(mean(mse.vector))
  return(df.act.final)
}

result <- do_cv(final.merge,"BTUEL", 10)
#walltype
index1 <- which(final.merge$WALLTYPE == 10)
p1 <- final.merge
p1[index1,"WALLTYPE"] = 4
p1 <- do_cv(p1,"BTUEL", 10)
result1 <- p1[index1,]
compare1<- result[index1,]

#URBRUR
index2 <- which(final.merge$URBRUR == 4)
p2 <- final.merge
p2[index2,"URBRUR"] = 1
p2 <- do_cv(p2,"BTUEL", 10)
result2 <- p2[index2,]
compare2<- result[index2,]

#NHSLDMEM
index3 <- which(final.merge$NHSLDMEM == 13)
p3 <- final.merge
p3[index3,"NHSLDMEM"] = 1
p3 <- do_cv(p3,"BTUEL", 10)
result3 <- p3[index3,]
compare3<- result[index3,]
#


