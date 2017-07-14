get_pred_lr <- function(train,test){
  set.seed(10)
  nf <- ncol(train) # find total columns in Training data
  model <- lm(paste(names(train)[nf], '~ GARHSQFT + BASCSQFT + ATTCSQFT + home.ty.2to4 + 
    wall.wood + wall.siding + wall.stone + wall.other + wall.indesc + 
                    yearmade.1940plus + yearmade.1950plus + yearmade.1970plus + 
                    yearmade.1980plus + yearmade.1985plus + yearmade.1990plus + 
                    yearmade.2003 + yearmade.2004 + neigh.city + neigh.town + 
                    neigh.suburbs + employment.unemp + ncashben.yes + moneypy.l2500 + 
                    moneypy.2500plus + moneypy.5000plus + moneypy.7500plus + 
                    moneypy.10000plus + moneypy.15000plus + moneypy.20000plus + 
                    moneypy.25000plus + moneypy.30000plus + moneypy.35000plus + 
                    moneypy.40000plus + moneypy.45000plus + moneypy.50000plus + 
                    moneypy.55000plus + moneypy.70000plus + moneypy.85000plus + 
                    moneypy.90000plus + moneypy.95000plus + moneypy.100000plus + 
                    PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC12 + PC13 + PC14 + 
                    PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + 
                    PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC33 + 
                    PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + 
                    PC43 + PC44 + PC45 + PC51 + PC52 + PC53 + PC54 + PC61'),data = train) # linear regression on the last column using all the other ones
  return(predict.lm(model,newdata = test)) # predicting the values of test data using the model generated above
  
}

get_pred_rf<-function(train,test){
  nf <- ncol(train)
  test_val<-as.data.frame(test[,-nf])
  names(train)[nf]<-"out"
  rf = randomForest(out ~ .,
                    data=train, mtry=17,nodesize = 16, importance =TRUE, na.rm=TRUE,ntree = 101)
  pred = predict(rf, newdata=test_val)
  return(pred)
} 

do_pca <- function(df,output,k,model){
  set.seed(10)
  df.intr <- df[, -which(names(df) %in% output)] # Find output column and move it to the last 
  df.intr <- cbind(df.intr,df[output])
  k.fold <- cut(seq(1,nrow(df.intr)),breaks=k,labels=FALSE)
  df.act <- data.frame()
  df.act.final <- data.frame()
  mse.vector = vector()
  rmse.vector = vector()
  r.squared = vector()
  for(i in (1:k)){
    test <- df.intr[which(k.fold==i,arr.ind=TRUE),] #extracting test data
    train <- df.intr[-which(k.fold==i,arr.ind=TRUE),] #extracting training data
    pred <- model(train,test) # call model function(dots or lr or default)
    error <- df.intr[which(k.fold==i,arr.ind=TRUE) ,output] - pred
    actual <- df.intr[which(k.fold==i,arr.ind=TRUE) ,output]
    r.squared[i] <- 1 - (sum(error* error)/sum((actual-mean(actual))^2))
    mse.vector[i] <- mean(error*error,na.rm = TRUE) # Calculate MSE for the error
    rmse.vector[i] <- sqrt(mean(error*error,na.rm = TRUE)) 
    df.act <- as.data.frame(cbind(df.intr[which(k.fold==i,arr.ind=TRUE),output],  pred)) 
    df.act.final <- as.data.frame(rbind(df.act.final,df.act))
  }
  print(mean(mse.vector))
  print(mean(rmse.vector))
  print(mean(r.squared))
  return(mean(rmse.vector))
}
df.new <- num.merge[, !names(num.merge) %in% corr.column]
df.new <- cbind(df.new, pc.result$x[,1:43])

mse.new <- do_pca(df.new,'BTUEL',10,get_pred_lr)
mse.new <- do_pca(df.new,'BTUEL',10,get_pred_rf)

mse.total <- vector()
for(i in 1:61) {
  df.new <- num.merge[, !names(num.merge) %in% corr.column]
  df.new <- cbind(df.new, pc.result$x[,1:i])
  mse.total[i] <- do_pca(df.new,'BTUEL',10, get_pred_lr)
}
#
model <- lm(BTUEL ~ ., data = df.new)
step <- stepAIC(model, direction = "backward", trace = FALSE)


#386361927-----used forward selection and lr
