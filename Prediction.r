get_pred_fr<-function(train,test){
  nf <- ncol(train)
  test_val<-as.data.frame(test[,-nf])
  names(train)[nf]<-"out"
  #tune.rf = tuneRF(x=train, y=train[,nf], ntreetry=100, mtryStart=10, 
  #                 stepFactor=20, nodesize=5)
  # algorithm chose mtry = 10
  rf = randomForest(out ~., data=train, ntreeTry = 100, 
                    importance =TRUE, na.rm=TRUE)
  pred = predict(rf, newdata=test_val)
  return(pred)
}   

get_pred_lr <- function(train,test){
  nf <- ncol(train) # find total columns in Training data
  model <- lm(paste(names(train)[nf], '~ .'),data = train) # linear regression on the last column using all the other ones
  return(predict.lm(model,newdata = test)) # predicting the values of test data using the model generated above
  
}

do_cv <- function(df,output,k,func)
{
  total_row<-nrow(df)
  set.seed(10)
  #Shuffles the data
  df <- df[sample(total_row),]
  #Creating folds when k is passed
  fold<-cut(seq(1,nrow(df)),breaks=k, labels=FALSE)
  nf<-ncol(df)
  num<-which(names(df)==toString(trimws(output)))
  #makes the output column as the last column of the dataframe 
  df<- df[, c(1:num-1,(num+1):(nf),num)]   
  MSE<-rep(NA,k)
  #nf_test<-ncol(test)
  #divides data frame into k parts and one part is used in each iteration for testing and MSE is calculated
  for (i in 1:k)
  { 
    index <- which(fold==i,arr.ind=TRUE)     #Source : http://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
    test  <- df[index, ] 
    train <- df[-index, ]
    nf <- ncol(train)
    result<-func(train,test)
    MSE[i]<- mean((result- (test[[output]]))^2 , na.rm = TRUE)
  }
  
  return (MSE)
}

merge30 <- final.merge[,c("BTUEL","CD65", "LRGSTATE", "HD65", "RHMCSQFT", "AGEHHMEMY","TYPEHUQ", "TOTCSQFT", "TOTRHMSQFT", "HHAGE", 
                          "RHMHSQFT","NHSLDMEM","TOTSQFT","AGEHHMEM2","WALLTYPE","TOTHSQFT", "RHMUCSQFT",
                          "TOTUCSQFT","MONEYPY", "URBRUR", "GARUCSQFT", "BASHSQFT", "TOTUSQFT",
                          "SPOUSE", "RETIREPY", "TOTATTCSQFT", "TOTBASESQFT", "RHMUSQFT", "BASUCSQFT",
                          "YEARMADE", "AGEHHMEM3")]

merge10 <- final.merge[,c("BTUEL","CD65", "LRGSTATE", "HD65", "RHMCSQFT", "AGEHHMEMY","TYPEHUQ", "TOTCSQFT", "TOTRHMSQFT", "HHAGE", 
                          "RHMHSQFT")]

merge15 <- final.merge[,c("BTUEL","CD65", "LRGSTATE", "HD65", "RHMCSQFT", "AGEHHMEMY","TYPEHUQ", "TOTCSQFT", "TOTRHMSQFT", "HHAGE", 
                          "RHMHSQFT","NHSLDMEM","TOTSQFT","AGEHHMEM2","WALLTYPE","TOTHSQFT")]


merge20 <- final.merge[,c("BTUEL","CD65", "LRGSTATE", "HD65", "RHMCSQFT", "AGEHHMEMY","TYPEHUQ", "TOTCSQFT", "TOTRHMSQFT", "HHAGE", 
                          "RHMHSQFT","NHSLDMEM","TOTSQFT","AGEHHMEM2","WALLTYPE","TOTHSQFT", "RHMUCSQFT",
                          "TOTUCSQFT","MONEYPY", "URBRUR", "GARUCSQFT" )]



hundred30 <- final.merge[,c("BTUEL","CD65", "LRGSTATE", "HD65", "RHMCSQFT", "AGEHHMEMY","TYPEHUQ", "TOTCSQFT", "TOTRHMSQFT", "HHAGE", 
                            "RHMHSQFT","NHSLDMEM","TOTSQFT","AGEHHMEM2","WALLTYPE","TOTHSQFT", "RHMUCSQFT",
                            "TOTUCSQFT","MONEYPY", "URBRUR", "GARUCSQFT", "BASHSQFT", "TOTUSQFT",
                            "SPOUSE", "RETIREPY", "TOTATTCSQFT", "TOTBASESQFT", "BASUCSQFT", "SDESCENT", "ATTUSQFT", "NCASHBEN"
)]

library(randomForest)
v <- do_cv(hundred30, "BTUEL", 10, get_pred_lr)
mean(v)

