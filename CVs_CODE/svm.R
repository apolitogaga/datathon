library(kernlab)
library(caret)
library(MASS)
mydata <- read.table("splice/Dataset.data", header=FALSE, sep=" ")
colnames(mydata)<-c("y", 1:60 )
## CHECK IF ALL VARIABLES ARE FACTORS
for( i in 1:61) {
  print( is.factor(mydata[,i]) ) 
}

##RANDOM SPLIT IN TRAINING (70%) AND TEST (30%)
set.seed(666)
trainIndex <- createDataPartition(mydata[,"y"], p = .7,
                                  list = FALSE,
                                  times = 1)
training<-mydata[trainIndex,]
test<-mydata[-trainIndex,]

##########CROSS VALIDATION FOR LINEAR KERNEL
###########################################
library(TunePareto) # for generateCVRuns()
k <- 10

cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")
cv.folds <- generateCVRuns(training[,], ntimes=1, nfold=k, stratified=FALSE)


#these are the values for the parameter cost that we will test
costs<-c(0.001,0.01,0.1,1,10, 20,50,75,100,500)
cost.results <- matrix (rep(0,5*length(costs) ),nrow=length(costs))
colnames(cost.results)<-c("COST", "avg TR error", "avg VA error","sd TR error","sd VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k
set.seed(666)

for(l in 1:length(costs)) {
  for(i in 1:k) {
    
    va <- unlist(cv.folds[[1]][[i]])
    svm.model<-ksvm( training[-va,"y"]~. , data=training[-va,2:61] , type="C-svc", kernel="vanilladot",
                     C=costs[l] , kpar=list(), scaled=FALSE)
    pred<-predict(svm.model, training[va,] )
    
    cv.results[i,"TR error"] <-error(svm.model)
    cf<- confusionMatrix(training[va,"y"], pred) 
    cv.results[i,"VA error"] <- 1-( cf$overall[[1]] )
    cv.results[i,"fold"] <- i
  }
  averageCVError<-mean(cv.results[,4] )
  averageTRError<-mean(cv.results[,3])
  cost.results[l,"COST"]<-costs[l]
  cost.results[l,"avg TR error"]<-averageTRError
  cost.results[l,"avg VA error"]<-averageCVError
  cost.results[l,"sd TR error"]<-sd(cv.results[,3])
  cost.results[l,"sd VA error"]<-sd(cv.results[,4])
  
}
cost.results
# HERE WE WRITE THE RESULTS OF THE CROSS VALIDATION IN A FILE
write.table(cost.results, file="SVM Cost LinearCV.txt")
###best c=0.1

##########CROSS VALIDATION FOR POLINOMIAL KERNEL
#LETS FIND THE BEST DEGREE FOR THE POLINOMIAL
k<-10

cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")
###these are the degrees that we will test on the CV
d<-c(1,2,3,4,5,6)
cost.results <- matrix (rep(0,5*length(d) ),nrow=length(d))
colnames(cost.results)<-c("D", "avg TR error", "avg VA error","sd TR error","sd VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k


#cv.folds <- generateCVRuns(training[,17], ntimes=1, nfold=k, stratified=TRUE)
for(l in 1:length(d)) {
  for(i in 1:k) {
    
    va <- unlist(cv.folds[[1]][[i]])
    svm.model<-ksvm( training[-va,"y"]~. , data=training[-va,2:61] , 
                     type="C-svc", kernel="polydot", scaled=FALSE, C=0.1,kpar=list(d=d[l]))
    pred<-predict(svm.model, training[va,] )
    
    cv.results[i,"TR error"] <-error(svm.model)
    cf<- confusionMatrix(training[va,"y"], pred) 
    cv.results[i,"VA error"] <- 1-( cf$overall[[1]] )
    
    cv.results[i,"fold"] <- i
  }
  averageCVError<-mean(cv.results[,4] )
  averageTRError<-mean(cv.results[,3])
  cost.results[l,"D"]<-d[l]
  cost.results[l,"avg TR error"]<-averageTRError
  cost.results[l,"avg VA error"]<-averageCVError
  cost.results[l,"sd TR error"]<-sd(cv.results[,3])
  cost.results[l,"sd VA error"]<-sd(cv.results[,4])
}
cost.results
#LET'S WRITE THE RESULTS IN A TEXT FILE
write.table(cost.results, file="SVM D POLY CV.txt")
###NO BETTER RESULTS WITH THE POLYNOMIAL KERNEL


#### 3RD CROSS VALIDATION: RBF KERNEL
#CHOOSE THE BEST COST PARAMETER
library(TunePareto) # for generateCVRuns()
k <- 10

## We need to resort to cross-validation to get more meaningful numbers
## prepare the structure to store the partial results

cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")
costs<-c(0.001,0.01,0.1,1,10, 20,50,75,100,500)
cost.results <- matrix (rep(0,5*length(costs) ),nrow=length(costs))
colnames(cost.results)<-c("COST", "avg TR error", "avg VA error","sd TR error","sd VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k

#cv.folds <- generateCVRuns(training[,17], ntimes=1, nfold=k, stratified=TRUE)
for(l in 1:length(costs)) {
  for(i in 1:k) {
    
    va <- unlist(cv.folds[[1]][[i]])
    svm.model<-ksvm( training[-va,"y"]~. , data=training[-va,2:61] , type="C-svc", kernel="rbfdot",
                     C=costs[l], kpar=list(sigma=1))
    pred<-predict(svm.model, training[va,] )
    
    cv.results[i,"TR error"] <-error(svm.model)
    cf<- confusionMatrix(training[va,"y"], pred) 
    cv.results[i,"VA error"] <- 1-( cf$overall[[1]] )
    cv.results[i,"fold"] <- i
  }
  averageCVError<-mean(cv.results[,4] )
  averageTRError<-mean(cv.results[,3])
  cost.results[l,"COST"]<-costs[l]
  cost.results[l,"avg TR error"]<-averageTRError
  cost.results[l,"avg VA error"]<-averageCVError
  cost.results[l,"sd TR error"]<-sd(cv.results[,3])
  cost.results[l,"sd VA error"]<-sd(cv.results[,4])
  
}
cost.results
write.table(cost.results, file="SVM RBF Cost CV.txt")
####BEST C=1

#############CROSS VALIDATION SIGMA PARAMETER
k<-10

cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")

sigma<-c(0.0001,0.0005,0.001, 0.005, 0.01, 0.05, 0.1,0.5,1)
cost.results <- matrix (rep(0,5*length(sigma) ),nrow=length(sigma))
colnames(cost.results)<-c("SIGMA", "avg TR error", "avg VA error","sd TR error","sd VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k
#cv.folds <- generateCVRuns(training[,17], ntimes=1, nfold=k, stratified=TRUE)
for(l in 1:length(sigma)) {
  for(i in 1:k) {
    
    va <- unlist(cv.folds[[1]][[i]])
    svm.model<-ksvm( training[-va,"y"]~. , data=training[-va,] , 
                     type="C-svc", kernel="rbfdot", C=1,scale=FALSE, kpar=list(sigma=sigma[l]))
    pred<-predict(svm.model, training[va,] )
    
    cv.results[i,"TR error"] <-error(svm.model)
    cf<- confusionMatrix(training[va,"y"], pred) 
    cv.results[i,"VA error"] <- 1-( cf$overall[[1]] )
    cv.results[i,"fold"] <- i
  }
  averageCVError<-mean(cv.results[,4] )
  averageTRError<-mean(cv.results[,3])
  cost.results[l,"SIGMA"]<-sigma[l]
  cost.results[l,"avg TR error"]<-averageTRError
  cost.results[l,"avg VA error"]<-averageCVError
  cost.results[l,"sd TR error"]<-sd(cv.results[,3])
  cost.results[l,"sd VA error"]<-sd(cv.results[,4])
}
cost.results
write.table(cost.results, file="SVM RBF SIGMA CV.txt")
##BEST 0.005

#### 4th CROSS VALIDATION: LAPLACE KERNEL
#CHOOSE THE BEST COST PARAMETER
library(TunePareto) # for generateCVRuns()
k <- 10

## We need to resort to cross-validation to get more meaningful numbers
## prepare the structure to store the partial results

cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")
costs<-c(0.1,1,10,20,50,75,100,200,500,1000,5000)
cost.results <- matrix (rep(0,5*length(costs) ),nrow=length(costs))
colnames(cost.results)<-c("COST", "avg TR error", "avg VA error","sd TR error","sd VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k

#cv.folds <- generateCVRuns(training[,17], ntimes=1, nfold=k, stratified=TRUE)
for(l in 1:length(costs)) {
  for(i in 1:k) {
    
    va <- unlist(cv.folds[[1]][[i]])
    svm.model<-ksvm( training[-va,"y"]~. , data=training[-va,2:61] , type="C-svc", kernel="laplacedot",
                     C=costs[l], kpar=list(sigma=1))
    pred<-predict(svm.model, training[va,] )
    
    cv.results[i,"TR error"] <-error(svm.model)
    cf<- confusionMatrix(training[va,"y"], pred) 
    cv.results[i,"VA error"] <- 1-( cf$overall[[1]] )
    cv.results[i,"fold"] <- i
  }
  averageCVError<-mean(cv.results[,4] )
  averageTRError<-mean(cv.results[,3])
  cost.results[l,"COST"]<-costs[l]
  cost.results[l,"avg TR error"]<-averageTRError
  cost.results[l,"avg VA error"]<-averageCVError
  cost.results[l,"sd TR error"]<-sd(cv.results[,3])
  cost.results[l,"sd VA error"]<-sd(cv.results[,4])
  
}
cost.results
write.table(cost.results, file="SVM LAPLACE Cost CV.txt")
######## BEST C=1
## NOW LET'S FIND THE BEST SIGMA
#############CROSS VALIDATION SIGMA PARAMETER
k<-10

cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")

sigma<-c(0.0001,0.0005,0.001, 0.005, 0.01, 0.05, 0.1,0.5,1)
cost.results <- matrix (rep(0,5*length(sigma) ),nrow=length(sigma))
colnames(cost.results)<-c("SIGMA", "avg TR error", "avg VA error","sd TR error","sd VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k
#cv.folds <- generateCVRuns(training[,17], ntimes=1, nfold=k, stratified=TRUE)
for(l in 1:length(sigma)) {
  for(i in 1:k) {
    
    va <- unlist(cv.folds[[1]][[i]])
    svm.model<-ksvm( training[-va,"y"]~. , data=training[-va,] , 
                     type="C-svc", kernel="laplacedot", C=1,scale=FALSE, kpar=list(sigma=sigma[l]))
    pred<-predict(svm.model, training[va,] )
    
    cv.results[i,"TR error"] <-error(svm.model)
    cf<- confusionMatrix(training[va,"y"], pred) 
    cv.results[i,"VA error"] <- 1-( cf$overall[[1]] )
    cv.results[i,"fold"] <- i
  }
  averageCVError<-mean(cv.results[,4] )
  averageTRError<-mean(cv.results[,3])
  cost.results[l,"SIGMA"]<-sigma[l]
  cost.results[l,"avg TR error"]<-averageTRError
  cost.results[l,"avg VA error"]<-averageCVError
  cost.results[l,"sd TR error"]<-sd(cv.results[,3])
  cost.results[l,"sd VA error"]<-sd(cv.results[,4])
}
cost.results
write.table(cost.results, file="SVM LAPLACE SIGMA CV.txt")
####################################

system.time( ksvm( training[,"y"]~., 
                   data=training[,2:61], scale=FALSE, kernel="rbfdot" ,kpar=list(sigma=0.005),C=1,type="C-svc"), gcFirst=TRUE )
system.time( ksvm( training[,"y"]~., 
                   data=training[,2:61], scale=FALSE, kernel="laplacedot" ,kpar=list(sigma=0.05),C=1,type="C-svc"), gcFirst=TRUE )


model<-lda(formula=training[,"y"]~., data=training[,2:61], CV=FALSE)

ab<-ksvm(training[,"y"]~., data=training[,2:61], scale=FALSE, kernel="rbfdot" ,kpar=list(sigma=0.005),C=1,type="C-svc")
pred<-predict(ab, test[,2:61])
pred<-predict(model, test[,2:61])$class
confusionMatrix(pred,test[,"y"])


#################################
##################################
############SOME PLOTS###########
#################################
a <- 0.01666667
s <-   0.05270463
n <- 2224
error <- qnorm(0.975)*s/sqrt(n)
left <- a-error
right <- a+error


table <- read.table("SVM Cost LinearCV.txt", header=TRUE, sep=" ")
table<-table[1:4,] # We cut some results for a better reading of the graph
#plot(linear$COST, linear$avg.TR.error, type="l", col="red")
plot(x=table$COST, y=c(0,0,0,0.10), type="l" ,col="white", 
     main="SVM-Linear Kernel CV ", xlab="Cost Parameter", ylab="Error")
abline(v=0.1, col="grey")
points(x=table$COST, y=table$avg.TR.error, type="l" ,col="blue")
points(x=table$COST, y=table$avg.VA.error, type="l" ,col="red")
legend("topright", c("TR err", "Val err"),lty=c(1,1) ,col=c(4,2))
################################################################
table <- read.table("SVM d POLY CV.txt", header=TRUE, sep=" ")
table<-table[1:4,] # We cut some results for a better reading of the graph
#plot(linear$COST, linear$avg.TR.error, type="l", col="red")
plot(x=table$D, y=c(0,0,0,0.15,0,0), type="l" ,col="white", 
     main="SVM-Poly Kernel CV ", xlab="Degree", ylab="Error")
abline(v=1, col="grey")
points(x=table$D, y=table$avg.TR.error, type="l" ,col="blue")
points(x=table$D, y=table$avg.VA.error, type="l" ,col="red")
legend("topleft", c("TR err", "Val err"),lty=c(1,1) ,col=c(4,2))
#####################################################
par(mfrow=c(1,2))
table2<-read.table(file="SVM RBF Cost CV.txt")
table2<-table2[1:5,] # We cut some results for a better reading of the graph
plot(x=table2$COST, y=c(-0.00001,0,0,0.50,0), type="l" ,col="white", 
     main="RBF_Kernel CV For COST", xlab="Cost Parameter", ylab="Error")
points(x=table2$COST, y=table2$avg.TR.error, type="l" ,col="blue")
points(x=table2$COST, y=table2$avg.VA.error, type="l" ,col="red")
abline(v=1, col="grey")

table2<-read.table(file="SVM RBF SIGMA CV.txt")
table2<-table2[1:5,]
plot(x=table2$SIGMA, y=c(-0.00001 ,0,0.090,0,0), type="l" ,col="white", 
     main="SVM-RBF_Kernel CV For  SIGMA, C=1", xlab="Sigma Parameter", ylab="Error")
points(x=table2$SIGMA, y=table2$avg.TR.error, type="l" ,col="blue")
points(x=table2$SIGMA, y=table2$avg.VA.error, type="l" ,col="red")
abline(v=0.005, col="grey")
#legend("topright", c("TR err", "Val err"),lty=c(1,1) ,col=c(4,2))
par(mfrow=c(1,2))
table2<-read.table(file="SVM LAPLACE Cost CV.txt")
table2<-table2[1:4,] # We cut some results for a better reading of the graph
plot(x=table2$COST, y=c(-0.00001,0,0,0.50), type="l" ,col="white", 
     main="LAPLACE CV For COST", xlab="Cost Parameter", ylab="Error")
points(x=table2$COST, y=table2$avg.TR.error, type="l" ,col="blue")
points(x=table2$COST, y=table2$avg.VA.error, type="l" ,col="red")
abline(v=1, col="grey")

table2<-read.table(file="SVM LAPLACE SIGMA CV.txt")
table2<-table2[2:9,]
plot(x=table2$SIGMA, y=c(-0.00001 ,0,0.40,0,0,0,0,0), type="l" ,col="white", 
     main="LAPLACE CV For  SIGMA, C=1", xlab="Sigma Parameter", ylab="Error")
points(x=table2$SIGMA, y=table2$avg.TR.error, type="l" ,col="blue")
points(x=table2$SIGMA, y=table2$avg.VA.error, type="l" ,col="red")
abline(v=0.05, col="grey")
