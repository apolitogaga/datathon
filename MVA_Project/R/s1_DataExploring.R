###################################################################
###                                                               #
###         MultiVariate     Analysis      Project                #
###                       Adult Project                           #
###       Héctor Apolo Rosales Pulido & Rocco Proscia             #
###                                                               #
###################################################################
#########################################################################################################
## libraries
#########################################################################################################
library(nortest)
library(MASS)
require(npmc)
library(discretization)
#########################################################################################################
## step 1 to explore the data and work with outliers or null fields
#########################################################################################################
adultD <- read.table("data/adult.data", head=FALSE, sep=",")
adultDT <- read.table("data/adult.test", head=FALSE, sep=",")
levels(adultDT$V15) <- c(" <=50K"," >50K")
adult<-rbind(adultD, adultDT)# 48796 size
#set the variable names.
columns <- c("age", "workclass","fnlwgt","education","education.num","martial.status","occupation","relationship","race"
             ,"sex","capital.gain","capital.loss","hours.per.week","native.country","income")
colnames(adult) <- columns 
summary(adult)

########################################### WORK CLASS #################################################
#summary(adult$workclass)
#levels(adult$workclass)
# we will add a new category for workclass
levels(adult$workclass) <- c("noWClass","Federal-gov","Local-gov","Never-worked","Private",
                             "Self-emp-inc","Self-emp-not-inc","State-gov","Without-pay")
#summary(adult$workclass)
#########################################################################################################
###########################################  Occupation #################################################
#summary(adult$occupation)
#levels(adult$occupation)
# we will add a new category for workclass
levels(adult$occupation) <- c("noOccupation","Adm-clerical","Armed-Forces","Craft-repair","Exec-managerial"
  , "Farming-fishing", "Handlers-cleaners","Machine-op-inspct","Other-service","Priv-house-serv"
  , "Prof-specialty" ,"Protective-serv","Sales","Tech-support","Transport-moving")
#summary(adult$occupation)
#########################################################################################################
###########################################   country   #################################################
#summary(adult$native.country)
#levels(adult$native.country)
# we will add a new category for workclass
levels(adult$native.country) <- c("noCountry","Cambodia","Canada","China"              
                ,"Colombia","Cuba","Dominican-Republic","Ecuador"
                ,"El-Salvador","England","France","Germany"
                ,"Greece","Guatemala","Haiti","Holand-Netherlands"
                ,"Honduras","Hong","Hungary","India"
                ,"Iran","Ireland","Italy","Jamaica"
                ,"Japan","Laos","Mexico","Nicaragua"                
                ,"Outlying-US(Guam-USVI-etc)","Peru","Philippines","Poland"                   
                ,"Portugal","Puerto-Rico","Scotland","South"                    
                ,"Taiwan","Thailand","Trinadad&Tobago","United-States"            
                ,"Vietnam","Yugoslavia")
levels(adult$native.country) <- c("noCountry","Asia","United-States","Asia"              
                                  ,"LatinAmerica","LatinAmerica","LatinAmerica","LatinAmerica"
                                  ,"LatinAmerica","Europe","Europe","Europe"
                                  ,"Europe","LatinAmerica","LatinAmerica","Europe"
                                  ,"LatinAmerica","Asia","Europe","Asia"
                                  ,"Asia","Europe","Europe","LatinAmerica"
                                  ,"Asia","Asia","Mexico","LatinAmerica"                
                                  ,"United-States","LatinAmerica","Asia","Europe"                   
                                  ,"Europe","United-States","Europe","Asia"                    
                                  ,"Asia","Asia","LatinAmerica","United-States"            
                                  ,"Asia","Europe")

chisq.test(adult$native.country,adult$income)

chisq.test(adult$fnlwgt,adult$income)
str(adult)
levels(adult$native.country)
table(adult$native.country,adult$income)
#summary(adult$native.country)
#########################################################################################################
########################################  Purge variables  ##############################################
summary (subset(adult, (adult$occupation=="noOccupation"&adult$"native.country"=="noCountry"&adult$workclass=="noWClass")))
indexToDelete<-as.numeric( rownames( subset(adult, ( adult$occupation=="noOccupation" & adult$"native.country"=="noCountry"&
                                                       adult$workclass=="noWClass" ) ) ) )
#DELETED THE ROWS WITH 3 VARIABLES NULL
adult<-adult[-indexToDelete,] # we end u with 48796 n size of Adult.

################################## Discretize continuous variables ##########################################

str(adult)
adult$age <- (cut(adult$age, breaks = c(15,25,35, 45,55, 65, Inf), 
                  labels = c("young","youngAdult","adult","oldAdult","senior","retired")))
table(adult$age)
adult$hours.per.week <- cut(adult$hours.per.week, breaks = c(0, 20,30,40,50, 60, Inf), 
                            labels = c("underEmployed","lowNormal","normal","highNormal","overworked","slaveLabor"))

adult[[ "capital.gain"]] <- ordered(cut(adult[[ "capital.gain"]],
                      c(-Inf,0,median(adult[[ "capital.gain"]][adult[[ "capital.gain"]]>0]),
                      Inf)), labels = c("None", "Low", "High"))
table(adult$capital.gain)

adult[[ "capital.loss"]] <- ordered(cut(adult[[ "capital.loss"]],
                      c(-Inf,0, median(adult[[ "capital.loss"]][adult[[ "capital.loss"]]>0]),
                      Inf)), labels = c("None", "Low", "High"))
table(adult$native.country,adult$income)

ad.test(adult$fnlwgt)


table(adult$capital.gain,adult$income)
table(adult$capital.loss,adult$income)

plot(adult$capital.gain)
plot(adult$capital.loss)
table(adult$hours.per.week)
levels(adult$race)
levels(adult$martial.status)
#########################################################################################################
#########################################################################################################
################################## OutlierDetection ##########################################
adult[,c("age","fnlwgt", "education.num","hours.per.week")]
library(chemometrics)
adult.cont <- adult[,c("age","fnlwgt", "education.num","hours.per.week")]
hist(adult$"capital.gain")
hist(adult$"capital.loss")
mahal<-Moutlier(adult.cont, plot=TRUE)
#########################################################################################################

############################## CHECKPOINT 1. ##############################
############################## clean data and categories ##############################

saveRDS(adult, file="data/AdultCleaned.Rda")
adult <- readRDS(file="data/AdultCleaned.Rda")

#***************************************** PROBABLY USELESS *********************************************
wilcox.test(y~A) 
names(adult)
table(adult$capital.gain,adult$income)
aovEdu <- aov(education.num~education,data=adult)
aovEdu
lm1 <- lm(adult$education.num~adult$income)
ad.test(adult$education.num)
qqplot(adult$education.num,residuals(lm1))
table(adult$education,adult$education.num)


glm(adult$capital.gain~adult$income)
summary(lm(adult$capital.loss~adult$income))
xad.test(aovEdu)

glm(income~.,data=adult[,-c(12,11,5)])

hist(log(adult$education.num))
kruskal.test(education.num~education,data=adult)
kruskal.test(education.num~native.country,data=adult)



t.test(capital.gain~income,data=adult)
wilcox.test(capital.gain~income,data=adult) 
?wilcox.test
?kruskal.test
kruskal.test(education.num~adult$martia,data=adult)
?


summary(adult)
#capital gain has errors?
table(adult['capital.gain']==99999) # 159 equal to 99999 none bigger.
# this could be  the biggest number in the census form.
boxplot(adult$"capital.gain")
nrow( subset(adult, adult$"capital.gain">99000 ))
summary(adult)
levels(adult$education)

#########################################################################################################
####################################### Feature Selection ###############################################

chisq<-chiSq(adult)


######################################## deleting native.country ########################################


plot(adult$native.country)
nCountTable<-table(adult$native.country)
cbind(nCountTable,prop.table(nCountTable)*100) # we see that United States have almost 90% of the sample.
# also this is good for us because we have 43 levels in this factor, which will become a computational
# challenge later on.
adult = adult[-14] # delete native.country
#########################################################################################################
kruskal.test(capital.loss~income,data=adult)
adult = adult[-12] # delete capital.loss
library(lmtest)
dwtest(adult$capital.loss~adult$income)
dwtest(adult$capital.loss~rownames(adu))
adult = adult[-11] # delete capital.gain



######################################## deleting native.country ########################################
table(adult$education,adult$education.num)
adult = adult[-5] # delete education.num because is redundant, and will increase our bias
adult = adult[-3] # Delete fnlwgt

saveRDS(adult, file="data/AdultCleaned2.Rda")
adult <- readRDS(file="data/AdultCleaned2.Rda")

chisq.test(adult$age, adult$income)
for (i in 1:14) {
 print( chisq.test(adultD[i], adultD$income ,simulate.p.value = T, B=999)$p.value )
}
?Moutlier

?boxplot
#######################################Analysis#########################################################
boxplot(adult$"capital.gain", xlab="capital gain")
boxplot(adult$"capital.loss", xlab="capital loss")
boxplot(adult$"hours.per.week", xlab="hours per week")
attach(adultD)# to work with the data without having to access the dF
detach(adult)
summary(ocupation)
summary(workclass)
subset(adultD, workclass="?"  )
adultD$workclass=="?"
levels(ocupation)
?subset
subset(adult, adult$"native.country"==" ?")
summary(adult)
summary(adult$"native.country" )
?plot
adult$income==" <=50K."
levels(education)
mahal<-Moutlier(adult[,c("age", "fnlwgt", "capital.gain", "capital.loss", "hours.per.week" )], plot=TRUE)
Moutlier(adult[,c("age", "fnlwgt")] )


t.test(income~native.country,data=adult)


#########################################################################################################
#############################################  MCA  #####################################################
library(FactoMineR)

par(mfrow=c(2,2))
res.mca <- MCA( bar,quali.sup = bar[,10])

str(bar)
bar<-bar[,-3]
summary(bar)
str(adult)
mean<-mean(res$eig$eigenvalue)
barplot(res$eig$eigenvalue)
table( res$eig$eigenvalue>mean )
#NOW 31 DIMENSIONS ARE GOOD
dimdesc(res, axes=1:6, proba=0.05)
##### Save data for later use instead of running all the code
saveRDS(adult, file="data/AdultPruned.Rda")
############################## CHECKPOINT 2. ##############################
adult <- readRDS(file="data/AdultPruned.Rda")
#########################################################################################################

#age and income are not independent
chisq.test(adult$education,adult$income) 
#age and income are not independent
chisq.test(adult$sex,adult$income)
#age and income are not independent
chisq.test(adult$adult,adult$income)
#age and income are not independent
chisq.test(adult$occupation,adult$relationship)

require(dplyr)
library(kernlab)
adultM1 <- glm (income ~ ., data=adult, family=binomial)
adultM1.AIC <- step (adultM1)

mutate(group_by(adult$native.country,size),am_pcnt = amount/sum(amount))
?plot
plot(adult$native.country)
plot(adult$race)
(mytable<-table(adult$native.country))

summary(adult$age)

for(i in 1:12){
  print(chisq.test(adult[i],adult$income)$p.value)
}

library(e1071)
model <- svm(income~., data=adult)
?svm

###### CHECKPOINT 2. #########
bar <- readRDS(file="data/AdultPruned.Rda")
summary(bar)
bar<-bar[-3]
bar<-bar[-10]
summary(bar)
###CHISQUARE, IS GOOD???!?!?!?
chisq.test(bar$age, bar$race)

x<-sample(nrow(bar),5000)
mat<-as.matrix(bar[1:5000,])
library(cluster)

saveRDS(res, file="data/MCAOUTPUT31DIMENSIONS")
mca.res<-readRDS(file="data/MCAOUTPUT31DIMENSIONS")
#d<-daisy(mca.res$ind$coord[1:15000,], metric="euclidean")
?dist
d<-dist( mca.res$ind$coord[1:20000,], method="euclidean")
gc() # USE THIS INSTRUCTION FOR FREE THE MEMORY WHEN U GO OUTOFBOUNDS!!! VERY GOOD
saveRDS(d, file="data/distanceMatrix20000TrainingMCA")
d<-readRDS(file="data/distanceMatrix20000TrainingMCA")
##LETS DO SOME CLUSTERING
clustering<-hclust(d, method="ward.D2")
?hclust
plot(clustering)
barplot(clustering$height[19980:20000])
library(rattle)
MCAPowa<-MCA( adult[c(1:12000,33000:36000),], quali.sup=adult$income , ncp=31, ind.sup=33000:36000) 
adultA<-adult[1:17000,]
adultB<-adult[39001:42000,]
for(i in 1:13) {
  for( j in 1:nlevels(adultB[,i])) {
    if(nrow( subset( adultB, adultB[,i]==levels(adultB[,i])[j] ) )<2) {
      print(nrow( subset( adultB, adultB[,i]==levels(adultB[,i])[j] ) ) )
    }
  }
  for( j in 1:nlevels(adultA[,i])) {
    if(nrow( subset( adultA, adultA[,i]==levels(adultA[,i])[j] ) )<2) {
      print(nrow( subset( adultA, adultA[,i]==levels(adultA[,i])[j] ) ) )
    }
  }
  print(levels(adultA[,i])==levels(adultB[,i]) )
#  print( levels(adultB[,i]) )
}

str(adultSubset)
adultSubset<-rbind(adultA, adultB)
rownames(adultSubset)<-1:20000
str(adultSubset)
dioporco<-NULL
MCAPowa<-MCA( adultSubset[1:17000,] , quali.sup=adultSubset$income, ncp=31  ) 
?MCA
c(1:5,9:17)
res<-NULL
#centroids<-centers.hclust(d , clustering, nclust=6, use.median=FALSE) IS GOOD BUT TOO MANY RAM IT CRASH
c1<-cutree(clustering, 6)
cdg<-aggregate(as.data.frame( res$ind$coord[1:20000,]), list(c1), mean)[,2:32]
kmeans<-kmeans(res$ind$coord[1:20000,], cdg)
kmeanstest<-kmeans(res$ind$coord[30000:36000,], cdg)
library(FactoMineR)
#catdes(cbind( mca.res$ind$coord[1:15000,], as.factor(kmeans$cluster)) , num.var=13 )
prof <- catdes(cbind(as.factor(kmeans$cluster), adult[1:20000,] ), num.var=1, proba = 0.05)
plot(prof$category)
plot(res$ind$coord[1:20000,1:2], col=kmeans$cluster+1)
legend('bottomright', c("1","2","3","4","5","6") ,pch=1,col=c(2,3,4,5,6,7) , bty='n', cex=.75)

plot(res$ind$coord[30000:36000, 1:2], col=kmeanstest$cluster+1)
legend('bottomright', c("1","2","3","4","5","6") ,pch=1,col=c(2,3,4,5,6,7) , bty='n', cex=.75)


?legend
?c

?plot
library(plot3D)
res

adultset<-bar[1:20000,]
#res is the MCA with factominer
summary(res)
mean<-mean(res$eig$eigenvalue) 
res$eig$eigenvalue>mean
MCA32<-MCA(adultset, ncp=32, quali.sup=13: 13, graph = FALSE)
d<-dist(MCA32$ind$coord, method="euclidean")
saveRDS(d, file="data/distanceMatrix20000TrainingMCA")

clustering<-hclust(d, method="ward.D2")
barplot(clustering$height[19980:20000])
c1<-cutree(clustering, 6)
cdg<-aggregate(as.data.frame( MCA32$ind$coord), list(c1), mean)[,2:33]
kmeans<-kmeans(MCA32$ind$coord, cdg)
prof <- catdes(cbind(as.factor(kmeans$cluster), adult[1:20000,] ), num.var=1, proba = 0.05)
plot(MCA32$ind$coord[,1:2], col=kmeans$cluster+1)
legend('bottomright', c("1","2","3","4","5","6") ,pch=1,col=c(2,3,4,5,6,7) , bty='n', cex=.75)

adultA<-adultset
adultB<-bar[30001:33000,]

adultD<-rbind(adultA, adultB)
rownames(adultD)<-1:23000
MCA322<-MCA(adultD, ncp=32, quali.sup=13: 13, ind.sup=20001:23000,graph = FALSE)

saveRDS(MCA322, file="data/MCAwithTestSupplementary")
kmeansTEST<-kmeans(MCA322$ind.sup$coord, cdg)
profTEST <- catdes(cbind(as.factor(kmeansTEST$cluster), adultB ), num.var=1, proba = 0.05)
plot(MCA322$ind.sup$coord[,1:2], col=kmeansTEST$cluster+1)
legend('bottomright', c("1","2","3","4","5","6") ,pch=1,col=c(2,3,4,5,6,7) , bty='n', cex=.75)

