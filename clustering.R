#1.east west airlines data 

install.packages("readxl")
library(readxl)
data1 <- read_excel(file.choose())

head(data1)

data1 <- as.data.frame(data1)
View(data1)
dim(data1)

cor(data1)
pairs(data1)

#Variables which are -vely correlated : Looking at the correlation plot , we can see that Balance and Qual_miles,Balance and Bonus_miles, Qual_miles and Bonus_miles,Flight_miles and Balance, Qual_miles and Flight_miles
#Qual_miles and Flight_trans_12, Balance and Flight_trans_12 are negatively correlated with each other

#Variables which are +vely correlated :Balance and Days_since_enroll, Bonus_trans and Flight_trans_12, Flight_miles_12mo and Flight_trans_12

summary(data1)

par(mfrow =c(3,3))
boxplot(data1$Balance,horizontal = T,main="Balance")
boxplot(data1$Qual_miles,horizontal = T,main="Qual_miles")
boxplot(data1$cc1_miles,horizontal = T,main="cc1_miles")
boxplot(data1$cc2_miles,horizontal = T,main="cc2_miles")
boxplot(data1$cc3_miles,horizontal = T,main="cc3_miles")
boxplot(data1$Bonus_miles,horizontal = T,main="Bonus_miles")
boxplot(data1$Bonus_trans,horizontal = T,main="Bonus_trans")
boxplot(data1$Flight_miles_12mo,horizontal = T,main="Flight_miles_12mo")
boxplot(data1$Flight_trans_12,horizontal = T,main="Flight_trans_12")
boxplot(data1$Days_since_enroll,horizontal = T,main="Days since enroll")
boxplot(data1$`Award?`,horizontal = T,main="Award")

head(data1)

#Balance, Qual_miles, Bonus_miles, Bonus_trans,Flight_miles_12mo,Flight_trans_12 shows outliers data. Also the distribution for these variables is skewed



newdata <- scale(data1[,2:12])
newdata <-as.data.frame(newdata)

head(newdata)
dim(newdata)

#Hirarichal clustering
d<- dist(newdata,method = "euclidean")
d

fit<- hclust(d,method = "average")
plot(fit)



#First find referance value for k using formula k ~ sqrt(n/2)

sqrt(3999/2)
#45

# To determine number of clusters

install.packages("NbClust")
library(NbClust)

nc <- NbClust(newdata,min.nc = 2,max.nc = 15,method = "kmeans")

nc

table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),xlab ="No. of clusters",ylab = "No.of Criterian") 


###find Optimum K value using below formula 

wss <- c()

length(wss)

for (i in 2:45) wss[i] <-sum(kmeans(newdata,centers = i)$withinss)

plot(1:45,wss,type = "b",xlab = "No.of clusters",ylab="Avg distance")


#Using elbow plot we have got 2 clusters. So ,put 2 in the below formula

#Cluster Algorithm building
airline <- kmeans(newdata,2) 

airline$size
airline$centers
airline$cluster

airline

airline_clusters = data.frame('ID'=data1[,1],'Cluster'=airline$cluster)
View(airline_clusters)

#Attach Segment to original data file
data1$Segment <- airline_clusters$Cluster
head(data1)



#Interpretation
#Cluster 1 has the highest Balance number of miles eligible for awards  
#Cluster 1 has the highest number Number of miles counted as qualifying for Topflight status 
#Cluster 1 has the highest member earned miles with airline freq. flyer credit card in the past 12 months 
#Cluster 2 has the highest member earned miles with Rewards credit cards in the past 12 months 
#Cluster 1 has the highest member earned miles with Small Business credit card 
#Cluster 1 has the highest Number of miles earned from non-flight bonus transactions 
#Cluster 1 has the highest Number of non-flight bonus transactions in the past 12 months 
#Cluster 1 has the highest Number of flight miles in the past 12 months 
#Cluster 1 has the highest Number of flight transactions in the past 12 months 
#Cluster 1 has the highest Number of days since enrolled in flier program 
#Cluster 1 has the highest award flight (free flight) 
#Overall Cluster 1 is the best

##################################################################################################################################################################################

#2. crime data
#To see the mean values for each group of cluster using aggregate function

aggregate(cbind(Balance,Qual_miles,cc1_miles,cc3_miles,Bonus_miles,Bonus_trans,Flight_miles_12mo,Flight_trans_12,Days_since_enroll)~Segment,data=data1,mean)


#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

#Data Description:
#Murder -- Muder rates in different places of United States
#Assualt- Assualt rate in different places of United States
#UrbanPop - urban population in different places of United States
#Rape - Rape rate in different places of United States

mydata1 <- read.csv(file.choose())

head(mydata1)

cor(mydata1[,-1])
pairs(mydata1[,-1])

#Murder and Assault ,Murder and Rape , Assault and Rape are positively correlated. Also as urban popn increases , Murder , Assault and Rape cases is showning an increasing trend

summary(mydata1)

boxplot(mydata1$Murder,main="Murder",horizontal = T)
boxplot(mydata1$Assault,main="Assault",horizontal = T)
boxplot(mydata1$UrbanPop,main="UrbanPop",horizontal = T)
boxplot(mydata1$Rape,main="Rape",horizontal = T)

#Variable Rape shows outliers data


newdata_crime <- scale(mydata1[,2:5])

dim(newdata_crime)
head(newdata_crime)

newdata_crime<-as.data.frame(newdata_crime)

head(newdata_crime)

d<- dist(newdata_crime,method = "euclidean")
d

fit <- hclust(d,method = "average")
fit
plot(fit)

groups.5 <- cutree(fit,k=5)#cut tree into 5 clusters

#Draw a dendogram with red borders around 5 clusters

rect.hclust(fit, k=5, border = "red")

table(groups.5)

#To see which countries are there in 1st cluster 5 cluster solution

head(mydata1)
mydata1$X[groups.5==1]

#if we want to do the same thing for all the groups at once, we can use sapply:

sapply(unique(groups.5),function(g)mydata1$X[groups.5 == g])


counts = sapply(2:6,function(ncl)table(cutree(fit,ncl)))
names(counts) = 2:6
counts


table(mydata1$X,groups.5)

#To see the median values for each group of cluster using agrregate function

a1 = aggregate(mydata1[,-c(1)],list(groups.5),median)
a1

data.frame(Cluster=a1[,1],Freq=as.vector(table(groups.5)),a1[,-1])


#Attach the cluster numbers to Uni

head(mydata1)
clusters = data.frame('Country'=mydata1[,1],'Cluster'=groups.5)

View(clusters)

mytable <- table(clusters$Country,clusters$Cluster)

mytable


install.packages("gmodels")
library(gmodels)

CrossTable(clusters$Country,clusters$Cluster)

###K-Means Clusters code##################

#First find referance value for k using formula k ~ sqrt(n/2)
sqrt(50/2)
#k=5 in this case

###find Optimum K value using below formula (k=5 put in range 2:15)

wss <- c()

for (i in 2:15) wss[i] <-sum(kmeans(newdata_crime,centers = i)$withinss)

plot(1:15,wss,type = "b",xlab = "No.of clusters",ylab="Avg distance")

#Using elbow plot we have got 4 clusters. So ,put 4 in the below formula

#Cluster Algorithm building
crime <- kmeans(newdata_crime,4) 


crime$size
crime$centers
crime$cluster

clusters = data.frame('Country'=mydata1[,1],'Cluster'=crime$cluster)

View(clusters)

#Check the number of clusters
library(NbClust)

nc <- NbClust(newdata_crime,min.nc = 2,max.nc = 15,method = "kmeans")

table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),xlab ="No. of clusters",ylab = "No.of Criterian") 


#Interpretation
# Cluster 4 has got highest murder crimes  and Cluster 2 includes countries that has got the lowest murder crimes
# Cluster 3 has got highest Assault crimes  and Cluster 2 includes countries that has got the lowest murder crimes
# Cluster 3 has got the highest urban population compared to Cluster 1 and 2 
# Cluster 3 has got the highest Rape cases while cluster 2 includes countries with lowest Rape cases
# Countries with cluster 2 seem to have less crime compared to countries in cluster 1,3 and 4.




