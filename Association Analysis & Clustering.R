#Association Analysis and Clustering

#Problem 14.2
#Association analysis

#installing necessary packages
install.packages("arules")
library(arules)

#reading the file
courses.df<-read.csv("Coursetopics.csv")
View(courses.df)

#converting df into matrix
courses.mat<-as.matrix(courses.df)
courses.mat

#converting the matrix into a transactions database
courses.trans <- as(courses.mat, "transactions")
inspect(courses.trans)

#plotting data
itemFrequencyPlot(courses.trans)

#getting rules
rules <- apriori(courses.trans, parameter = list(supp = 0.05, conf = 0.2, minlen=2,target = "rules"))
inspect(sort(rules, by = "lift"))

#Problem 15.2
#Reading the file:
pharma.df<-read.csv("Pharmaceuticals.csv")
View(pharma.df)

#Setting row names to the pharma dataframe
row.names(pharma.df) <- pharma.df[,1]

#Normalizing all numerical variables
pharma.df.norm<-sapply(pharma.df[3:11],scale)

#Setting row names for normalized df
row.names(pharma.df.norm)<-row.names(pharma.df)
View(pharma.df.norm)

#Computing euclidean distance 
d.norm <- dist(pharma.df.norm, method = "euclidean")
d.norm

#Hierarchical clustering using wards.d method
hc<-hclust(d.norm,method="ward.D")
plot(hc,hang = -1, ann = FALSE)

#Cutting two clusters
memb<- cutree(hc, k = 2)
memb

#Computing means 
pharma.df<-cbind(memb,pharma.df.norm)
View(pharma.df)

pharma<-cbind(pharma.df,memb)
aggregate(pharma.df.norm,list(cluster=memb),mean)
