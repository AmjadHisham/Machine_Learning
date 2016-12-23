
setwd("D:\\Work\\Training\\Ivy Training\\Clustering")
#install.packages("amap") 
library(amap)
#Read the data in the file
cust_data<-read.csv("Insurance_Dataset_Clustering_Analysis.csv")
# Select the requried columns for clustering
cust_data<-cust_data[,c(1,2,4,5,10)]

#Verify the data
colnames(cust_data)
head(cust_data)

#Run the kmeans algorithm to generate the clusters
k1<-Kmeans(cust_data[,-c(1)], 3, iter.max = 4000, nstart = 1, method = c("euclidean"))

#See the clustering results
#Fetch the group means for each variable
k1$centers

#Fetch size/n of obs for the groups
k1$size

#Fetch the cluster for each obs
k1$cluster

#Attach the cluster to the original dataset
cust_data[,"final_seg"] <- data.frame(k1$cluster)


