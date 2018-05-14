## Data Visualization
## First Project - Douglas Color Cards
## DIMENSION REDUCION

setwd('C:/Users/Bina/Desktop/DataScience/2.Semester/DataVis/FirstProject_Douglas')

lab <- read.csv2("LabMeasurements-Color-Card.csv")

# Run a K-means clustering with 4 clusters and plot the clusters for first 2 principal components
km.out<-kmeans(lab,centers=3,nstart=1) #run with four clusters
pr.out<-prcomp(lab,scale=TRUE)
plot(pr.out$x[,1:2],type="n", main="546 Clustered Color Cards", ylab="PC2")
text(pr.out$x[,1:2],col=km.out$cluster)
