## Data Visualization
## DIMENSION REDUCION

# install.packages("factoextra")
library(factoextra)

lab <- read.csv2("LabMeasurements-Color-Card.csv")

# function will determine best k via elbow method
fviz_nbclust(lab, kmeans, method = "wss")
# => elbow is at cluster = 3

# Run a K-means clustering with 3 clusters and plot the clusters for first 2 principal components
km.out<-kmeans(lab,centers=3,nstart=1) #run with three clusters
pr.out<-prcomp(lab,scale=TRUE)
plot(pr.out$x[,1:2],type="n", main="546 Clustered Color Cards", ylab="PC2")
text(pr.out$x[,1:2],col=km.out$cluster)
