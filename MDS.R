## Data Visualization
## First Project - Douglas Color Cards
## MDS
# The main goal of MDS it is to plot multivariate data points in two dimensions,
# thus revealing the structure of the dataset by visualizing the relative distance
# of the observations.

master <- read.csv2("MasterColorCard.csv") # CMYKS colors are in p1- p5 (neglect?)
lab <- read.csv2("LabMeasurements-Color-Card.csv")

# Plot the master card's color spots with their field number labels into a 2D space
master <- as.matrix(master)[, 9:11]
mds <- cmdscale(dist(master))
plot(mds)
plot(mds, type = 'n')
text(mds[, 1], mds[, 2])#, labels(master))


# Plot the labs means of all color spots with their field number labels into a 2D space
# calculate the lab measurements means
mlab <- colMeans(x = lab)
mlab <- as.matrix(mlab)[3:194]
mlab <- t(mlab)
mlab <- as.data.frame(matrix(unlist(mlab, use.names=FALSE),ncol=3, byrow=TRUE))
colnames(mlab) <- c("L", "a", "b")
mlab <- as.matrix(mlab)
mds.ml <- cmdscale(dist(mlab))
plot(mds.ml)
plot(mds.ml, type = 'n')
text(mds.ml[, 1], mds.ml[, 2], col="red")


# Plot both (master and mean lab results) into one space
plot(mds.ml, type = 'n', main="Master and Mean Lab Results compared")
text(mds[, 1], mds[, 2], cex=0.8)
text(mds.ml[, 1], mds.ml[, 2], col="red", cex=0.8)
legend(x = 40 , y = 51,
       legend = c("Master", "Mean Lab Results"), 
       fill = c("black", "red"), 
       bty = "n", 
       cex = 0.8)

# add connecting lines into plot
segments(mds[, 1], mds[, 2], mds.ml[, 1], mds.ml[, 2] ,col = par("fg"), lty = par("lty"), lwd = par("lwd"))
