## Data Visualization
## First Project - Douglas Color Cards
## MDS

setwd('C:/Users/Bina/Desktop/DataScience/2.Semester/DataVis/FirstProject_Douglas')
master <- read.csv2("MasterColorCard.csv") # CMYKS colors are in p1- p5 (neglect?)
lab <- read.csv2("LabMeasurements-Color-Card.csv")

# Plot the master card's color spots with their field number labels into a 2D space
master <- as.matrix(master)[, 9:11]
mds <- cmdscale(dist(master))
plot(mds)
plot(mds, type = 'n')
text(mds[, 1], mds[, 2])#, labels(master))

# Plot the labs first row's color spots with their field number labels into a 2D space
lab <- as.matrix(lab)[1,3:194]
lab <- t(lab)
lab <- as.data.frame(matrix(unlist(lab, use.names=FALSE),ncol=3, byrow=TRUE))
colnames(lab) <- c("L", "a", "b")
lab <- as.matrix(lab)
mds.l <- cmdscale(dist(lab))
plot(mds.l)
plot(mds.l, type = 'n')
text(mds.l[, 1], mds.l[, 2], col="red")

# Plot both (master and first lab result) into one space
plot(mds.l, type = 'n', main="Master and Lab Result compared")
text(mds[, 1], mds[, 2], cex=0.8)
text(mds.l[, 1], mds.l[, 2], col="red", cex=0.8)

# add connecting lines into plot
segments(mds[, 1], mds[, 2], mds.l[, 1], mds.l[, 2] ,col = par("fg"), lty = par("lty"), lwd = par("lwd"))
