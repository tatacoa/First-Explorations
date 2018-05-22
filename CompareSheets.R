## Data Visualization
## First Project - Color Cards

lab.measure <- read.csv2("LabMeasurements-Color-Card.csv")
master.color <- read.csv2("MasterColorCard.csv") 
load("deltae94.Rda")
CIE94 <- lab.distances;rm(lab.distances)

sheet.number <- {}
k <- 1
for (i in 1:546) {
  if (k>13) {
    k <- 1
  }
  sheet.number[i] <- k
  k <- k +1
}
CIE94 <- cbind(sheet.number, CIE94)

sheet.stats = data.frame(index=integer(),
                         mean=double(),
                         variance=double())


for (i in 1:13) {
  sheet.data <- CIE94[which(CIE94$sheet.number == i),]
  colnames(sheet.data) <- colnames(CIE94)
  sheet.stats[i,1] <- i
  # calculating the mean of all distances of all pixels of all cards on each sheet
  sheet.stats[i,2] <- mean(apply(sheet.data[4:67],1, mean))
  # calculating the variance (1 = mean of variances inside the cards, or 2 = mean of variances inside colors)
  sheet.stats[i,3] <- mean(apply(sheet.data[4:67],2, var))
}

plot(sheet.stats$mean, sheet.stats$variance, type= "n")
text(sheet.stats$mean, sheet.stats$variance, 1:13)
# this basically tells us that sheet 8 is, speaking of the mean of all distances, the farthest away from the master card.
# Sheet 8 also contains the largest variance among its colors.
