## Data Visualization
## First Project - Color Cards
## This R file aims

library(colorspace)

lab.measure <- read.csv2("LabMeasurements-Color-Card.csv")
master.color <- read.csv2("MasterColorCard.csv") 
load("deltae94.Rda")
CIE94 <- lab.distances;rm(lab.distances)

# attach sheet numbers to distance deltae data frame
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

# identify cell with the largest distance between lab and master color
CIE94[which.max(apply(CIE94,1,max)), which.max(apply(CIE94,2,max))]
# extract row and column values
max.color <- c(which.max(apply(CIE94,1,max)), which.max(apply(CIE94,2,max))); max.color
# extract column name i.e. color card pixel position
pixel.number <- colnames(CIE94[max.color[2]]); pixel.number
# extract sheet number
sheet.number <- CIE94[max.color[1], 1]; sheet.number
# build column name, e.g. "L12", "a42"
col1 <- paste(c("L", pixel.number), collapse = "")
col2 <- paste(c("a", pixel.number), collapse = "")
col3 <- paste(c("b", pixel.number), collapse = "")
# collect Lab value
Lab <- c(lab.measure[max.color[1],col1], lab.measure[max.color[1],col2], lab.measure[max.color[1],col3])
# transform Lab value to hexadecimal color
lab.hex <- hex(LAB(Lab[1],Lab[2],Lab[3]), fixup = T)
# create hexadecimal color of master card
master.hex <- hex(LAB(master.color[which(master.color$Crow == 3 & master.color$Ccol == 2), "L"],master.color[which(master.color$Crow == 3 & master.color$Ccol == 2), "a"],master.color[which(master.color$Crow == 3 & master.color$Ccol == 2), "b"]), fixup=T)
# build image
image(matrix(1:2,nrow=2),col=c(lab.hex, master.hex))



allMax <- matrix(NA,nrow = 64, ncol=3, byrow = TRUE)
for (i in 4:67) {
  print(i)
  allMax[i-3, 1] <- which.max(CIE94[,i])
  allMax[i-3, 2] <- i-3
  allMax[i-3, 3] <- max(CIE94[,i])
  print(CIE94[max(CIE94[,i]),i])
}

# function to display a color card. Expects a distance matrix of 64:2, 
# with rowvalue being the row value of CIE94 and column value being the column value of CIE94 (as in 3~13,64~88)
colorcardDisplay <- function(pixels){
  pixel.numbers <- matrix(NA, nrow=64, ncol=1, byrow=TRUE)
  for (i in 1:64){
    pixel.numbers[i,1] <- colnames(CIE94[pixels[i, 2]+3])
  }
  pixels <- cbind(pixels, pixel.numbers)

  pixel.hex <- matrix(NA, nrow=64, ncol=2, byrow=T)
  for (i in 1:64){
    col1 <- paste(c("L", pixels[i,3]), collapse = "")
    col2 <- paste(c("a", pixels[i,3]), collapse = "")
    col3 <- paste(c("b", pixels[i,3]), collapse = "")
    Lab <- c(lab.measure[pixels[i,1],col1], lab.measure[pixels[i,1],col2], lab.measure[pixels[i,1],col3])
    lab.hex <- hex(LAB(Lab[1],Lab[2],Lab[3]), fixup = T)
    pixel.hex[i,1] <- pixels[i,3]
    pixel.hex[i,2] <- lab.hex
  }
  image(matrix(1:64,nrow=8),col=pixel.hex[,2])
}

# function to display the master color card. No inputs expected
mastercardDisplay <- function(){
  master.hex <- matrix(NA,nrow=64,ncol = 1,byrow=T)
  for(i in 1:64){
    master.hex[i,1] <- hex(LAB(master.color[i, "L"],master.color[i, "a"],master.color[i, "b"]), fixup=T)
  }
  image(matrix(1:64,nrow=8),col=master.hex[,1])
}
par(mfrow=c(1,2))
colorcardDisplay(allMax)
mastercardDisplay()

# show all pixels with max difference of > 5
allMax[which(allMax[,3]>5),]
