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

col1 <- paste(c("L", pixel.number), collapse = "")
col2 <- paste(c("a", pixel.number), collapse = "")
col3 <- paste(c("b", pixel.number), collapse = "")
Lab <- c(lab.measure[max.color[1],col1], lab.measure[max.color[1],col2], lab.measure[max.color[1],col3])
lab.hex <- hex(LAB(Lab[1],Lab[2],Lab[3]), fixup = T)
master.hex <- hex(LAB(master.color[which(master.color$Crow == 3 & master.color$Ccol == 2), "L"],master.color[which(master.color$Crow == 3 & master.color$Ccol == 2), "a"],master.color[which(master.color$Crow == 3 & master.color$Ccol == 2), "b"]), fixup=T)
master.color[which(master.color$Crow == 3 & master.color$Ccol == 2), "L"]
image(matrix(1:2,nrow=2),col=c(lab.hex, master.hex))
# to be finished
