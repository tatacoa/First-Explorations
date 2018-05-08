## Data Visualization
## First Project - Color Cards
## Analyzing Lab color differences

# function to compute the distances based on Lab1 and Lab2
labDistance <- function(L1,a1,b1,L2,a2,b2) {
  # collection of all values from CIE94 (https://en.wikipedia.org/wiki/Color_difference)
  dL <- L1 - L2
  C1 <- sqrt((a1)^2 + (b1)^2)
  C2 <- sqrt((a2)^2 + (b2)^2)
  dC <- C1-C2
  da <- a1 - a2
  db <- b1 - b2
  dH <- sqrt(da^2 + db^2 - dC^2)
  Sl <- 1
  kL <- 1
  K1 <- 0.045
  K2 <- 0.015
  Sc <- 1 + K1*C1
  Sh <- 1 + K2*C1
  
  # final distance value
  dE <- sqrt((dL/(kL * Sl))^2 + (dC/Sc)^2 + (dH/Sh)^2)
  return(dE)
}

lab.measure <- read.csv2("LabMeasurements-Color-Card.csv")
master.color <- read.csv2("MasterColorCard.csv") # CMYKS colors are in p1- p5 (neglect?)

# instantiate the iterators and the dataframe to store the values in
lab.distances <- as.data.frame(matrix(c(1:3),nrow = 1))
k <- 1
l <- 1
i <- 1
# choose the amount of sheets to scan through (max=13), max will take ~5 sec
sheets <- 13

# iterate through cards
for (m in c(1:(sheets*42))){
  # iterate through pixel
  for (j in c(3:dim(lab.measure)[2])){
    # if iterator is not dividable by 3, jump to the next j i.e. jumping over the Lab sequences
    if (j%%3 != 0){
      
    }else{
      
      if (i > 64){
        i <- 1
      }
      
      # in the storage dataframe, jump to the next row for the next card if k > 64
      if (k < 65){
        lab.distances[l,k + 2] <- labDistance(master.color[i,9], master.color[i,10],master.color[i,11],lab.measure[m,j],lab.measure[m,j+1], lab.measure[m,j+2])
        k <- k + 1
      }else{
        # adding row label from lab.measure file
        lab.distances[l, 1] <- lab.measure[l, 1]
        lab.distances[l, 2] <- lab.measure[l, 2]
        l <- l +1
        lab.distances[l, 3] <- labDistance(master.color[i,9], master.color[i,10],master.color[i,11],lab.measure[m,j],lab.measure[m,j+1], lab.measure[m,j+2])
        k <- 2
      }
      #print(i) #to check that everything is running smooth
      i <- i + 1
    }
  }
  # adding the labels for the two very last rows
  lab.distances[l, 1] <- lab.measure[l, 1]
  lab.distances[l, 2] <- lab.measure[l, 2]
}
