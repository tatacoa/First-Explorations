####################################################################################
# loop for all the distances

# Load data
lab <- read.csv2("LabMeasurements-Color-Card.csv")
master <- read.csv2("MasterColorCard.csv")

# create an empty matrix to store the deltae values, the matrix is 546x66 (66 columns so we know from where they come
# 546 row -> total of color cards (13x42)
# the first two columns are the row  and column in a sheet (6x7)
# the deltae for each card is stored rowwise row = card

# deltae is the color distance :)
deltae <- matrix(NA,nrow = 546, ncol=66, byrow = TRUE)

# put the values for the first two columns (indicates de card position in a sheet)
deltae[,1] <- lab[,1]; deltae[,1]
deltae[,2] <- lab[,2]; deltae[,c(1,2)]
dim(deltae); dim(lab) #check sizes 

# colnames(deltae)
colnames(deltae) <- c("Row", "Column", c(11:18, 21:28, 31:38, 41:48, 51:58, 61:68, 71:78, 81:88))
colnames(deltae)


# for loop to compute the color distances
distance_single <- {}
distance_row <- {}
for (i in 1:length(row.names(lab))) {
  cardmatrix <- matrix(lab[i,-c(1:2)],nrow=64,ncol=3,byrow=TRUE)
  colnames(cardmatrix) = c("L","a","b")
  for (j in 1:64) {
    cardmatrix[j,] -> matrixA
    master[j,] -> matrixB
    distance_single <- sqrt((matrixA$L - matrixB$L)^2 + (matrixA$a - matrixB$a)^2 + (matrixA$b - matrixB$b)^2)
    distance_row[j] <- distance_single
  }
  deltae[i,-c(1:2)] <- distance_row
}

head(deltae) 
deltae <- as.data.frame(deltae)

save(deltae, file="deltae76.Rda")

# Just noticeable difference starts at deltae 2.3
# Add column to deltae data frame which counts the cells rowwise which are above the threshold
deltae$JDNcount <- rowSums(deltae >= 2.3)
# Which counts are the most common among all color cards
boxplot(deltae$JDNcount)

# all 64 color spots deltaes as boxplot next to each other
boxplot(deltae[3:66])

# which color spots have the highest mean - hence are most commonly different from the master data
deltae.means <- colMeans(x = deltae); deltae.means
sort(deltae.means, decreasing = TRUE)
# => 32         85         34         87         38         24         43         12         77
#    34.4323202 10.2118046  9.6919683  8.5726534  7.1780514  6.8787069  5.9783299  5.9769575  5.7851123 
#    18(hgrau)   61(lila)   20(braun)  63(blau)   24(drot)   12(braun)  27(braun)   2(dgruen) 55(grau)

###### continuation @tata #########
means_colors <- {}
for (i in c(3:66)) {
  means <- mean(CIE76[,i])
  means_colors[i] <- means
}
means_colors[-c(1:2)] -> means_colors
apply(CIE76[,3:66],2,mean)

means_colors_sorted <- {}
for (i in c(1:64)) {
  colorMean <- means_colors[i] 
  if (colorMean < 2)
    means_colors_sorted[i] = 1
  else if (colorMean > 2 && colorMean < 4)
    means_colors_sorted[i] = 2
  else if (colorMean > 4)
    means_colors_sorted[i] = 3
}
means_colors_sorted

# make a matrix 
library("colorscience")
colrange <- colorRampPalette(c("yellowgreen","yellow","red"))(3)
mean_matrix <- matrix(means_colors_sorted, nrow = 8, ncol= 8, byrow = FALSE, dimnames = NULL)
mean_heatmap <- heatmap(mean_matrix, Rowv=NA, Colv=NA, col = colrange, 
                       scale="column", margins=c(5,10))



