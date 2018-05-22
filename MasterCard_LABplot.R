## Data Visualization
## First Project - Color Cards

lab.measure <- read.csv2("FILENAME.csv")
master.color <- read.csv2("FILENAME2") # CMYKS colors are in p1- p5 (neglect?)

# install.packages("plot3D")
# install.packages("rgl")
# install.packages("scatterplot3d")
library("scatterplot3d")
library(colorspace)
library("plot3D")
library("rgl")

plot(master.color[1:3])
plot3d(master.color, col=4)

# save L, a and b values from dataframe to vectors
Lstar = master.color$L
Astar = master.color$a
Bstar = master.color$b

# create new dataframe from L-a-b vectors
solarized = structure(list(Lstar, Astar, Bstar), .Names = c("Lstar", "Astar", "Bstar"), row.names = c(NA,16), class = "data.frame")
head(solarized) # show top
solarizedLAB = with(solarized, LAB(Lstar, Astar, Bstar))

# show us all 64 color spots in a L-a-b plot coloured in their LAB color
s3d <- scatterplot3d(master.color[,9:11], 
              main="Master Color Card - Lab values",
              color = hex(solarizedLAB, fixup = TRUE),
              pch = 16, 
              type = "h",
              grid=TRUE,
              #col.grid = "lightblue",
              box=FALSE)
# add labels according to color spot field
text(s3d$xyz.convert(master.color[,9:11]), 
     labels = rownames(master.color),
     cex= 0.7, 
     col = hex(solarizedLAB, fixup = TRUE),
     pos = 3) 
