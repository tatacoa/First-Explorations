## Data Visualization
## First Project - Douglas Color Cards

setwd('C:/Users/Bina/Desktop/DataScience/2.Semester/DataVis/FirstProject_Douglas')

lab.measure <- read.csv2("LabMeasurements-Color-Card.csv")
master.color <- read.csv2("MasterColorCard.csv") # CMYKS colors are in p1- p5 (neglect?)

## Play with data
# install.packages("plot3D")
# install.packages("rgl")
library("plot3D")
library("rgl")

plot(master.color)

plot3d(master.color$Field, master.color$Crow, master.color$Ccol, 
       col=4)

plot(lab.measure$Row, lab.measure$Column)
# install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
library(colorspace) # load

# save L, a and b values from dataframe to variables
Lstar = master.color$L
Astar = master.color$a
Bstar = master.color$b
# apply unknown magic
solarized = structure(list(Lstar, Astar, Bstar), .Names = c("Lstar", "Astar", "Bstar"), row.names = c(NA,16), class = "data.frame")
head(solarized) # show top
solarizedLAB = with(solarized, LAB(Lstar, Astar, Bstar))

scatterplot3d(master.color[,9:11], 
              main="3D Scatter Plot",
              color = hex(solarizedLAB, fixup = TRUE), 
              pch = 16, 
              type = "h",
              grid=TRUE,
              col.grid = "lightpink",
              box=FALSE)
