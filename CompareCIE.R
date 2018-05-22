## Data Visualization
## First Project - Color Cards
## Comparing results of CIE94 and CIE76

# load data and clean up workspace
load("deltae76.Rda")
load("deltae94.Rda")
CIE76 <- deltae; rm(deltae)
CIE94 <- lab.distances; rm(lab.distances)

# compare dimensions
dim(CIE76); dim(CIE94)

# subtract the deltaes of CIE94 off the deltaes of CIE76 to analyse the differences between both equations (dd, the double d stands for double deltae!)
dd <- CIE76[3:66] - CIE94[3:66]
# calculate means of the double deltae and the total mean of everything
dd.means <- apply(dd, 2, mean)
dd.mean <- mean(dd.means)
dd.max <- max(dd)
dd.mean; dd.max

# do the same to the original two dataframes
# CIE76
CIE76.means <- apply(CIE76[3:66], 2, mean)
CIE76.mean <- mean(CIE76.means)
CIE76.max <- max(CIE76[3:66])
CIE76.mean; CIE76.max
# CIE94
CIE94.means <- apply(CIE94[3:66], 2, mean)
CIE94.mean <- mean(CIE94.means)
CIE94.max <- max(CIE94[3:66])
CIE94.mean; CIE94.max

# plot histograms of CIE76, CIE94 data and the double d in same scale for comparison
par(mfrow=c(3,1))
hist(CIE76.means, breaks = 20, xlim = c(0,40))
hist(CIE94.means, breaks = 20, xlim = c(0,40))
hist(dd.means, breaks = 20, xlim = c(0,40))

# in the means and the max of the CIE dataframes we can see that both distance formulas provided quite similar results.
# This is shown by the similar appearing histograms and the low max of dd.means at only ~6.5 with CIE max values of ~36. 
# there seems to be an outlier at distance ~35 where the pixel did not match the master criteria.

# plotting means of cIE94 against CIE76
par(mfrow=c(1,1))
plot(CIE76.means ~ CIE94.means, type="n")
# add line for comparing level of identity
abline(a=0, b=1)
text(CIE94.means, CIE76.means, labels=colnames(CIE94[3:66]))
# the plot shows that CIE76 always generated a greater difference than CIE94. 
