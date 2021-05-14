# Set working dictionary
setwd("C:/Users/Mordor Gong/Desktop/Models/2. Copula Model")
library("VineCopula")
library('shiny')

# Input data for period 1,2,3
da = read.csv('3PIT.csv',header = T, row.names = 1)
# da = read.csv('2PIT.csv',header = T, row.names = 1)
# da = read.csv('3PIT.csv',header = T, row.names = 1)
da = as.matrix(da)


BiCopEstList(da[,1], da[,2])
BiCopEstList(da[,1], da[,3])
BiCopEstList(da[,1], da[,4])
BiCopEstList(da[,2], da[,3])
BiCopEstList(da[,2], da[,4])
BiCopEstList(da[,3], da[,4])


BiCopCompare(da[,1], da[,2])
BiCopCompare(da[,1], da[,3])
BiCopCompare(da[,1], da[,4])
BiCopCompare(da[,2], da[,3])
BiCopCompare(da[,2], da[,4])
BiCopCompare(da[,3], da[,4])


BiCopKDE(da[,1], da[,2])
BiCopKDE(da[,1], da[,3])
BiCopKDE(da[,1], da[,4])
BiCopKDE(da[,2], da[,3])
BiCopKDE(da[,2], da[,4])
BiCopKDE(da[,3], da[,4])


BiCopKPlot(da[,1], da[,2])
BiCopKPlot(da[,1], da[,3])
BiCopKPlot(da[,1], da[,4])
BiCopKPlot(da[,2], da[,3])
BiCopKPlot(da[,2], da[,4])
BiCopKPlot(da[,3], da[,4])


cor(da,method = "spearman")
cor(da,method = "kendall")
cor(da,method = "pearson")