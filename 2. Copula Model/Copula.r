# Set working dictionary
setwd("C:/Users/Mordor Gong/Desktop/Models/2. Copula Model")
library(copula)

# Input data for period 1,2,3
da = read.csv('3PIT.csv',header = T, row.names = 1)
# da = read.csv('2PIT.csv',header = T, row.names = 1)
# da = read.csv('3PIT.csv',header = T, row.names = 1)
da = as.matrix(da)

# Draw correlation and matrix, more in Python
# splom2(da[,1:6],cex = 0.4, col.mat = adjustcolor('black', 0.5))
# cor(da, method = "spearman")

# Copula Model Estimate for period 1
m1 = fitCopula(tCopula(), data = cbind(da[,1], da[,2]), method = 'ml')
summary(m1)
gofCopula(tCopula(df.fixed = TRUE), x = cbind(da[,1], da[,2]), simulation = 'mult')

m1 = fitCopula(normalCopula(), data = cbind(da[,1], da[,2]), method = 'ml')
summary(m1)
gofCopula(normalCopula(), x = cbind(da[,1], da[,2]), simulation = 'mult')

m1 = fitCopula(gumbelCopula(), data = cbind(da[,1], da[,2]), method = 'ml')
summary(m1)
gofCopula(gumbelCopula(), x = cbind(da[,1], da[,2]), simulation = 'mult')

m1 = fitCopula(claytonCopula(), data = cbind(da[,1], da[,2]), method = 'ml')
summary(m1)
gofCopula(claytonCopula(), x = cbind(da[,1], da[,2]), simulation = 'mult')

m1 = fitCopula(frankCopula(), data = cbind(da[,1], da[,2]), method = 'ml')
summary(m1)
gofCopula(frankCopula(), x = cbind(da[,1], da[,2]), simulation = 'mult')

m1 = fitCopula(plackettCopula(), data = cbind(da[,1], da[,2]), method = 'ml')
summary(m1)
gofCopula(plackettCopula(), x = cbind(da[,1], da[,2]), simulation = 'mult')

m1 = fitCopula(rotCopula(claytonCopula()), data = cbind(da[,1], da[,2]), method = 'ml')
summary(m1)
gofCopula(rotCopula(claytonCopula()), x = cbind(da[,1], da[,2]), simulation = 'mult')

m1 = fitCopula(rotCopula(gumbelCopula()), data = cbind(da[,1], da[,2]), method = 'ml')
summary(m1)
gofCopula(rotCopula(gumbelCopula()), x = cbind(da[,1], da[,2]), simulation = 'mult')




# Copula Model Estimate for period 1
m2 = fitCopula(tCopula(), data = cbind(da[,1], da[,3]), method = 'ml')
summary(m2)
gofCopula(tCopula(df.fixed = TRUE), x = cbind(da[,1], da[,3]), simulation = 'mult')

m2 = fitCopula(normalCopula(), data = cbind(da[,1], da[,3]), method = 'ml')
summary(m2)
gofCopula(normalCopula(), x = cbind(da[,1], da[,3]), simulation = 'mult')

m2 = fitCopula(gumbelCopula(), data = cbind(da[,1], da[,3]), method = 'ml')
summary(m2)
gofCopula(gumbelCopula(), x = cbind(da[,1], da[,3]), simulation = 'mult')

m2 = fitCopula(claytonCopula(), data = cbind(da[,1], da[,3]), method = 'itau')
summary(m2)
gofCopula(claytonCopula(), x = cbind(da[,1], da[,3]), method ="Sn",estim.method ="itau", simulation = 'pb')

m2 = fitCopula(frankCopula(), data = cbind(da[,1], da[,3]), method = 'ml')
summary(m2)
gofCopula(frankCopula(), x = cbind(da[,1], da[,3]), simulation = 'mult')

m2 = fitCopula(plackettCopula(), data = cbind(da[,1], da[,3]), method = 'ml')
summary(m2)
gofCopula(plackettCopula(), x = cbind(da[,1], da[,3]), simulation = 'mult')

m2 = fitCopula(rotCopula(claytonCopula()), data = cbind(da[,1], da[,3]), method = 'itau')
summary(m2)
gofCopula(rotCopula(claytonCopula()), x = cbind(da[,1], da[,3]), method ="Rn",estim.method ="itau", simulation = 'mult')

m2 = fitCopula(rotCopula(gumbelCopula()), data = cbind(da[,1], da[,3]), method = 'itau')
summary(m2)
gofCopula(rotCopula(gumbelCopula()), x = cbind(da[,1], da[,3]), simulation = 'mult')




# Copula Model Estimate for period 1
m3 = fitCopula(tCopula(), data = cbind(da[,1], da[,4]), method = 'ml')
summary(m3)
gofCopula(tCopula(df.fixed = TRUE), x = cbind(da[,1], da[,4]), simulation = 'mult')

m3 = fitCopula(normalCopula(), data = cbind(da[,1], da[,4]), method = 'ml')
summary(m3)
gofCopula(normalCopula(), x = cbind(da[,1], da[,4]), simulation = 'mult')

m3 = fitCopula(gumbelCopula(), data = cbind(da[,1], da[,4]), method = 'mpl')
summary(m3)
gofCopula(gumbelCopula(), x = cbind(da[,1], da[,4]), simulation = 'mult')

m3 = fitCopula(claytonCopula(), data = cbind(da[,1], da[,4]), method = 'itau')
summary(m3)
gofCopula(claytonCopula(), x = cbind(da[,1], da[,4]), method ="Sn", estim.method ="itau", simulation = 'pb')

m3 = fitCopula(frankCopula(), data = cbind(da[,1], da[,4]), method = 'ml')
summary(m3)
gofCopula(frankCopula(), x = cbind(da[,1], da[,4]), simulation = 'mult')

m3 = fitCopula(plackettCopula(), data = cbind(da[,1], da[,4]), method = 'ml')
summary(m3)
gofCopula(plackettCopula(), x = cbind(da[,1], da[,4]), simulation = 'mult')

m3 = fitCopula(rotCopula(claytonCopula()), data = cbind(da[,1], da[,4]), method = 'itau')
summary(m3)
gofCopula(rotCopula(claytonCopula()), x = cbind(da[,1], da[,4]), method ="Rn",estim.method ="itau", simulation = 'mult')

m3 = fitCopula(rotCopula(gumbelCopula()), data = cbind(da[,1], da[,4]), method = 'mpl')
summary(m3)
gofCopula(rotCopula(gumbelCopula()), x = cbind(da[,1], da[,4]), method ="Rn", simulation = 'mult')




# Copula Model Estimate for period 1
m4 = fitCopula(tCopula(), data = cbind(da[,2], da[,3]), method = 'ml')
summary(m4)
gofCopula(tCopula(df.fixed = TRUE), x = cbind(da[,2], da[,3]), simulation = 'mult')

m4 = fitCopula(normalCopula(), data = cbind(da[,2], da[,3]), method = 'ml')
summary(m4)
gofCopula(normalCopula(), x = cbind(da[,2], da[,3]), simulation = 'mult')

m4 = fitCopula(gumbelCopula(), data = cbind(da[,2], da[,3]), method = 'ml')
summary(m4)
gofCopula(gumbelCopula(), x = cbind(da[,2], da[,3]), simulation = 'mult')

m4 = fitCopula(claytonCopula(), data = cbind(da[,2], da[,3]), method = 'itau')
summary(m4)
gofCopula(claytonCopula(), x = cbind(da[,2], da[,3]),estim.method ="itau", simulation = 'mult')

m4 = fitCopula(frankCopula(), data = cbind(da[,2], da[,3]), method = 'ml')
summary(m4)
gofCopula(frankCopula(), x = cbind(da[,2], da[,3]), simulation = 'mult')

m4 = fitCopula(plackettCopula(), data = cbind(da[,2], da[,3]), method = 'ml')
summary(m4)
gofCopula(plackettCopula(), x = cbind(da[,2], da[,3]), simulation = 'mult')

m4 = fitCopula(rotCopula(claytonCopula()), data = cbind(da[,2], da[,3]), method = 'itau')
summary(m4)
gofCopula(rotCopula(claytonCopula()), x = cbind(da[,2], da[,3]),estim.method ="itau", simulation = 'mult')

m4 = fitCopula(rotCopula(gumbelCopula()), data = cbind(da[,2], da[,3]), method = 'itau')
summary(m4)
gofCopula(rotCopula(gumbelCopula()), x = cbind(da[,2], da[,3]), simulation = 'mult')




# Copula Model Estimate for period 1
m5 = fitCopula(tCopula(), data = cbind(da[,2], da[,4]), method = 'ml')
summary(m5)
gofCopula(tCopula(df.fixed = TRUE), x = cbind(da[,2], da[,4]), simulation = 'mult')

m5 = fitCopula(normalCopula(), data = cbind(da[,2], da[,4]), method = 'ml')
summary(m5)
gofCopula(normalCopula(), x = cbind(da[,2], da[,4]), simulation = 'mult')

m5 = fitCopula(gumbelCopula(), data = cbind(da[,2], da[,4]), method = 'itau')
summary(m5)
gofCopula(gumbelCopula(), x = cbind(da[,2], da[,4]), simulation = 'mult')

m5 = fitCopula(claytonCopula(), data = cbind(da[,2], da[,4]), method = 'itau')
summary(m5)
gofCopula(claytonCopula(), x = cbind(da[,2], da[,4]),estim.method ="itau", simulation = 'mult')

m5 = fitCopula(frankCopula(), data = cbind(da[,2], da[,4]), method = 'ml')
summary(m5)
gofCopula(frankCopula(), x = cbind(da[,2], da[,4]), simulation = 'mult')

m5 = fitCopula(plackettCopula(), data = cbind(da[,2], da[,4]), method = 'ml')
summary(m5)
gofCopula(plackettCopula(), x = cbind(da[,2], da[,4]), simulation = 'mult')

m5 = fitCopula(rotCopula(claytonCopula()), data = cbind(da[,2], da[,4]), method = 'itau')
summary(m5)
gofCopula(rotCopula(claytonCopula()), x = cbind(da[,2], da[,4]), simulation = 'mult')

m5 = fitCopula(rotCopula(gumbelCopula()), data = cbind(da[,2], da[,4]), method = 'mpl')
summary(m5)
gofCopula(rotCopula(gumbelCopula()), x = cbind(da[,2], da[,4]), simulation = 'mult')


# Copula Model Estimate for period 1
m5 = fitCopula(tCopula(), data = cbind(da[,3], da[,4]), method = 'ml')
summary(m5)
gofCopula(tCopula(df.fixed = TRUE), x = cbind(da[,3], da[,4]), simulation = 'mult')

m5 = fitCopula(normalCopula(), data = cbind(da[,3], da[,4]), method = 'ml')
summary(m5)
gofCopula(normalCopula(), x = cbind(da[,3], da[,4]), simulation = 'mult')

m5 = fitCopula(gumbelCopula(), data = cbind(da[,3], da[,4]), method = 'itau')
summary(m5)
gofCopula(gumbelCopula(), x = cbind(da[,3], da[,4]), simulation = 'mult')

m5 = fitCopula(claytonCopula(), data = cbind(da[,3], da[,4]), method = 'itau')
summary(m5)
gofCopula(claytonCopula(), x = cbind(da[,3], da[,4]),estim.method ="itau", simulation = 'mult')

m5 = fitCopula(frankCopula(), data = cbind(da[,3], da[,4]), method = 'ml')
summary(m5)
gofCopula(frankCopula(), x = cbind(da[,3], da[,4]), simulation = 'mult')

m5 = fitCopula(plackettCopula(), data = cbind(da[,3], da[,4]), method = 'ml')
summary(m5)
gofCopula(plackettCopula(), x = cbind(da[,3], da[,4]), simulation = 'mult')

m5 = fitCopula(rotCopula(claytonCopula()), data = cbind(da[,3], da[,4]), method = 'itau')
summary(m5)
gofCopula(rotCopula(claytonCopula()), x = cbind(da[,3], da[,4]), simulation = 'mult')

m5 = fitCopula(rotCopula(gumbelCopula()), data = cbind(da[,3], da[,4]), method = 'mpl')
summary(m5)
gofCopula(rotCopula(gumbelCopula()), x = cbind(da[,3], da[,4]), simulation = 'mult')