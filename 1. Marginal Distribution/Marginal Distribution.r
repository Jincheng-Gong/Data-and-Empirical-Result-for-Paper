setwd("C:\\Users\\Mordor Gong\\Desktop\\Models\\1. Marginal Distribution")
library(rugarch)
library(fGarch)
library(quantmod)
library(fBasics)
library(fUnitRoots)
library(TSA)
library(FinTS)
library(copula)

da =read.csv('0.csv',header = T, row.names = 1)
da
# Descriptive Statistics
basicStats(da[,1])
basicStats(da[,2])
basicStats(da[,3])
basicStats(da[,4])
# ADF Test
adfTest(da[,1], lags = 10, type=c('c'))
adfTest(da[,2], lags = 10, type=c('c'))
adfTest(da[,3], lags = 10, type=c('c'))
adfTest(da[,4], lags = 10, type=c('c'))
# Jarque - Bera Test
normalTest(da[,1], method='jb')
normalTest(da[,2], method='jb')
normalTest(da[,3], method='jb')
normalTest(da[,4], method='jb')
# Ljung - Box Test
Box.test(da[,1], lag=20, type='Ljung')
Box.test(da[,2], lag=20, type='Ljung')
Box.test(da[,3], lag=20, type='Ljung')
Box.test(da[,4], lag=20, type='Ljung')
# Lagrange - Multiplier Test
ArchTest(da[,1], 20)
ArchTest(da[,2], 20)
ArchTest(da[,3], 20)
ArchTest(da[,4], 20)

da =read.csv('1.csv',header = T, row.names = 1)
da
# Descriptive Statistics
basicStats(da[,1])
basicStats(da[,2])
basicStats(da[,3])
basicStats(da[,4])
# ADF Test
adfTest(da[,1], lags = 10, type=c('c'))
adfTest(da[,2], lags = 10, type=c('c'))
adfTest(da[,3], lags = 10, type=c('c'))
adfTest(da[,4], lags = 10, type=c('c'))
# Jarque - Bera Test
normalTest(da[,1], method='jb')
normalTest(da[,2], method='jb')
normalTest(da[,3], method='jb')
normalTest(da[,4], method='jb')
# Ljung - Box Test
Box.test(da[,1], lag=20, type='Ljung')
Box.test(da[,2], lag=20, type='Ljung')
Box.test(da[,3], lag=20, type='Ljung')
Box.test(da[,4], lag=20, type='Ljung')
# Lagrange - Multiplier Test
ArchTest(da[,1], 20)
ArchTest(da[,2], 20)
ArchTest(da[,3], 20)
ArchTest(da[,4], 20)

da =read.csv('2.csv',header = T, row.names = 1)
da
# Descriptive Statistics
basicStats(da[,1])
basicStats(da[,2])
basicStats(da[,3])
basicStats(da[,4])
# ADF Test
adfTest(da[,1], lags = 10, type=c('c'))
adfTest(da[,2], lags = 10, type=c('c'))
adfTest(da[,3], lags = 10, type=c('c'))
adfTest(da[,4], lags = 10, type=c('c'))
# Jarque - Bera Test
normalTest(da[,1], method='jb')
normalTest(da[,2], method='jb')
normalTest(da[,3], method='jb')
normalTest(da[,4], method='jb')
# Ljung - Box Test
Box.test(da[,1], lag=20, type='Ljung')
Box.test(da[,2], lag=20, type='Ljung')
Box.test(da[,3], lag=20, type='Ljung') # Has some problem
Box.test(da[,4], lag=20, type='Ljung') # Has some problem
# Lagrange - Multiplier Test
ArchTest(da[,1], 20)
ArchTest(da[,2], 20)
ArchTest(da[,3], 20) # Has some problem
ArchTest(da[,4], 20) # Has some problem

da =read.csv('3.csv',header = T, row.names = 1)
da
# Descriptive Statistics
basicStats(da[,1])
basicStats(da[,2])
basicStats(da[,3])
basicStats(da[,4])
# ADF Test
adfTest(da[,1], lags = 10, type=c('c'))
adfTest(da[,2], lags = 10, type=c('c'))
adfTest(da[,3], lags = 10, type=c('c'))
adfTest(da[,4], lags = 10, type=c('c'))
# Jarque - Bera Test
normalTest(da[,1], method='jb')
normalTest(da[,2], method='jb')
normalTest(da[,3], method='jb')
normalTest(da[,4], method='jb')
# Ljung - Box Test
Box.test(da[,1], lag=30, type='Ljung')
Box.test(da[,2], lag=20, type='Ljung')
Box.test(da[,3], lag=20, type='Ljung')
Box.test(da[,4], lag=20, type='Ljung')
# Lagrange - Multiplier Test
ArchTest(da[,1], 20)
ArchTest(da[,2], 20)
ArchTest(da[,3], 20)
ArchTest(da[,4], 20)

# ARMA-GARCH Model
da = read.csv('0.csv',header = T, row.names = 1)
da

#1
spec1=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1),external.regressors = NULL),
                mean.model = list(armaOrder=c(1,1),include.mean = T),
                distribution.model = "std")
m1=ugarchfit(data=da[,1],spec = spec1)
m1

#2
spec2=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1),external.regressors = NULL),
                 mean.model = list(armaOrder=c(1,1),include.mean = T),
                 distribution.model = "ged")
m2=ugarchfit(data=da[,2],spec = spec2)
m2

#3
spec3=ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(2,1),external.regressors = NULL),
                 mean.model = list(armaOrder=c(1,0),include.mean = T),
                 distribution.model = "sged")
m3=ugarchfit(data=da[,3],spec = spec3)
m3

#4
spec4=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1),external.regressors = NULL),
                 mean.model = list(armaOrder=c(1,1),include.mean = T),
                 distribution.model = "std")
m4=ugarchfit(data=da[,4],spec = spec4)
m4

# stdresidual
stdresidual1 = m1@fit$residuals / m1@fit$sigma
stdresidual2 = m2@fit$residuals / m2@fit$sigma
stdresidual3 = m3@fit$residuals / m3@fit$sigma
stdresidual4 = m4@fit$residuals / m4@fit$sigma

# pobs
#U1 = pobs(stdresidual1)
#U2 = pobs(stdresidual2)
#U3 = pobs(stdresidual3)
#U4 = pobs(stdresidual4)

# PIT process divide into 3 parts and 1 total
#0
U1 = pstd(stdresidual1,nu = 4.407865)
ks.test(U1,punif)
U2 = pged(stdresidual2,nu = 1.321046)
ks.test(U2,punif)
U3 = psged(stdresidual3,nu = 1.266846,xi = 0.968126)
ks.test(U3,punif)
U4 = pstd(stdresidual4,nu = 4.386817)
ks.test(U4,punif,alternative='greater')
da = read.csv('0.csv',header = T)
U = cbind(da[,1],U1,U2,U3,U4)
write.csv(U,"0PIT.csv",row.names = F)

#1
stdresidual11 = stdresidual1[0:2058]
U1 = pstd(stdresidual11,nu = 4.407865)
ks.test(U1,punif)

stdresidual22 = stdresidual2[0:2058]
U2 = pged(stdresidual22,nu = 1.321046)
ks.test(U2,punif)

stdresidual33 = stdresidual3[0:2058]
U3 = psged(stdresidual33,nu = 1.266846,xi = 0.968126)
ks.test(U3,punif)

stdresidual44 = stdresidual4[0:2058]
U4 = pstd(stdresidual44,nu = 4.386817)
ks.test(U4,punif)

da = read.csv('1.csv',header = T)
U = cbind(da[,1],U1,U2,U3,U4)
write.csv(U,"1PIT.csv",row.names = F)

#2
stdresidual11 = stdresidual1[2059:(2058+592)]
U1 = pstd(stdresidual11,nu = 4.407865)
ks.test(U1,punif,alternative='greater')

stdresidual22 = stdresidual2[2059:(2058+592)]
U2 = pged(stdresidual22,nu = 1.321046)
ks.test(U2,punif,alternative='greater')

stdresidual33 = stdresidual3[2059:(2058+592)]
U3 = psged(stdresidual33,nu = 1.266846,xi = 0.968126)
ks.test(U3,punif)

stdresidual44 = stdresidual4[2059:(2058+592)]
U4 = pstd(stdresidual44,nu = 4.386817)
ks.test(U4,punif,alternative='less')

da = read.csv('2.csv',header = T)
U = cbind(da[,1],U1,U2,U3,U4)
write.csv(U,"2PIT.csv",row.names = F)

#3
stdresidual11 = stdresidual1[(2058+593):3712]
U1 = pstd(stdresidual11,nu = 4.407865)
ks.test(U1,punif)

stdresidual22 = stdresidual2[(2058+593):3712]
U2 = pged(stdresidual22,nu = 1.321046)
ks.test(U2,punif)

stdresidual33 = stdresidual3[(2058+593):3712]
U3 = psged(stdresidual33,nu = 1.266846,xi = 0.968126)
ks.test(U3,punif,alternative='greater')

stdresidual44 = stdresidual4[(2058+593):3712]
U4 = pstd(stdresidual44,nu = 4.386817)
ks.test(U4,punif)

da = read.csv('3.csv',header = T)
U = cbind(da[,1],U1,U2,U3,U4)
write.csv(U,"3PIT.csv",row.names = F)


# Got the para of CoVaR
#0
sigma1 = m1@fit$sigma
sigma2 = m2@fit$sigma
sigma3 = m3@fit$sigma
sigma4 = m4@fit$sigma

da = read.csv('0.csv',header = T)
U = cbind(da[,1],sigma1,sigma2,sigma3,sigma4)
write.csv(U,"0CoVaR Para.csv",row.names = F)

#1
sigma1 = m1@fit$sigma[0:2058]
sigma2 = m2@fit$sigma[0:2058]
sigma3 = m3@fit$sigma[0:2058]
sigma4 = m4@fit$sigma[0:2058]

da = read.csv('1.csv',header = T)
U = cbind(da[,1],sigma1,sigma2,sigma3,sigma4)
write.csv(U,"1CoVaR Para.csv",row.names = F)

#2
sigma1 = m1@fit$sigma[2059:(2058+592)]
sigma2 = m2@fit$sigma[2059:(2058+592)]
sigma3 = m3@fit$sigma[2059:(2058+592)]
sigma4 = m4@fit$sigma[2059:(2058+592)]

da = read.csv('2.csv',header = T)
U = cbind(da[,1],sigma1,sigma2,sigma3,sigma4)
write.csv(U,"2CoVaR Para.csv",row.names = F)

#3
sigma1 = m1@fit$sigma[(2058+593):3712]
sigma2 = m2@fit$sigma[(2058+593):3712]
sigma3 = m3@fit$sigma[(2058+593):3712]
sigma4 = m4@fit$sigma[(2058+593):3712]

da = read.csv('3.csv',header = T)
U = cbind(da[,1],sigma1,sigma2,sigma3,sigma4)
write.csv(U,"3CoVaR Para.csv",row.names = F)
