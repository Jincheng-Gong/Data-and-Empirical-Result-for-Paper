setwd("C:\\Users\\Mordor Gong\\Desktop\\Models\\3. CoVaR")
da = read.csv('3CoVaR Para.csv',header = T, row.names = 1)
da

library("fGarch")
source("CoVaR.R")
source("DynCopulaCoVaR.R")
source("DynCopulaCoVaRUpper.R")
source("skewtdis_inv.R")
require("pracma")
require("copula")

######## Down CoVaR L to R
# 1-2
CoVaRD1 = CoVaR(0.05,0.05,par=0.91,par2=6.64,dof=1.321046,cond.mean=as.matrix(da[,3]),
                cond.sigma=as.matrix(da[,4]),dist="ged",type="Student")
CoVaR1DL = CoVaRD1$CoVaR
# 1-3
CoVaRD2 = CoVaR(0.05,0.05,par=0.63,par2=14.87,dof=1.266846,gamma=0.968126,cond.mean=as.matrix(da[,5]),
                cond.sigma=as.matrix(da[,6]),dist="sged",type="Student")
CoVaR2DL = CoVaRD2$CoVaR
# 1-4
CoVaRD3 = CoVaR(0.05,0.05,par=1.825,dof=4.386817,cond.mean=as.matrix(da[,7]),
                cond.sigma=as.matrix(da[,8]),dist="t",type="Plackett")
CoVaR3DL = CoVaRD3$CoVaR
# 2-3
CoVaRD4 = CoVaR(0.05,0.05,par=0.55,par2=11.27,dof=1.266846,gamma=0.968126,cond.mean=as.matrix(da[,5]),
                cond.sigma=as.matrix(da[,6]),dist="sged",type="Student")
CoVaR4DL = CoVaRD4$CoVaR
# 2-4
CoVaRD5 = CoVaR(0.05,0.05,par=1.19,par2=0.07,dof=4.386817,cond.mean=as.matrix(da[,7]),
                cond.sigma=as.matrix(da[,8]),dist="t",type="BB7")
CoVaR5DL = CoVaRD5$CoVaR
# 3-4
CoVaRD6 = CoVaR(0.05,0.05,par=0.06,par2=1.21,dof=4.386817,cond.mean=as.matrix(da[,7]),
                cond.sigma=as.matrix(da[,8]),dist="t",type="BB1")
CoVaR6DL = CoVaRD6$CoVaR

######## Down CoVaR R to L
# 1-2
CoVaRD1 = CoVaR(0.05,0.05,par=0.91,par2=6.64,,dof=4.407865,cond.mean=as.matrix(da[,1]),
                cond.sigma=as.matrix(da[,2]),dist="t",type="Student")
CoVaR1DR = CoVaRD1$CoVaR
# 1-3
CoVaRD2 = CoVaR(0.05,0.05,par=0.63,par2=14.87,dof=4.407865,cond.mean=as.matrix(da[,1]),
                cond.sigma=as.matrix(da[,2]),dist="t",type="Student")
CoVaR2DR = CoVaRD2$CoVaR
# 1-4
CoVaRD3 = CoVaR(0.05,0.05,par=1.825,dof=4.407865,cond.mean=as.matrix(da[,1]),
                cond.sigma=as.matrix(da[,2]),dist="t",type="Plackett")
CoVaR3DR = CoVaRD3$CoVaR
# 2-3
CoVaRD4 = CoVaR(0.05,0.05,par=0.55,par2=11.27,dof=1.321046,cond.mean=as.matrix(da[,3]),
                cond.sigma=as.matrix(da[,4]),dist="ged",type="Student")
CoVaR4DR = CoVaRD4$CoVaR
# 2-4
CoVaRD5 = CoVaR(0.05,0.05,par=1.19,par2=0.07,dof=1.321046,cond.mean=as.matrix(da[,3]),
                cond.sigma=as.matrix(da[,4]),dist="ged",type="BB7")
CoVaR5DR = CoVaRD5$CoVaR
# 3-4
CoVaRD6 = CoVaR(0.05,0.05,par=0.06,par2=1.21,dof=1.266846,gamma=0.968126,cond.mean=as.matrix(da[,5]),
                cond.sigma=as.matrix(da[,6]),dist="sged",type="BB1")
CoVaR6DR = CoVaRD6$CoVaR


######## Upper CoVaR L to R
# 1-2
CoVaRUP1 = CoVaR(0.95,0.95,par=0.91,par2=6.64,dof=1.321046,cond.mean=as.matrix(da[,3]),
                cond.sigma=as.matrix(da[,4]),dist="ged",type="StudentUp")
CoVaR1UL = CoVaRUP1$CoVaR
# 1-3
CoVaRUP2 = CoVaR(0.95,0.95,par=0.63,par2=14.87,dof=1.266846,gamma=0.968126,cond.mean=as.matrix(da[,5]),
                cond.sigma=as.matrix(da[,6]),dist="sged",type="StudentUp")
CoVaR2UL = CoVaRUP2$CoVaR
# 1-4
CoVaRUP3 = CoVaR(0.95,0.95,par=1.825,dof=4.386817,cond.mean=as.matrix(da[,7]),
                cond.sigma=as.matrix(da[,8]),dist="t",type="PlackettUp")
CoVaR3UL = CoVaRUP3$CoVaR
# 2-3
CoVaRUP4 = CoVaR(0.95,0.95,par=0.55,par2=11.27,dof=1.266846,gamma=0.968126,cond.mean=as.matrix(da[,5]),
                cond.sigma=as.matrix(da[,6]),dist="sged",type="StudentUp")
CoVaR4UL = CoVaRUP4$CoVaR
# 2-4
CoVaRUP5 = CoVaR(0.95,0.95,par=1.19,par2=0.07,dof=4.386817,cond.mean=as.matrix(da[,7]),
                cond.sigma=as.matrix(da[,8]),dist="t",type="BB7Up")
CoVaR5UL = CoVaRUP5$CoVaR
# 3-4
CoVaRUP6 = CoVaR(0.95,0.95,par=0.06,par2=1.21,dof=4.386817,cond.mean=as.matrix(da[,7]),
                cond.sigma=as.matrix(da[,8]),dist="t",type="BB1Up")
CoVaR6UL = CoVaRUP6$CoVaR

######## Upper CoVaR R to L
# 1-2
CoVaRUP1 = CoVaR(0.95,0.95,par=0.91,par2=6.64,dof=4.407865,cond.mean=as.matrix(da[,1]),
                cond.sigma=as.matrix(da[,2]),dist="t",type="StudentUp")
CoVaR1UR = CoVaRUP1$CoVaR
# 1-3
CoVaRUP2 = CoVaR(0.95,0.95,par=0.63,par2=14.87,dof=4.407865,cond.mean=as.matrix(da[,1]),
                cond.sigma=as.matrix(da[,2]),dist="t",type="StudentUp")
CoVaR2UR = CoVaRUP2$CoVaR
# 1-4
CoVaRUP3 = CoVaR(0.95,0.95,par=1.825,dof=4.407865,cond.mean=as.matrix(da[,1]),
                cond.sigma=as.matrix(da[,2]),dist="t",type="PlackettUp")
CoVaR3UR = CoVaRUP3$CoVaR
# 2-3
CoVaRUP4 = CoVaR(0.95,0.95,par=0.55,par2=11.27,dof=1.321046,cond.mean=as.matrix(da[,3]),
                cond.sigma=as.matrix(da[,4]),dist="ged",type="StudentUp")
CoVaR4UR = CoVaRUP4$CoVaR
# 2-4
CoVaRUP5 = CoVaR(0.95,0.95,par=1.19,par2=0.07,dof=1.321046,cond.mean=as.matrix(da[,3]),
                cond.sigma=as.matrix(da[,4]),dist="ged",type="BB7Up")
CoVaR5UR = CoVaRUP5$CoVaR
# 3-4
CoVaRUP6 = CoVaR(0.95,0.95,par=0.06,par2=1.21,dof=1.266846,gamma=0.968126,cond.mean=as.matrix(da[,5]),
                cond.sigma=as.matrix(da[,6]),dist="sged",type="BB1Up")
CoVaR6UR = CoVaRUP6$CoVaR


######### Calc. VaR
# 1
VaR1D = as.matrix(da[,1])+as.matrix(da[,2])*as.numeric(qt(0.05,4.407865))
VaR1U = as.matrix(da[,1])+as.matrix(da[,2])*as.numeric(qt(0.95,4.407865))
# 2
VaR2D = as.matrix(da[,3])+as.matrix(da[,4])*as.numeric(qged(as.numeric(0.05),nu=1.321046))
VaR2U = as.matrix(da[,3])+as.matrix(da[,4])*as.numeric(qged(as.numeric(0.95),nu=1.321046))
# 3
VaR3D = as.matrix(da[,5])+as.matrix(da[,6])*as.numeric(qsged(as.numeric(0.05),nu=as.numeric(1.266846),xi=as.numeric(0.968126)))
VaR3U = as.matrix(da[,5])+as.matrix(da[,6])*as.numeric(qsged(as.numeric(0.95),nu=as.numeric(1.266846),xi=as.numeric(0.968126)))
# 4
VaR4D = as.matrix(da[,7])+as.matrix(da[,8])*as.numeric(qt(0.05,4.386817))
VaR4U = as.matrix(da[,7])+as.matrix(da[,8])*as.numeric(qt(0.95,4.386817))

########### Data Output
da = read.csv('3CoVaR Para.csv',header = T)
U = cbind(da[,1],
          VaR1D,VaR2D,VaR3D,VaR4D,
          VaR1U,VaR2U,VaR3U,VaR4U,
          CoVaR1DL,CoVaR1DR,CoVaR1UL,CoVaR1UR,
          CoVaR2DL,CoVaR2DR,CoVaR2UL,CoVaR2UR,
          CoVaR3DL,CoVaR3DR,CoVaR3UL,CoVaR3UR,
          CoVaR4DL,CoVaR4DR,CoVaR4UL,CoVaR4UR,
          CoVaR5DL,CoVaR5DR,CoVaR5UL,CoVaR5UR,
          CoVaR6DL,CoVaR6DR,CoVaR6UL,CoVaR6UR
          )
write.csv(U,"3VaR and CoVaR Result.csv",row.names = F)
