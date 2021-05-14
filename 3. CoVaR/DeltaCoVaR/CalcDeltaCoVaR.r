setwd("C:\\Users\\Mordor Gong\\Desktop\\Models\\3. CoVaR\\DeltaCoVaR")
da = read.csv('1VaR and CoVaR Result.csv',header = T, row.names = 1)
da

######Cal. DeltaCoVaR and PercentageCoVaR
###DownSide
# 1-2 L
DCoVaR1DL = da[,9]-da[,2]
PCoVaR1DL = DCoVaR1DL/da[,2]
# 1-3 L
DCoVaR2DL = da[,13]-da[,3]
PCoVaR2DL = DCoVaR2DL/da[,3]
# 1-4 L
DCoVaR3DL = da[,17]-da[,4]
PCoVaR3DL = DCoVaR3DL/da[,4]
# 2-3 L
DCoVaR4DL = da[,21]-da[,3]
PCoVaR4DL = DCoVaR4DL/da[,3]
# 2-4 L
DCoVaR5DL = da[,25]-da[,4]
PCoVaR5DL = DCoVaR5DL/da[,4]
# 3-4 L
DCoVaR6DL = da[,29]-da[,4]
PCoVaR6DL = DCoVaR6DL/da[,4]

# 1-2 R
DCoVaR1DR = da[,10]-da[1]
PCoVaR1DR = DCoVaR1DR/da[,1]
# 1-3 R
DCoVaR2DR = da[,14]-da[,1]
PCoVaR2DR = DCoVaR2DR/da[,1]
# 1-4 R
DCoVaR3DR = da[,18]-da[,1]
PCoVaR3DR = DCoVaR3DR/da[,1]
# 2-3 R
DCoVaR4DR = da[,22]-da[,2]
PCoVaR4DR = DCoVaR4DR/da[,2]
# 2-4 R
DCoVaR5DR = da[,26]-da[,2]
PCoVaR5DR = DCoVaR5DR/da[,2]
# 3-4 R
DCoVaR6DR = da[,30]-da[,3]
PCoVaR6DR = DCoVaR6DR/da[,3]

###UpSide
# 1-2 L
DCoVaR1UL = da[,11]-da[,6]
PCoVaR1UL = DCoVaR1UL/da[,6]
# 1-3 L
DCoVaR2UL = da[,15]-da[,7]
PCoVaR2UL = DCoVaR2UL/da[,7]
# 1-4 L
DCoVaR3UL = da[,19]-da[,8]
PCoVaR3UL = DCoVaR3UL/da[,8]
# 2-3 L
DCoVaR4UL = da[,23]-da[,7]
PCoVaR4UL = DCoVaR4UL/da[,7]
# 2-4 L
DCoVaR5UL = da[,27]-da[,8]
PCoVaR5UL = DCoVaR5UL/da[,8]
# 3-4 L
DCoVaR6UL = da[,31]-da[,8]
PCoVaR6UL = DCoVaR6UL/da[,8]

# 1-2 R
DCoVaR1UR = da[,12]-da[,6]
PCoVaR1UR = DCoVaR1UR/da[,6]
# 1-3 R
DCoVaR2UR = da[,16]-da[,7]
PCoVaR2UR = DCoVaR2UR/da[,7]
# 1-4 R
DCoVaR3UR = da[,20]-da[,8]
PCoVaR3UR = DCoVaR3UR/da[,8]
# 2-3 R
DCoVaR4UR = da[,24]-da[,7]
PCoVaR4UR = DCoVaR4UR/da[,7]
# 2-4 R
DCoVaR5UR = da[,28]-da[,8]
PCoVaR5UR = DCoVaR5UR/da[,8]
# 3-4 R
DCoVaR6UR = da[,32]-da[,8]
PCoVaR6UR = DCoVaR6UR/da[,8]

########### Data Output
da = read.csv('1VaR and CoVaR Result.csv',header = T)
U = cbind(da[,1],
          
DCoVaR1DL,
DCoVaR2DL,
DCoVaR3DL,
DCoVaR4DL,
DCoVaR5DL,
DCoVaR6DL,
DCoVaR1DR
,DCoVaR2DR
,DCoVaR3DR
,DCoVaR4DR
,DCoVaR5DR
,DCoVaR6DR
,DCoVaR1UL
,DCoVaR2UL
,DCoVaR3UL
,DCoVaR4UL
,DCoVaR5UL
,DCoVaR6UL,
DCoVaR1UR
,DCoVaR2UR
,DCoVaR3UR
,DCoVaR4UR
,DCoVaR5UR
,DCoVaR6UR
          )
write.csv(U,"1DeltaCoVaR Result.csv",row.names = F)

########### Data Output
da = read.csv('1VaR and CoVaR Result.csv',header = T)
U = cbind(da[,1],
          PCoVaR1DL,PCoVaR2DL,PCoVaR3DL,PCoVaR4DL,PCoVaR5DL,PCoVaR6DL,
          PCoVaR1DR
,PCoVaR2DR
,PCoVaR3DR
,PCoVaR4DR
,PCoVaR5DR
,PCoVaR6DR,
PCoVaR1UL
,PCoVaR2UL
,PCoVaR3UL
,PCoVaR4UL
,PCoVaR5UL
,PCoVaR6UL,
PCoVaR1UR
,PCoVaR2UR
,PCoVaR3UR
,PCoVaR4UR
,PCoVaR5UR
,PCoVaR6UR
          )
write.csv(U,"1PerceCoVaR Result.csv",row.names = F)
