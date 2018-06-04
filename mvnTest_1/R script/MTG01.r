print("############# MTG01 ############################")
data_pd <- read.csv("data_pd.csv", header=TRUE)
data_lgd <- read.csv("data_lgd.csv", header=TRUE)
# ------ LGD -------------
print("checkpoint 01")
coeff_lgd <- c(-0.027585, -0.060976, 0.014468, 0.078894)
intercept_lgd <- c(-0.23391)

#LGDScore_t <- t(t(data_lgd[ -1, -c(1:3)]) * coeff_lgd)
LGDScore_t <- as.matrix(data_lgd[ , -c(1:3)] )%*% as.matrix(coeff_lgd)
print(LGDScore_t)

LGDScore <- LGDScore_t + intercept_lgd
LGD <- 1/(1 + exp(-LGDScore))
LGD <- cbind(data_lgd[ ,c(1:3)], LGD)

print("----------------------------  LGD result  -------------------------------")
print(head(LGD))

# PD -----------------------
coeff_Current <- read.csv("Coeff_Current.csv", header = TRUE, row.names = 1)
coeff_DQ30 <- read.csv("Coeff_DQ30.csv", header = TRUE, row.names = 1)
coeff_DQ60 <- read.csv("Coeff_DQ60.csv", header = TRUE, row.names = 1)
## ETA ====================
### ETA Current ####
ETA_C_30 <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_Current["ToDQ30", ]))
ETA_C_60 <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_Current["ToDQ60", ]))
ETA_C_DEF <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_Current["ToDefault", ]))
ETA_C_PO <-  as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_Current["ToPO", ]))

### ETA DQ30 ####
ETA_30_C <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_DQ30["ToCurrent", ]))
ETA_30_60 <-as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_DQ30["ToDQ60", ]))
ETA_30_DEF <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_DQ30["ToDefault", ]))
ETA_30_PO <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_DQ30["ToPO", ]))

### ETA DQ60 ####
ETA_60_C <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_DQ60["ToCurrent", ]))
ETA_60_30 <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_DQ60["ToDQ30", ]))
ETA_60_DEF <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_DQ60["ToDefault", ]))
ETA_60_PO <- as.matrix(data_pd[,-c(1:3)]) %*% as.matrix(as.numeric(coeff_DQ60["ToPO", ]))

## Combine ETA result ====
ETA <- data.frame(ETA_C_30, ETA_C_60, ETA_C_DEF, ETA_C_PO,
                  ETA_30_C, ETA_30_60, ETA_30_DEF, ETA_30_PO,
                  ETA_60_C, ETA_60_30, ETA_60_DEF, ETA_60_PO)
ETA <- cbind(data_pd[ ,c(1,2)], ETA)

print("-------------------------------  ETA result  -------------------------------")
print(head(ETA))
## PD ====================
SumExpETA_Current <- exp(ETA_C_30) + exp(ETA_C_60) + exp(ETA_C_DEF) + exp(ETA_C_PO)
PD_C_30 <- exp(ETA_C_30) / (1 + SumExpETA_Current)
PD_C_60 <- exp(ETA_C_60) / (1 + SumExpETA_Current)
PD_C_DEF <- exp(ETA_C_DEF) / (1 + SumExpETA_Current)
PD_C_PO <- exp(ETA_C_PO) / (1 + SumExpETA_Current)
PD_C_C <- 1 - PD_C_30 - PD_C_60 - PD_C_DEF - PD_C_PO

SumExpETA_DQ30 <- exp(ETA_30_C) + exp(ETA_30_60) + exp(ETA_30_DEF) + exp(ETA_30_PO)
PD_30_C <- exp(ETA_30_C) / (1 + SumExpETA_DQ30)
PD_30_60 <- exp(ETA_30_60) / (1 + SumExpETA_DQ30)
PD_30_DEF <- exp(ETA_30_DEF) / (1 + SumExpETA_DQ30)
PD_30_PO <- exp(ETA_30_PO) / (1 + SumExpETA_DQ30)
PD_30_30 <- 1 - PD_30_C - PD_30_60 - PD_30_DEF - PD_30_PO

SumExpETA_DQ60 <- exp(ETA_60_C) + exp(ETA_60_30) + exp(ETA_60_DEF) + exp(ETA_60_PO)
PD_60_C <- exp(ETA_60_C) / (1 + SumExpETA_DQ60)
PD_60_30 <- exp(ETA_60_30) / (1 + SumExpETA_DQ60)
PD_60_DEF <- exp(ETA_60_DEF) / (1 + SumExpETA_DQ60)
PD_60_PO <- exp(ETA_60_PO) / (1 + SumExpETA_DQ60)
PD_60_60 <- 1 - PD_60_C - PD_60_30 - PD_60_DEF - PD_60_PO

PD <- data.frame(PD_C_C, PD_C_30, PD_C_60, PD_C_DEF, PD_C_PO,
                 PD_30_C, PD_30_30, PD_30_60, PD_30_DEF, PD_30_PO,
                 PD_60_C, PD_60_30, PD_60_60, PD_60_DEF, PD_60_PO)
PD <- cbind( data_pd[ ,c(1,2)], PD)

print("------------------------------- PD result  -------------------------------")
print(head(PD))
PD[is.na(PD)] <- 0
print("------------------------------- PD columnsume result -------------------------------")

print(colSums(PD[,-c(1:2)]))
