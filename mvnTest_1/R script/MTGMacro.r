print("############# MTGMacro ############################")
#library('org.renjin.cran:data.table')
## Macro data transform Functions

### lag
lagpad <- function(x, k) {
  if (!is.vector(x))
    stop('x must be a vector')
  if (!is.numeric(x))
    stop('x must be numeric')
  if (!is.numeric(k))
    stop('k must be numeric')
  if (1 != length(k))
    stop('k must be a single number')
  c(rep(NA, k), x)[1 : length(x)]
}

# growth
gr <- function(x){
  x <- as.numeric(as.character(x))
  ((x/lagpad(x,1))^4-1) *100
}
# quarter diff
Diff <- function(x){
  x <- as.numeric(as.character(x))
  x-(lagpad(x,1))
}
# Annual Diff
AnnDiff <- function(x){
  x-(lagpad(x,4))
}

# Change
# k=1, default, quarter change
# k=4, annual change
Chg <- function(x,k=1){
  x <- as.numeric(as.character(x))
  (x/lagpad(x,k)-1)
}

## ---------------------- ##
## ---- read dataset ---- ##
## ---------------------- ##
raw <- read.csv("C:/Users/hsong/eclipse-workspace/mvnTest_1/Macro.csv", header = TRUE)
raw$Mnemonic <- as.Date(raw$Mnemonic,format="%m/%d/%Y")
ind <- sapply(raw[], is.factor)
raw[ind] <- lapply(raw[ind], function(x) as.numeric(as.character(x)))
# remove tag column and type row.
raw <- raw[-1,-2]

## Date variable
date_QTR <- raw$Mnemonic
# --------------------------------------
### Mortgage macro variables
# rgdp FGDP_.US
# unemp FLBR_region
# mr FRFHLMCFM.US
# rdi FYPDPI_Q.US
# cpil1e FCPIUL1E.US
# rhpi FHCLHP1TIM_region

print("````````````````````check point")
library(data.table)
library(dplyr)

rgdp <- raw$FGDP_.US
unemp <- raw[,names(raw) %like% "FLBR"]
print("```````````%like% function check `````````" )
print(head(unemp))
unemp <- cbind(Mnemonic=raw$Mnemonic, unemp)

mr <- raw$FRFHLMCFM.US
rdi <- raw$FYPDPI_Q.US
cpil1e <- raw$FCPIUL1E.US

rhpi <- raw[,names(raw) %like% "FHCLHP1TIM"]
rhpi <- cbind(Mnemonic=raw$Mnemonic, rhpi)


## ------------------------------------------ ##
## ----  prepare lgd and pd Macro data ------ ##
## ------------------------------------------ ##
# For LGD model
data_lgd_t <- data.frame(
  Mnemonic = raw$Mnemonic,
  RGDPGR = gr(rgdp)
)
data_lgd_macro <- merge(data_lgd_t, unemp, by="Mnemonic")
##
# For PD model
data_pd_t <- data.frame(
  Mnemonic = raw$Mnemonic,
  MR = mr,
  RDIGR = gr(rdi),
  CPIL1EGR = gr(cpil1e)
)
data_pd_macro <- merge(data_pd_t, unemp, by="Mnemonic")
data_pd_macro <- merge(data_pd_macro, rhpi, by="Mnemonic")
print("```````````data_pd_macro `````````" )
print(head(data_pd_macro))
