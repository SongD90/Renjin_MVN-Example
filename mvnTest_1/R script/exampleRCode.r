print("############# example R Code ############################")
EstDate <- as.Date("08/30/2017", format="%m/%d/%Y")

## ---------------------- ##
## ---- read dataset ---- ##
## ---------------------- ##
currentloan <- read.csv("C:/Users/hsong/eclipse-workspace/mvnTest_1/currentloan.csv", header = TRUE)
customfield <- read.csv("C:/Users/hsong/eclipse-workspace/mvnTest_1/customfielddata.csv", header = TRUE)


print("------------------------------ currentloan and customfield ----------------------------------------")
print(head(currentloan))
print(head(customfield))
## ---------------------- ##
## ---- loan field data ---- ##
## ---------------------- ##

loan <- merge(currentloan, customfield, by="CLIENTINSTRUMENTID")
loan$ORIGINATIONDATE <- as.Date(loan$ORIGINATIONDATE,format="%m/%d/%Y")

print(head(loan))
# region mapping
region <- read.csv("C:/Users/hsong/eclipse-workspace/mvnTest_1/RegionMapping.csv", header = TRUE)

# add RegionName to loan data
loan <- merge(loan, region, by.x = "RF_STATE", by.y = "State", all.x = TRUE)
print(loan[, c("RF_STATE", "RegionName" )])
# add US level, if RegionName==NA, set it to US.
levels(loan$RegionName) <- c(levels(loan$RegionName), "US")
print(loan[, c("RF_STATE", "RegionName" )])
loan$RegionName[is.na(loan$RegionName)] <- "US"

print("----------------- region maping result----------------------------------------------")
print(loan[, c("RF_STATE", "RegionName" )])


print(" ------------------re order loan and output loan order check 2----------------------")
loan <- loan[order(loan$CLIENTINSTRUMENTID), ]
print(head(loan$CLIENTINSTRUMENTID))

print("Check Merge wiht macro ")
## ---------------------------- ##
## ---- merge with macro ---- ##
## ---------------------------- ##

# take loan 8738693 as an example
library(data.table)

print("------------------------------------- loan data check -----------------------------------------")
print(head(loan))

print("------------------------------ loan 8738693 data check ----------------------------------------")
data_loan <- loan[loan$CLIENTINSTRUMENTID==8738693, ]
print(data_loan)

# print("------------------------------ loan(11) 8738693 data check ----------------------------------------")
# data_loan <- loan[11,]
# print(data_loan)



## ----------  lgd data  --------------------

### regional variables

### HPI
HPI_at_ORGDate <- rhpi[(year(rhpi$Mnemonic) == year(data_loan$ORIGINATIONDATE) & quarter(rhpi$Mnemonic) == quarter(data_loan$ORIGINATIONDATE)),
                       names(rhpi)%like%data_loan$RF_STATE]

# HPI_at_ORGDate <- 98.259
print("------------------------------ HPI_at_ORGDate data check ----------------------------------------")
print(HPI_at_ORGDate)



HPI_IND <- rhpi[ , names(rhpi) %like% data_loan$RegionName]/ HPI_at_ORGDate
print("-------------------------------- rhpi names and loan region name ----------------------------------")
print(names(rhpi))
print(data_loan$RegionName)
print(data_loan$CLIENTINSTRUMENTID)
print("-------------------------------- rhpi name -----------------------------------------------------")
print(names(rhpi) %like% data_loan$RegionName)
print("-------------------------------- rhpi -----------------------------------------------------")
print(rhpi[ , names(rhpi) %like% data_loan$RegionName])
print("-------------------------------- HPI_IND -----------------------------------------------------")
print((HPI_IND))

### UNEMP
unemp_loan <- unemp[ , names(unemp) %like% paste("FLBR.",data_loan$RegionName, sep="")]

data_lgd_macro_loan <- data_lgd_macro[ ,
                                       -which(!(names(data_lgd_macro)%like% paste("FLBR.",data_loan$RegionName, sep="")))[3:7]]

print(head(data_lgd_macro_loan))
# HS
# now for each loan it will generate data for all period.
# loanlevel data
data_lgd_t <- data.frame(
  LOANID = data_loan$CLIENTINSTRUMENTID,
  Mnemonic = date_QTR,
  HPI_IND = HPI_IND,
  ORIGLTV = data_loan$LTV_MODEL
)
# combine with macro data
data_lgd <- merge(data_lgd_t, data_lgd_macro_loan, by="Mnemonic")

data_lgd <- data_lgd[ ,c(1:3, 5, 4, 6)]


## ----------  pd data  --------------------

### regional variables
rhpi_loan <- rhpi[ , names(rhpi) %like% data_loan$RegionName]

data_pd_macro_loan <- data_pd_macro[,1:4]


# loanlevel data
library(zoo)
AGE <- (as.yearmon(EstDate) - as.yearmon(data_loan$ORIGINATIONDATE)) * 12

BV_MV_Ratio <- data_loan$AMORTIZEDCOST/data_loan$HOUSEVALUE
ELTV <- BV_MV_Ratio / HPI_IND
data_pd_t <- data.frame(
  LOANID = data_loan$CLIENTINSTRUMENTID,
  Mnemonic = date_QTR,
  UNEMP = unemp_loan,
  HPI = rhpi_loan,
  AGE = AGE,
  AGE2 = AGE^2,
  ORIGSC = data_loan$FICO08_ORIGINAL,
  ELTV = ELTV,
  INSURANCE = data_loan$INSURANCE,
  NUM30 = data_loan$TIMES_30_DAYS_PAST_DUE,
  JUMBO = data_loan$JUMBO_INDICATOR,
  NUM60 = data_loan$TIMES_60_DAYS_PAST_DUE
)

data_pd <- merge(data_pd_t, data_pd_macro_loan, by="Mnemonic")

# add intercept column
data_pd$Intercept <- 1
# reorder columns
data_pd <- data_pd[ , c( "Mnemonic", "LOANID", "Intercept", "MR", "RDIGR", "CPIL1EGR", "AGE", "AGE2",
                         "ORIGSC", "ELTV", "INSURANCE", "NUM30", "UNEMP", "JUMBO", "NUM60", "HPI")]
