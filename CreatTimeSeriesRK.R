#rm(list = ls(all = TRUE))

library(RSQLite)

#############################################################################################################################################
### CRIANDO O BANCO DE DADOS
m <- dbDriver("SQLite")
tfile <- "~/RK.db"
con <- dbConnect(m, dbname = tfile)
###
###############################################################################################################################################

###############################################################################################################################################
### CRIANDO AS TABELAS

qrytbCountry <- "CREATE TABLE tbCountry (PK_IsoCode CHAR(2) NOT NULL PRIMARY KEY, Name CHAR(50) NOT NULL, IsoCode3 CHAR(3) NOT NULL, 
Number INTERGER NOT NULL)"

qrytbBorse <- "CREATE TABLE tbBorse (PK_ID CHAR(6) NOT NULL PRIMARY KEY, ID_MIC_PRIM_EXCH CHAR(6), EXCH_CODE CHAR(6),
FK_tbCountry_PK_IsoCode CHAR(2) NOT NULL REFERENCES tbCountry( PK_IsoCode))"

qrytbCurrency <- "CREATE TABLE tbCurrency (PK_IsoCode CHAR(3) NOT NULL PRIMARY KEY, Name CHAR(50) NOT NULL,
Number INTERGER NOT NULL)"

qrytbJoinCurrencytbCountry <- "CREATE TABLE tbJoinCurrencytbCountry (FK_tbCurrency_PK_IsoCode CHAR(3) NOT NULL REFERENCES tbCurrency( PK_IsoCode),
FK_tbCountry_PK_IsoCode CHAR(2) NOT NULL REFERENCES tbCountry( PK_IsoCode),PRIMARY KEY (FK_tbCurrency_PK_IsoCode, FK_tbCountry_PK_IsoCode))"

qrytbCalendar <- "CREATE TABLE tbCalendar (PK_ID CHAR(10) NOT NULL PRIMARY KEY, Name CHAR(50) NOT NULL)"

qrytbJoinCalendartbCountry <- "CREATE TABLE tbJoinCalendartbCountry (
FK_tbCalendar_PK_ID CHAR(10) NOT NULL REFERENCES tbCalendar( PK_ID),
FK_tbCountry_PK_IsoCode CHAR(2) NOT NULL REFERENCES tbCountry(PK_IsoCode), PRIMARY KEY (FK_tbCalendar_PK_ID, FK_tbCountry_PK_IsoCode))"

qrytbQuoteUnits <- "CREATE TABLE tbQuoteUnits (
PK_ID CHAR(8) NOT NULL PRIMARY KEY, Name CHAR(50) NOT NULL)"

qrytbDayCount <- "CREATE TABLE tbDayCount (
PK_ID CHAR(8) NOT NULL PRIMARY KEY, Name CHAR(50) NOT NULL)"

qrytbFrequency <- "CREATE TABLE tbFrequency (
PK_ID CHAR(3) NOT NULL PRIMARY KEY, Name CHAR(50) NOT NULL, Nick CHAR(6) NOT NULL)"

qrytbBusinessDayConvention <- "CREATE TABLE tbBusinessDayConvention (
PK_ID INTEGER NOT NULL PRIMARY KEY, Name CHAR(50) NOT NULL, Nick CHAR(6) NOT NULL)"

qrytbMonth <- "CREATE TABLE tbMonth (
PK_ID INTEGER NOT NULL PRIMARY KEY, Nick CHAR(3) NOT NULL, Month CHAR(12) NOT NULL)"

qrytbIndexType <- "CREATE TABLE tbIndexType (
PK_ID CHAR(12) NOT NULL PRIMARY KEY,
Name CHAR(50) NOT NULL)"

qrytbIndex <- "CREATE TABLE tbIndex (
PK_ID CHAR(12) NOT NULL PRIMARY KEY,
Name CHAR(50) NOT NULL,
FK_tbCurrency_PK_IsoCode CHAR(3) REFERENCES tbCurrency(PK_IsoCode),
FK_tbCountry_PK_IsoCode CHAR(2)  REFERENCES tbCountry(PK_IsoCode),
FK_tbQuoteUnits_PK_ID CHAR(8) NOT NULL REFERENCES QuoteUnits(PK_ID),
FK_tbDayCount_PK_ID CHAR(8) REFERENCES tbDayCount(PK_ID),
FK_tbCalendar_PK_ID CHAR(10) REFERENCES tbCalendar(PK_ID),
FK_tbFrequency_PK_ID CHAR(3) REFERENCES tbFrequency(PK_ID),
Bloomberg_Ticker CHAR(30),
FK_tbIndexType_PK_ID CHAR(12) NOT NULL REFERENCES tbIndexType(PK_ID),
Desc VARCHAR(200))"

qrytbSecurityClassInExchange <- "CREATE TABLE tbSecurityClassInExchange(
PK_ID CHAR(12) NOT NULL PRIMARY KEY, Desc CHAR(50) NOT NULL,
FK_tbBorse_PK_ID CHAR(6) NOT NULL REFERENCES tbBorse_PK_ID(PK_ID))"

qrytbContracts <- "CREATE TABLE tbContracts (
PK_ID CHAR(30) NOT NULL PRIMARY KEY,
Bloomberg_Ticker CHAR(30),
Name CHAR(50) NOT NULL,
FK_tbCountry_PK_IsoCode CHAR(2) NOT NULL REFERENCES tbCountry(PK_IsoCode),
FUT_FIRST_TRADE_DT DATE NOT NULL,
LAST_TRADEABLE_DT DATE NOT NULL,
FUTURES_VALUATION_DATE DATE NOT NULL,
FK_tbBorse_PK_ID CHAR(6) NOT NULL REFERENCES tbBorse(PK_ID),
ID_BB_UNIQUE CHAR(15),
ID_ISIN CHAR(12),
Borse_Ticker CHAR(20) NOT NULL,
FUT_CONT_SIZE INTEGER NOT NULL,
FK_tbDayCount_PK_ID CHAR(8) REFERENCES tbDayCount(PK_ID),
FK_tbCurrency_PK_IsoCode CHAR(3) REFERENCES tbCurrency(PK_IsoCode),
FK_tbQuoteUnits_PK_ID CHAR(8) REFERENCES QuoteUnits(PK_ID),
FK_tbCalendar_PK_ID CHAR(10) REFERENCES tbCalendar(PK_ID),
Maturuty DATE NOT NULL,
FK_tbIndex_PK_ID CHAR(12) NOT NULL REFERENCES tbIndex(PK_ID),
FK_tbBusinessDayConvention_PK_ID INTERGER NOT NULL REFERENCES tbBusinessDayConvention(PK_ID),
FK_tbSecurityTypeInExchange_PK_ID CHAR(12) NOT NULL REFERENCES tbSecurityTypeInExchange(PK_ID), 
FK_tbMonth_PK_ID INTEGER NOT NULL REFERENCES tbMonth(PK_ID),
Year INTEGER NOT NULL,
Desc VARCHAR(200), 
UNIQUE (FK_tbBorse_PK_ID, FK_tbSecurityTypeInExchange_PK_ID, FK_tbMonth_PK_ID, Year))"

qrytbContractQuotes <- "CREATE TABLE tbContractQuotes(
  Dt DATE NOT NULL,
	FK_tbContracts_PK_ID CHAR(30) NOT NULL REFERENCES tbContracts(PK_ID),
	Settle DOUBLE,
	Last DOUBLE,
	Open DOUBLE,
	High DOUBLE,
	Low DOUBLE,
	Volume INTEGER,
	Open_Interest INTEGER,
	FUT_BUS_DAYS_VAL INTEGER,
  PRIMARY KEY (DT, FK_tbContracts_PK_ID))"

qrytbIndexQuotes <- "CREATE TABLE tbIndexQuotes (
FK_tbIndex_PK_ID CHAR(12) NOT NULL REFERENCES tbIndex(PK_ID),
Dt DATE NOT NULL,
Value DOUBLE,
PRIMARY KEY (FK_tbIndex_PK_ID, Dt))"

qrys <- c(qrytbCountry, qrytbBorse, qrytbCurrency, qrytbJoinCurrencytbCountry,
          qrytbCalendar, qrytbJoinCalendartbCountry, qrytbQuoteUnits, qrytbDayCount,
          qrytbFrequency, qrytbBusinessDayConvention,  qrytbIndex, qrytbSecurityClassInExchange,
          qrytbContracts, qrytbContractQuotes, qrytbIndexQuotes)

for(k in seq_along(qrys)){
res <- dbSendQuery(con, qrys[k])
}

### clean up
dbDisconnect(con)
file.info(tfile)
#file.remove(tfile)
##


